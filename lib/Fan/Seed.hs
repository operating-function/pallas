-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Fan.Seed
    ( loadSeed, saveSeed -- seed = everything in one go
    , saveGermPin, loadGerm
    , loadPod, savePod   -- save each pin, then save table of seeds
    , savePin, savePin'  -- save one pin with header
    , LoadErr(..)
    , splitBlob
    , loadHead
    , loadBody
    , Seed.withContext
    , Seed.Ctx
    )
where


import Data.Bits
import Data.Sorted
import Fan.Convert
import Fan.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import PlunderPrelude

import Control.Monad.Primitive          (touch)
import Control.Monad.Trans.Except       (runExcept, throwE)
import Control.Monad.Trans.State.Strict (State(..), StateT(..), evalState,
                                         evalStateT, execState, execStateT, get,
                                         modify', put, runState)

import Fan.Eval        (boom, evalArity, mkLawPreNormalized, mkPin', mkRow,
                        tabValsRow, (%%))
import Fan.Trace       (doTrk)
import Foreign.C.Types (CBool(..))
import GHC.Word        (Word(..))
import Hash256         (Hash256, hashToByteString, toHash256)
import Loot.Backend    (loadClosure, loadShallow)
import Loot.ReplExe    (closureRex, dieFan, showFan, trkFan)
import Rex             (GRex(..), RuneShape(..), TextShape(..), rexLine)
import Rex.Print       (RexColor, RexColorScheme(NoColors))

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as VM
import qualified Fan.Prof                 as Prof
import qualified Fan.Seed.FFI             as Seed
import qualified Fan.Seed.FragLoader      as JFL
import qualified GHC.Exts                 as GHC


--------------------------------------------------------------------------------

savePin' :: Seed.Ctx -> Pin -> IO (Vector Pin, ByteString, ByteString)
savePin' ctx p = do
    body <- saveGermPin' ctx p
    head <- saveHead p
    pure (p.refs, head, body)

savePin :: Pin -> IO (Vector Pin, ByteString, ByteString)
savePin pin = Seed.withContext \ctx -> savePin' ctx pin

saveHead :: Pin -> IO ByteString
saveHead pin = do
    ptr <- mallocForeignPtrBytes size
    withForeignPtr ptr \buf -> do
        poke buf (fromIntegral numPins :: Word64)
        for_ (zip [0..] (toList pin.refs)) \(i, dep) -> do
            let dst = buf `plusPtr` (8 + (i*32))
            poke (castPtr dst :: Ptr Hash256) dep.hash
    pure (BS.BS (castForeignPtr ptr) size)
  where
    numPins = length pin.refs

    size :: Int
    size = 8 + (32*numPins)

loadBody :: Vector Pin -> ByteString -> Either LoadErr Fan
loadBody refs bs = unsafePerformIO (loadGerm refs bs)

loadHead :: ByteString -> Either LoadErr (Vector Hash256)
loadHead bs@(BS.BS fp siz) = do
    let siz = length bs

    when (siz < 8) do
        Left $ HEAD_TOO_SMALL_NO_LENGTH

    -- TODO: Handle words that don't fit in ints
    let !numPins = unsafePerformIO do
            res <- withForeignPtr (castForeignPtr fp) peek
            pure (fromIntegral (res :: Word64))

    let required = (8 + numPins * 32) :: Int

    when (fromIntegral siz < required) do
        Left $ HEAD_TOO_SMALL (fromIntegral siz) (fromIntegral required)

    let f remain = (toHash256 (take 32 remain), drop 32 remain)

    pure $ V.unfoldrExactN numPins f $ drop 8 bs

splitBlob :: ByteString -> Either LoadErr (ByteString, ByteString)
splitBlob bs@(BS.BS fp siz) = do
    when (siz < 8) do
        Left HEAD_TOO_SMALL_NO_LENGTH

    -- TODO: Handle words that don't fit in ints
    let !numPins = unsafePerformIO do
            res <- withForeignPtr (castForeignPtr fp) peek
            pure (fromIntegral (res :: Word64))

    let required = (8 + fromIntegral numPins * 32) :: Int

    when (siz < required) do
        Left $ HEAD_TOO_SMALL (fromIntegral siz) (fromIntegral required)

    pure (take required bs, drop required bs)


--------------------------------------------------------------------------------

data LoadErr
    = EOF Text
    | EMPTY_INPUT
    | INPUT_NOT_WORD64_PADDED
    | LEAF_HAS_IMPOSSIBLE_SIZE
    | NAT_HAS_TRAILING_ZEROS
    | GERM_BAD_HOLE_COUNT { passed :: Nat, required :: Nat }
    | POD_INTEGRITY_CHECK_FAILED Hash256 Pin
    | POD_MALFORMED Fan
    | POD_NO_MAGIC
    | POD_MISSING_HASH
    | POD_NO_ROUND
    | POD_NO_PINS
    | HEAD_TOO_SMALL_NO_LENGTH
    | HEAD_TOO_SMALL { bufferSize :: Nat, requiredSize :: Nat }
  deriving (Eq, Ord, Show, Generic, Exception)

instance ToNoun LoadErr where
    toNoun = \case
        EOF loc                        -> toNoun ("EOF"::Text, loc)
        EMPTY_INPUT                    -> toNoun ("EMPTY_INPUT"::Text)
        INPUT_NOT_WORD64_PADDED        -> toNoun ("INPUT_NOT_WORD64_PADDED"::Text)
        LEAF_HAS_IMPOSSIBLE_SIZE       -> toNoun ("LEAF_HAS_IMPOSSIBLE_SIZE"::Text)
        NAT_HAS_TRAILING_ZEROS         -> toNoun ("NAT_HAS_TRAILING_ZEROS"::Text)
        GERM_BAD_HOLE_COUNT p r        -> toNoun ("GERM_BAD_HOLE_COUNT"::Text, p, r)
        POD_INTEGRITY_CHECK_FAILED h p -> toNoun ("POD_INTEGRITY_CHECK_FAILED"::Text, h, p)
        POD_MALFORMED f                -> toNoun ("POD_MALFORMED"::Text, f)
        POD_NO_MAGIC                   -> toNoun ("POD_NO_MAGIC"::Text)
        POD_MISSING_HASH               -> toNoun ("POD_MISSING_HASH"::Text)
        POD_NO_ROUND                   -> toNoun ("POD_NO_ROUND"::Text)
        HEAD_TOO_SMALL_NO_LENGTH       -> toNoun ("HEAD_TOO_SMALL_NO_LENGTH"::Text)
        HEAD_TOO_SMALL s r             -> toNoun ("HEAD_TOO_SMALL"::Text, s, r)

instance (ToNoun a, ToNoun b) => ToNoun (Either a b) where
    toNoun (Left x)  = ROW $ arrayFromListN 2 [0, toNoun x]
    toNoun (Right x) = ROW $ arrayFromListN 2 [1, toNoun x]

trkM :: Monad m => Fan -> m ()
trkM msg = do
    let !() = doTrk msg ()
    pure ()

loadSeed :: ByteString -> IO (Either LoadErr Fan)
loadSeed = loadGerm mempty

loadGerm :: Vector Pin -> ByteString -> IO (Either LoadErr Fan)
loadGerm holes germBar@(BS.BS fp bufByteSz) =
    Prof.withSimpleTracingEvent "loadGerm" "load" $ try do
        -- trkM $ REX $ planRexFull $ toNoun ("LOAD"::Text, (.hash) <$> holes)
        withForeignPtr fp \byteBuf -> do
            evalStateT (go byteBuf) 0
  where
    holesFan = PIN <$> holes

    (bufWordSz, overflow) = bufByteSz `quotRem` 8

    needWords :: Text -> Int -> StateT Int IO ()
    needWords location need = do
        used <- get
        when ((used + need) > bufWordSz) do
            throwIO (EOF location)

    go :: Ptr Word8 -> StateT Int IO Fan
    go byteBuf = do
        when (overflow /= 0) do throwIO INPUT_NOT_WORD64_PADDED

        let wordBuf :: Ptr Word = castPtr byteBuf

        needWords "header" 5

        numHolesW :: Word <- liftIO $ peekElemOff wordBuf 0
        numBigsW  :: Word <- liftIO $ peekElemOff wordBuf 1
        numWordsW :: Word <- liftIO $ peekElemOff wordBuf 2
        numBytesW :: Word <- liftIO $ peekElemOff wordBuf 3
        numFragsW :: Word <- liftIO $ peekElemOff wordBuf 4

        put 5

        let numHoles = fromIntegral numHolesW :: Int
        let numBigs  = fromIntegral numBigsW  :: Int
        let numWords = fromIntegral numWordsW :: Int
        let numBytes = fromIntegral numBytesW :: Int
        let numFrags = fromIntegral numFragsW :: Int

        let numAtoms  = numBytes + numWords + numBigs
        let numLeaves = numHoles + numAtoms
        let tableSize = numLeaves + numFrags

        when (numHoles /= length holes) do
            throwIO GERM_BAD_HOLE_COUNT { passed = fromIntegral (length holes)
                                        , required = fromIntegral (numHoles)
                                        }

        when (tableSize == 0) do throwIO EMPTY_INPUT

        table :: VM.IOVector Fan <- VM.unsafeNew (fromIntegral tableSize)

        for (take numHoles [0..]) \i -> do
            VM.unsafeWrite table i (holesFan V.! i)

        -- BigNat widths
        needWords "bignat widths" numBigs
        bigWidths <- liftIO $ V.generateM (fromIntegral numBigs) \i ->
                                  peekElemOff wordBuf (5+i)
        modify' (+ numBigs)

        -- BigNats
        needWords "bignat data" (fromIntegral $ sum bigWidths)
        for_ (take numBigs [0..]) \i -> do
            let wid = bigWidths V.! i
            off <- get
            put $! (off + fromIntegral wid)
            let pntr = castForeignPtr (fp `plusForeignPtr` (off * 8))
            let valu = NAT $ NatJ# (EXO wid pntr)
            VM.unsafeWrite table (numHoles + i) valu

        -- Words
        needWords "words" numWords
        wordSection <- get
        for_ (take numWords [0..]) \i -> do
            !(W# w) <- liftIO (peekElemOff wordBuf (wordSection + i))
            VM.unsafeWrite table (numHoles + numBigs + i) (NAT $ NatS# w)
        modify' (+ numWords)

        -- Bytes
        let (byteWords, byteExtra) = numBytes `quotRem` 8

        needWords "bytes" (byteWords + if byteExtra == 0 then 0 else 1)

        bytesSection <- get <&> \off -> (byteBuf `plusPtr` (off * 8))
        for_ (take numBytes [0..]) \i -> do
            byt :: Word8 <- liftIO (peekByteOff bytesSection i)
            let !(W# w) = fromIntegral byt
            VM.unsafeWrite table (numHoles + numBigs + numWords + i) (NAT (NatS# w))

        modify' (+ byteWords)

        -- Fragments
        do
            off <- get
            let usedBits = byteExtra * 8
            let fragPtr  = wordBuf `plusPtr` (8 * off)
            let endPtr   = wordBuf `plusPtr` bufByteSz
            finalPtr <- liftIO $ JFL.loadFrags2 True
                                                table
                                                (numLeaves, numFrags)
                                                usedBits
                                                (fragPtr, endPtr)

            -- TODO: This fires even though things are working?  What gives?
            when ((finalPtr > endPtr) && False) do
                throwIO (EOF "fragments")

        -- Return final value
        VM.read table (tableSize - 1)


--------------------------------------------------------------------------------

{-# INLINE saveSeed #-}
saveSeed :: Fan -> IO ByteString
saveSeed top = Seed.withContext \ctx -> saveSeed' ctx top

{-# INLINE saveSeed' #-}
saveSeed' :: Seed.Ctx -> Fan -> IO ByteString
saveSeed' ctx top = do
    vPins <- newIORef mempty
    vZoo  <- newIORef Nothing
    saveWorker ctx vZoo vPins top

{-
    This is just broken off into a separate function for syntactic reasons
    (to make avoid needing to move the whole `where`  block into let
    clauses within the top-level `do` block)
-}
{-# INLINE saveWorker #-}
saveWorker
    :: Seed.Ctx
    -> IORef (Maybe Seed.CNode)
    -> IORef (Map Hash256 Seed.CNode)
    -> Fan
    -> IO ByteString
saveWorker !ctx !vZoo !vPins !top = do

    _   <- Prof.withSimpleTracingEvent "walk" "save" do
               loop top

    ()  <- Prof.withSimpleTracingEvent "done" "save" do
               Seed.c_done ctx

    wid <- Prof.withSimpleTracingEvent "size" "save" do
               Seed.c_size ctx

    ptr <- mallocForeignPtrBytes (fromIntegral wid)

    Prof.withSimpleTracingEvent "write" "save" do
        withForeignPtr ptr \buf -> do
            void (fillBytes buf 0 $ fromIntegral wid)
            written <- Seed.c_save ctx wid buf
            unless (wid == written) do
                error $ unlines $ concat
                    [ [ "INTERNAL ERROR IN save_seed()"
                      , ""
                      , "When serializing a fan value (using seed), the number"
                      , "of bytes written did not match the pre-computed buffer"
                      , "size.  This is is an internal invariant violation and"
                      , "is fatal, please submit a bug report!"
                      , ""
                      , "Here is the plan value that we were trying to"
                      , "serialize:"
                      , ""
                      ]
                    , fmap ("\t" <>) $ lines $ unpack $ showFan top
                    , [ "pre-calculated size: " <> show wid
                      , ""
                      , "written size: " <> show written
                      ]
                    ]

    Prof.withSimpleTracingEvent "wipe" "save" do
        Seed.c_wipe ctx

    -- Need to make sure no bars or atoms are collected while the C code
    -- still has reference to them.
    touch top

    pure (BS.BS ptr $ fromIntegral wid)
  where
    -- COW 3 = (0 0 4 0)
    doCow :: Nat -> IO Seed.CNode
    doCow n = do
        z    <- Seed.c_word ctx 0
        zz   <- Seed.c_cons ctx z z
        r    <- doNat (n+1)
        zzr  <- Seed.c_cons ctx zz r
        zzrz <- Seed.c_cons ctx zzr z
        pure zzrz

    -- Keys must be given in descending order.
    --
    -- Example Shape:
    --
    --     %[y x] = (0 1 2 (0 0 3 0 y x))
    --
    -- Example Insertion Order:
    --
    --     0
    --     (0 0)
    --     3
    --     (0 0 3)
    --     (0 0 3 0)
    --     y
    --     (0 0 3 0 y)
    --     x
    --     (0 0 3 0 y x)
    --     ((0 0 3) (0 0 3 0 y x))
    doSet :: Int -> [Fan] -> IO Seed.CNode
    doSet len keyz = do
        let go acc []     = pure acc
            go acc (x:xs) = do key  <- loop x
                               acc' <- Seed.c_cons ctx acc key
                               go acc' xs
        z    <- Seed.c_word ctx 0
        o    <- Seed.c_word ctx 1
        zo   <- Seed.c_cons ctx z o
        t    <- Seed.c_word ctx 2
        zot  <- Seed.c_cons ctx zo t
        zzrz <- doCow (fromIntegral len)
        row  <- go zzrz keyz
        Seed.c_cons ctx zot row

    doNat (NatS# w) = do
        -- print ("WORD"::Text, W# w)
        Seed.c_word ctx (fromIntegral (W# w))

    doNat n@(NatJ# x) = do
        withForeignPtr x.ptr \buf -> do
            Seed.c_nat ctx (fromIntegral x.sz) (castPtr buf)

    loop :: Fan -> IO Seed.CNode
    loop = \case
        NAT n -> do
            doNat n

        PIN pin -> do
            let hax = pin.hash
            (lookup hax <$> readIORef vPins) >>= \case
                Just p -> do
                    Prof.withSimpleTracingEvent "touch" "save" do
                        Seed.c_touch ctx p
                    pure p

                Nothing -> do
                    four <- Seed.c_word ctx 4
                    item <- loop pin.item
                    node <- Seed.c_cons ctx four item
                    modifyIORef vPins (insertMap hax node)
                    pure node

        -- bar b = (0 1 1 (BARNAT b))
        BAR (BS.BS fpt wid) -> do
            withForeignPtr fpt \buf -> do
                zoo <- readIORef vZoo >>= \case
                           Just zoo -> do
                               Seed.c_touch ctx zoo
                               pure zoo
                           Nothing -> do
                               zer <- Seed.c_word ctx 0
                               one <- Seed.c_word ctx 1
                               zo  <- Seed.c_cons ctx zer one
                               zoo <- Seed.c_cons ctx zo one
                               pure zoo
                bod <- Seed.c_barnat ctx (fromIntegral wid) buf
                res <- Seed.c_cons   ctx zoo bod
                pure res

        COw n ->
            doCow n

        SET ks -> do
            let wid  = length ks
            let keyz = ssetToDescList ks
            doSet wid keyz

        -- This needs to have the same behaviors as a head-first traversal
        -- using `boom`.  Rows are represented as
        --
        --     [a b c]=((COW 3) c b a)
        --
        -- So, that examples should be loaded by running:
        --
        --     x = loop (COW 3)
        --     y = loop c
        --     x = (cons x y)
        --     y = loop b
        --     x = (cons x y)
        --     y = loop a
        --     return (cons x y)
        --
        -- So, we basically want to fold over:
        --
        --     (COW n : reverse (toList row))
        --
        -- Except that we want to do that as a traversal so that we
        -- don't need to allocate anything.
        --
        ROW !row -> do
            let go !acc !i =
                    if i<0 then do
                        pure acc
                    else do
                        x <- loop (row!i)
                        y <- Seed.c_cons ctx acc x
                        go y (i-1)

            start <- doCow (fromIntegral $ length row)
            let lastIx = length row - 1
            go start lastIx

        --  #[3=4 5=6] = (%[3 5] [4 6])
        TAb tab -> do
            ks <- doSet (length tab) (fst <$> tabToDescPairsList tab)
            vs <- loop (tabValsRow tab)
            kv <- Seed.c_cons ctx ks vs
            pure kv

        KLO _ env -> do
            let !end = sizeofSmallArray env

            let go !acc !i | i>=end = pure acc
                go !acc !i = do
                    !x <- loop (env .! i)
                    !y <- Seed.c_cons ctx acc x
                    go y (i+1)

            !start <- loop (env .! 0)
            go start 1

        FUN (L (LN nv) av bv _) -> do
            z    <- Seed.c_word ctx 0
            n    <- doNat nv
            zn   <- Seed.c_cons ctx z n
            a    <- doNat av
            zna  <- Seed.c_cons ctx zn a
            b    <- loop bv
            znab <- Seed.c_cons ctx zna b
            pure znab

--------------------------------------------------------------------------------

saveGerm :: Fan -> IO ByteString
saveGerm val = do
    pin <- mkPin' val -- cheap, just to have a uniform interface.
                      -- This is only used for Pin.refs, which allows
                      -- saveGermPin to take advantage of the cache
                      -- instead of recalculating.
    Seed.withContext \ctx -> do
        saveGermPin' ctx pin

saveGermPin :: Pin -> IO ByteString
saveGermPin pin =
    Seed.withContext \ctx -> do
        saveGermPin' ctx pin

saveGermPin' :: Seed.Ctx -> Pin -> IO ByteString
saveGermPin' ctx pin = do

    tab <-
        Prof.withSimpleTracingEvent "setup" "save" do
            -- Create entries for each seed.
            for_ pin.refs \_ -> Seed.c_hole ctx

            -- We can serialize "with holes for each pin" by just using
            -- pre-filling the pin cache with the corresponding hole.
            let tab :: Map Hash256 Seed.CNode
                tab = mapFromList (zip hashes [0..])
                        where hashes = toList ((.hash) <$> pin.refs)

            evaluate tab

    vPins <- newIORef tab
    vZoo  <- newIORef Nothing
    res   <- saveWorker ctx vZoo vPins pin.item

    pure res

{-
    TODO: A better representation might be:

    -   (Vector (ByteString, [Nat])), using the index as a key instead
    -   of the hashes.
-}

type PinStorage = Vector (ByteString, Vector Nat)

data Pod = POD
    { top        :: !Hash256
    , pinStorage :: !PinStorage
    }

instance ToNoun Pod where
    toNoun p = ROW $ arrayFromListN 2 [toNoun p.top, toNoun p.pinStorage]

instance FromNoun Pod where
    fromNoun n = do
        r <- getRawRow n
        guard (length r == 2)
        POD <$> fromNoun (r!0)
            <*> fromNoun (r!1)

magicHeader :: ByteString
magicHeader = "SEEDPOD:"

planRexFull :: Any -> GRex a
planRexFull = fmap absurd . itemizeRexes . closureRex Nothing . loadClosure

savePod :: Pin -> IO ByteString
savePod pin =
    Prof.withSimpleTracingEvent "savePod" "save" do
    liftIO $ Seed.withContext \ctx -> do
        pod     <- collect ctx pin
        payload <- saveSeed' ctx (toNoun pod)
        pure (magicHeader <> payload)

{-
        We should have a version of this that is given a callback which
        loads the blob, either from disk or from local cache.
-}
collect :: Seed.Ctx -> Pin -> IO Pod
collect ctx topPin = do
    Prof.withSimpleTracingEvent "collect" "save" do
        (haz, tab) <- runStateT (collectWorker ctx topPin) mempty
        pure (POD haz $ finish tab)
  where
    finish :: PinStorageAcc -> PinStorage
    finish (hashes, list) =
        V.fromListN (length hashes) (reverse list)

type PinStorageAcc = (Map Hash256 Nat, [(ByteString, Vector Nat)])

collectWorker :: Seed.Ctx -> Pin -> StateT PinStorageAcc IO Hash256
collectWorker ctx pin = do
    haz <- evaluate pin.hash
    t1  <- fst <$> get
    unless (member haz t1) do
        traverse_ (collectWorker ctx) pin.refs
        body <- liftIO (saveGermPin' ctx pin)
        refs <- do
            t2   <- fst <$> get
            for (pin.refs <&> (.hash)) \h -> do
                case lookup h t2 of
                    Nothing -> error "impossible: already inserted"
                    Just ix -> pure ix
        haz <- evaluate pin.hash
        modify' \(tab, acc) -> ( insertMap haz (fromIntegral $ length tab) tab
                               , (body, refs) : acc
                               )
    pure haz

reconstruct :: Pod -> IO (Either LoadErr Pin)
reconstruct pod = try
    if null pod.pinStorage then
        throwIO POD_NO_PINS
    else
        flip evalStateT mempty do
            loop $ fromIntegral $ pred $ length pod.pinStorage
  where
    build :: Nat -> StateT (Map Nat Pin) IO Pin
    build ix = do
        -- TODO: Overflow and out of bounds checking
        let (body, refs) = pod.pinStorage V.! fromIntegral ix

        deps <- traverse loop refs
        fan  <- liftIO $ loadGerm deps body >>= either throwIO pure
        pin  <- liftIO $ mkPin' fan
        modify' (insertMap ix pin)
        pure pin

    loop :: Nat -> StateT (Map Nat Pin) IO Pin
    loop hash = do
        tab <- get
        case lookup hash tab of
            Just f  -> pure f
            Nothing -> build hash

-- | Loads a previously packed bytestring pack to a full Fan value.
loadPod :: ByteString -> IO (Either LoadErr Pin)
loadPod bs = try do
    (pin, pod) <- Prof.withSimpleTracingEvent "loadPod" "load" do
        let (header, payload) = splitAt 8 bs

        when (header /= magicHeader) do
            throwIO POD_NO_MAGIC

        val <- loadSeed payload >>= either throwIO pure

        pod <- case fromNoun val of
                   Nothing  -> throwIO (POD_MALFORMED val)
                   Just pod -> pure pod

        pin <- reconstruct pod >>= either throwIO pure

        pure (pin, pod)

    Prof.withSimpleTracingEvent "validate" "load" do
        when (pod.top /= pin.hash) do
            throwIO (POD_INTEGRITY_CHECK_FAILED pod.top pin)

    pure pin

itemizeRexes :: [GRex a] -> GRex a
itemizeRexes [x] = x
itemizeRexes rs  = go rs
  where
    go []     = N OPEN "*" [] Nothing
    go [x]    = N OPEN "*" [x] Nothing
    go (x:xs) = N OPEN "*" [x] (Just $ go xs)
