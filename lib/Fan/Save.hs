-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Fan.Save
    ( splitBlob
    , loadHead
    , loadBody
    , saveFan
    , saveFan'
    , savePack
    , loadPack
    , getPinHash
    , unsafeGetPinHash
    , subPins
    )
where

import Fan.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import PlunderPrelude        hiding ((^))

import Control.Monad.Primitive (touch)
--import Control.Monad.State              (State, evalState, execState, modify')
import Control.Monad.Trans.Except       (runExcept, throwE)
import Control.Monad.Trans.State.Strict (State(..), StateT(..), evalState,
                                         evalStateT, execState, execStateT, get,
                                         modify', put, runState)
import Data.Bits                        (clearBit, testBit, (.&.))
import Data.Vector                      ((!))
import Fan.Convert
import Fan.Eval                         (boom, evalArity, mkLawPreNormalized,
                                         mkPin', mkRow, (^), (%%), tabValsRow)
import Foreign.C.Types                  (CBool(..))
import Foreign.Ptr                      (Ptr, castPtr)
import Foreign.Storable                 (peek, poke)
import GHC.Word                         (Word(..))
import Jelly.Reference                  (loadDeps, splitBlob)
import Jelly.Types                      (Hash256, hashToByteString, toHash256)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import qualified GHC.Exts                  as GHC
import qualified GHC.Natural               as GHC
import qualified Jelly                     as Jelly
import qualified Jelly.Fast.FFI            as Jelly
import qualified Jelly.Reference           as Jelly

--------------------------------------------------------------------------------

loadHead :: ByteString -> Either Text (Vector Hash256)
loadHead = Jelly.loadDeps

data LoadException
    = EOF
    | INPUT_NOT_WORD64_PADDED
    | LEAF_HAS_IMPOSSIBLE_SIZE
    | NAT_HAS_TRAILING_ZEROS
  deriving (Eq, Ord, Show, Generic, Exception)

loadLeavesTable
    :: Vector Fan
    -> ByteString
    -> IO (Vector Fan, Int, [Bool])
loadLeavesTable pinz bodyBytes = do
    when ((length bodyBytes `mod` 8) /= 0) do
        throwIO INPUT_NOT_WORD64_PADDED

    flip evalStateT bodyBytes do
        numBars <- loadLen
        bars <- replicateM numBars loadBar
        numNats <- loadLen
        nats <- replicateM numNats (NAT <$> loadNat)
        numFrags <- loadLen

        fragBytes <- get
        pure ( pinz <> bars <> nats
             , numFrags
             , bsBits fragBytes
             )
  where
    bsBits :: ByteString -> [Bool]
    bsBits = bytesBits . unpack

    bytesBits []     = []
    bytesBits (w:ws) = fmap (testBit w) [0..7] <> bytesBits ws

loadBar :: StateT ByteString IO Fan
loadBar =
    StateT \top@(BS.BS topPtr topWid) ->
        withForeignPtr topPtr \topBuf -> do
            (bytes, remain) <- loadBytes topPtr topWid topBuf
            pure (BAR bytes, remain)

loadLen :: StateT ByteString IO Int
loadLen = do
    lenNat <- loadNat

    let lenInt = fromIntegral lenNat

    when (fromIntegral lenInt /= lenNat) do
        throwIO LEAF_HAS_IMPOSSIBLE_SIZE

    pure lenInt

{-
    This is essentially just (bytesNat <$> loadBytes) with a fast-path
    for one-byte results and a check that there are no trailing zeros.
-}
loadNat :: StateT ByteString IO Nat
loadNat = do
    StateT \top@(BS.BS topPtr topWid) ->
        withForeignPtr topPtr \topBuf -> do
            when (topWid == 0) do
                throwIO EOF

            fstByt <- peek topBuf

            if fstByt < 128 then do

                let nexPtr = BS.plusForeignPtr topPtr 1

                pure ( fromIntegral fstByt
                     , BS.BS nexPtr (topWid - 1)
                     )

            else do
                (bytes, remain) <- loadBytes topPtr topWid topBuf

                when (0 == BS.last bytes) do
                    throwIO NAT_HAS_TRAILING_ZEROS

                pure (bytesNat bytes, remain)

{-# INLINE loadBytes #-}
loadBytes :: ForeignPtr Word8 -> Int -> Ptr Word8 -> IO (ByteString, ByteString)
loadBytes topPtr topWid topBuf = do
    when (topWid == 0) do
        throwIO EOF

    fstByt <- peek topBuf

    let nexPtr = BS.plusForeignPtr topPtr 1
    let nexWid = topWid - 1

    if fstByt == 0 then do
        pure (mempty, BS.BS nexPtr nexWid)
    else if fstByt < 128 then do
        pure (BS.BS topPtr 1, BS.BS nexPtr nexWid)
    else do
        if (fstByt `testBit` 6) then do
            let widthOfWidth = fromIntegral (fstByt .&. 63)

            -- TODO Change data format to eliminate this edge-case
            when (widthOfWidth == 0) do
                throwIO LEAF_HAS_IMPOSSIBLE_SIZE

            when (nexWid < widthOfWidth) do
                throwIO EOF

            let widthBytes = BS.BS nexPtr widthOfWidth

            when (0 == BS.last widthBytes) do
                throwIO NAT_HAS_TRAILING_ZEROS

            let widthNat = bytesNat widthBytes
            let widthInt = fromIntegral widthNat

            -- We already know it can't be zero because it's a non-empty
            -- byte-string that doesn't end with a zero byte.

            when (widthNat /= fromIntegral widthInt) do
                throwIO LEAF_HAS_IMPOSSIBLE_SIZE

            let bytesAfterWidth = plusForeignPtr nexPtr widthOfWidth
            let bytesAfterData  = plusForeignPtr bytesAfterWidth widthInt
            let countAfterData  = nexWid - (widthOfWidth + widthInt)

            pure ( BS.BS bytesAfterWidth widthInt
                 , BS.BS bytesAfterData countAfterData
                 )

        else do
            let datumWidth::Int = fromIntegral (fstByt .&. 63)

            -- TODO Change data format to eliminate this edge-case
            when (datumWidth == 0) do
                throwIO LEAF_HAS_IMPOSSIBLE_SIZE

            when (nexWid < datumWidth) do
                throwIO EOF

            pure ( BS.BS nexPtr datumWidth
                 , BS.BS (plusForeignPtr nexPtr datumWidth)
                         (nexWid - datumWidth)
                 )


{-
    This directly loads data into Fan values without using the evaluation
    machinery, and rejects inputs that are not in normal form.

    There is no need to call "normalize" on the result, since we always
    produce closures in their canonical shape (the head of a closure is
    never a closure).

    TODO Make this code "actually fast", there's a lot of toList,
    fromList, etc.  We also operating on lazy lists of bools, which is
    a pretty stupidly slow way to go about this.
-}
loadBody :: Vector Pin -> ByteString -> Either Text Fan
loadBody pinz bodByt = do
    if (length bodByt `mod` 8) /= 0 then
        Left "Input length must be a multiple of 8 bytes"
    else do
        (leaves, numFrags, fragBits) <-
            unsafePerformIO do
                try (loadLeavesTable (PIN <$> pinz) bodByt) >>= \case
                    Left (err :: LoadException) ->
                        pure (Left (pack $ displayException err))
                    Right vl ->
                        pure (Right vl)

        finalSt <- runExcept $ flip execStateT (Jelly.FRAG_ST leaves fragBits) do
                      frags <- replicateM_ numFrags getFrag
                      Jelly.checkZeroPadding
                      pure frags

        let tab = finalSt.table
        let wid = length tab

        when (wid == 0) do
            Left "getJelly: No data"

        pure (tab ! (wid-1))
  where

    -- So, each fragment is a pair, but so we omit the leading 1 bit.
    -- Therefore, we jump into the closure parser in the same state we
    -- would be in when we see a 1 bit in normal tree parsing.
    getFrag :: Jelly.LoadTrees Fan ()
    getFrag = do
        res <- getTree 1
        st <- get
        put (st { Jelly.table = snoc st.table res })
        pure ()

    -- 1110f0x0y0z
    -- ((( f x y z
    -- (((f x) y) z)

    getTree :: Int -> Jelly.LoadTrees Fan Fan
    getTree !oneBits = do
        -- get >>= traceM . (\x -> "(STEP " <> x <> ")") . show
        Jelly.getBit >>= \case
            True  -> getTree (oneBits + 1)
            False -> do
                -- get >>= traceM . (\x -> "(DONE " <> x <> ")") . show
                hed <- Jelly.getLeaf
                -- traceM $ show ("oneBits"::Text, oneBits, "hed"::Text, hed)
                if oneBits == 0 then do
                    pure hed
                else do
                    args <- replicateM oneBits (getTree 0)
                    construct hed args

    construct :: Fan -> Vector Fan -> Jelly.LoadTrees a Fan
    construct hed arg = do
        let width    = length arg
        let hedArity = evalArity hed
        let resArity = hedArity - width

        case hed of
           -- The head can be a closure if the head "leaf" is a
           -- back-reference to a fragment that occured more than once.
           KLO _ xs -> do
               let realHed = (xs ^ 0)
               let hedArgs = fromList $ drop 1 $ toList xs
               construct realHed (hedArgs <> arg)

           _ | width == 0 -> do
               pure hed

           -- If the result is in WHNF, then no evaluation can happen.
           -- It's safe to just directly construct a KLO node.
           _ | resArity > 0 ->
               pure $ KLO resArity $ smallArrayFromList $ (hed : toList arg)

           NAT 0 ->
               case (arg!0, arg!1, arg!2) of
                   (NAT n, NAT a, b) -> constructLaw n a b (drop 3 arg)
                   _                 -> lift $ throwE "Invalid law literal"

           COw{} -> hydrateLaw hed arg
           CAB{} -> hydrateLaw hed arg
           _     -> lift $ throwE ("Result is not in normal form: " <> tshow (hed, toList arg))

{-
    doLaw
            (NAT n, NAT a, b) -> constructLaw n a b (drop 3 arg)
            _                 -> lift $ throwE "Invalid law literal"

-}
    constructLaw :: Nat -> Nat -> Fan -> Vector Fan -> Jelly.LoadTrees a Fan
    constructLaw n a b args = hydrateLaw (mkLawPreNormalized (LN n) a b) args

    hydrateLaw :: Fan -> Vector Fan -> Jelly.LoadTrees a Fan
    hydrateLaw hed args = do
        let
            numArgs = length args
            ari = evalArity hed - numArgs
            klo = KLO ari $ smallArrayFromList (hed : toList args)

        case hed of
            _ | numArgs == 0 -> pure hed
            _ | ari > 0      -> pure klo
            _ | ari < 0      -> lift $ throwE "Oversaturated law"

            -- Here we know that the evalArity of the result is 0.
            -- Either this is an actually-saturated form (no good),
            -- or it's a row/tab literal.

            COw{}  -> pure $ ROW $ reverse args
            CAB ks ->
                case toList args of
                    [ROW vs] | length ks == length vs ->
                        pure $ TAb $ mapFromList $ zip (S.toList ks) (toList vs)
                    [arg] ->
                        pure (hed %% arg) -- malformed cab, just use eval path
                    _ -> error "what?"

            _     -> lift (throwE "Oversatured law")


--------------------------------------------------------------------------------

-- TODO: We can implement a faster version of `saveFan` that re-uses
-- the jelly context, but it requires  cordinatination with the caller,
-- because it will break if a dependency has not yet been saved, and
-- is then processed (in the middle of the processing of the outer pin)
-- using the same context.
--
-- I do this on my other branch because the phase-shift from Pin ->
-- CanonicalPin is explicit, so it's tractable to ensure this invariant.
-- However, with the current approach (of having every pin get a lazy
-- hash/blob field) it's too complicated.

{-
    This is unsafe because it requires that every sub-pin already be
    canonized.

    Unfortunaty, this is over twice as fast as going through Jelly.Node.
    If we can find some way to eliminate that overhead and get GHC to
    inline everything, that would make this unnecssary.
-}
{-# INLINE saveFan #-}
saveFan :: Fan -> IO (Vector Pin, ByteString, ByteString)
saveFan top = Jelly.withContext \ctx -> saveFan' ctx top

{-# INLINE saveFan' #-}
saveFan' :: Jelly.Ctx -> Fan -> IO (Vector Pin, ByteString, ByteString)
saveFan' ctx top = do
    vPins <- newIORef []
    vTemp <- newIORef []
    saveFanWorker ctx vPins vTemp top

{-
    This is just broken off into a separate function for syntactic reasons
    (to make avoid needing to move the whole `where`  block into let
    clauses within the top-level `do` block)
-}
{-# INLINE saveFanWorker #-}
saveFanWorker
    :: Jelly.Ctx
    -> IORef [(Pin, ByteString)]
    -> IORef [ByteString]
    -> Fan
    -> IO (Vector Pin, ByteString, ByteString)
saveFanWorker !ctx !vPins !vTemp !top = do
    loop top

    ()  <- Jelly.c_done ctx

    hed_wid <- Jelly.c_head_size ctx
    hed_ptr <- mallocForeignPtrBytes (fromIntegral hed_wid)
    withForeignPtr hed_ptr \buf -> do
        void (BS.memset buf 0 hed_wid)
        Jelly.c_save_head ctx hed_wid buf

    bod_wid <- Jelly.c_body_size ctx
    bod_ptr <- mallocForeignPtrBytes (fromIntegral bod_wid)
    withForeignPtr bod_ptr \buf -> do
        void (BS.memset buf 0 bod_wid)
        Jelly.c_save_body ctx bod_wid buf

    -- Make sure none of the temporary bytestrings are freed until
    -- the C code is done with them.
    Jelly.c_wipe ctx
    readIORef vTemp >>= touch

    pinList <- map fst . fromList . reverse <$> readIORef vPins
    let hed_bs = BS.BS hed_ptr (fromIntegral hed_wid)
    let bod_bs = BS.BS bod_ptr (fromIntegral bod_wid)
    pure (pinList, hed_bs, bod_bs)
  where
    -- COW 3 = (0 0 4 0)
    doCow :: Nat -> IO Jelly.CNode
    doCow n = do
        z    <- Jelly.c_word ctx 0
        zz   <- Jelly.c_cons ctx z z
        r    <- doNat (n+1)
        zzr  <- Jelly.c_cons ctx zz r
        zzrz <- Jelly.c_cons ctx zzr z
        pure zzrz

    -- Keys must be given in descending order.
    --
    -- Example Shape:
    --
    --     %[y x] = (0 0 3 (0 0 3 0 y x))
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
    doCab :: Int -> [Fan] -> IO Jelly.CNode
    doCab len keyz = do
        let go acc []     = pure acc
            go acc (x:xs) = do key  <- loop x
                               acc' <- Jelly.c_cons ctx acc key
                               go acc' xs
        z    <- Jelly.c_word ctx 0
        o    <- Jelly.c_word ctx 1
        zo   <- Jelly.c_cons ctx z o
        t    <- Jelly.c_word ctx 2
        zot  <- Jelly.c_cons ctx zo t
        zzrz <- doCow (fromIntegral len)
        row  <- go zzrz keyz
        Jelly.c_cons ctx zot row

    doNat (GHC.NatS# w) = do
        Jelly.c_word ctx (fromIntegral (W# w))

    doNat n@(GHC.NatJ# _) = do
        let bs@(BS.BS fpt wid) = natBytes n
        modifyIORef' vTemp (bs:)
        withForeignPtr fpt \buf -> do
            Jelly.c_nat ctx (fromIntegral wid) buf

    loop :: Fan -> IO Jelly.CNode
    loop = \case
        NAT n -> do
            doNat n

        PIN pin -> do
            bs@(BS.BS fpt _) <- hashToByteString <$> getPinHash pin
            alloca \vIsUnique -> do
                poke vIsUnique (CBool 0)
                res <- withForeignPtr fpt \buf -> do
                    Jelly.c_pin ctx vIsUnique (castPtr buf)

                peek vIsUnique >>= \case
                    CBool 0 -> pure ()
                    CBool _ -> modifyIORef' vPins ((pin, bs):)

                pure res

        BAR (BS.BS fpt wid) -> do
            withForeignPtr fpt \buf -> do
                Jelly.c_bar ctx (fromIntegral wid) buf

        COw n ->
            doCow n

        CAB ks -> do
            let wid  = length ks
            let keyz = S.toDescList ks
            doCab wid keyz

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
                        y <- Jelly.c_cons ctx acc x
                        go y (i-1)

            start <- doCow (fromIntegral $ length row)
            let lastIx = length row - 1
            go start lastIx

        --  #[3=4 5=6] = (%[3 5] [4 6])
        TAb tab -> do
            ks <- doCab (length tab) (M.keys tab)
            vs <- loop (tabValsRow tab)
            kv <- Jelly.c_cons ctx ks vs
            pure kv

        KLO _ env -> do
            let !end = sizeofSmallArray env

            let go !acc !i | i>=end = pure acc
                go !acc !i = do
                    !x <- loop (env ^ i)
                    !y <- Jelly.c_cons ctx acc x
                    go y (i+1)

            !start <- loop (env ^ 0)
            go start 1

        FUN (L (LN nv) av bv _) -> do
            z    <- Jelly.c_word ctx 0
            n    <- doNat nv
            zn   <- Jelly.c_cons ctx z n
            a    <- doNat av
            zna  <- Jelly.c_cons ctx zn a
            b    <- loop bv
            znab <- Jelly.c_cons ctx zna b
            pure znab

        -- This is complicated, and it does not need to be fast.
        fan@REX{} -> do
            let (!x, !y) = boom fan
            !xv <- loop x
            !yv <- loop y
            Jelly.c_cons ctx xv yv


-- Pack ------------------------------------------------------------------------

-- Note: PinStorage is destined to be a TAB, so use a Map instead of a HashMap
-- here, even though it'd be faster.
type PinStorage = Map Hash256 (Vector Hash256, ByteString)

data Pack = PACK {
  top        :: Hash256,
  pinStorage :: PinStorage
  }

instance ToNoun Pack where
  toNoun PACK{..} = mkRow [toNoun top, toNoun pinStorage]

instance FromNoun Pack where
  fromNoun n = do
    r <- getRawRow n
    guard (length r == 2)
    PACK <$> fromNoun (r!0)
         <*> fromNoun (r!1)

instance ToNoun Hash256 where
  toNoun = toNoun . hashToByteString

instance FromNoun Hash256 where
  fromNoun n = do
    b <- getRawBar n
    guard (length b == 32)
    pure $ toHash256 b

{-
        We should have a version of this that is given a callback which
        loads the blob, either from disk or from local cache.
-}
collect :: Pin -> IO Pack
collect topPin =
    Jelly.withContext \ctx -> do
        (haz, tab) <- runStateT (collectWorker ctx topPin) mempty
        pure (PACK haz tab)

collectWorker :: Jelly.Ctx -> Pin -> StateT PinStorage IO Hash256
collectWorker ctx pin = do
    readIORef pin.node >>= \case
        Nothing -> weSerialize Nothing
        Just dagInfo -> do
            tab <- get
            case lookup dagInfo.hash tab of
                Just{}  -> pure dagInfo.hash
                Nothing -> weSerialize (Just dagInfo)

  where
    loop = collectWorker ctx

    weSerialize maybeDagInfo = do
        traverse_ loop (listOfSubPins maybeDagInfo)
        (refs, hed, bod) <- liftIO (saveFan' ctx pin.item)
        haz <- liftIO (Jelly.hash' ctx hed bod)
        writeIORef pin.node (Just (DAG_INFO haz refs))
        hashRefs <- traverse (liftIO . forciblyGetHashRef) refs
        modify' (insertMap haz (hashRefs, bod))
        pure haz

    -- We alread added it so it best be there.  We can't use the
    -- result of the `traverse_ loop` since the list it runs against
    -- maybe not be not deduplicated.
    forciblyGetHashRef :: Pin -> IO Hash256
    forciblyGetHashRef pin =
        readIORef pin.node >>= \case
            Nothing -> error "collectWorker: impossible"
            Just vl -> pure vl.hash

{-
                hashRefs <- for dag.refs loop
                modify' (insertMap dag.hash (hashRefs, error "pin.blob"))
                pure dag.hash
-}

    listOfSubPins (Just dagInfo) = toList dagInfo.refs
    listOfSubPins Nothing        = subPins pin.item

-- This produces a lazy stream of all of the direct-pin-references inside
-- a Fan.
--
-- The resulting list may contain multiple references to the same pin,
-- and is not the same values as the edge-list used in `DagInfo` and
-- `Jelly`.
--
-- This exists we want to work with pins that are not yet serialized
-- and we can't compute the proper edge-list until we serialize.
subPins :: Fan -> [Pin]
subPins BAR{}      = []
subPins COw{}      = []
subPins NAT{}      = []
subPins (PIN p)    = pure p
subPins (FUN f)    = subPins f.body
subPins (KLO _ xs) = do x <- toList xs; subPins x
subPins (ROW xs)   = do x <- toList xs; subPins x
subPins (TAb xs)   = do (k,v) <- mapToList xs; subPins k <> subPins v
subPins (CAB fs)   = do x <- toList fs; subPins x
subPins (REX r)    = do x <- toList r; subPins x



reconstruct :: Hash256 -> PinStorage -> IO Pin
reconstruct topHash hashTab =
    evalStateT (loop topHash) mempty
  where
    build hash = do
        case lookup hash hashTab of
            Nothing -> error "invalid pack"
            Just (refs, body) -> do
                deps <- for refs loop
                case loadBody deps body of
                  Left x  -> error $ "invalid pack: " <> show x
                  Right f -> do
                    p <- liftIO $ mkPin' f
                    modify' (insertMap hash p)
                    pure p

    loop :: Hash256 -> StateT (Map Hash256 Pin) IO Pin
    loop hash = do
        tab <- get
        case lookup hash tab of
            Just f  -> pure f
            Nothing -> build hash

magicHeader :: ByteString
magicHeader = "JELLYPAK"

-- | Packages a fan value including all pins into a bytestring.
savePack :: Fan -> IO ByteString
savePack f = do
  pack <- mkPin' f >>= collect
  (pins, _, payload) <- saveFan $ toNoun pack
  pure $ magicHeader <> payload

-- | Loads a previously packed bytestring pack to a full Fan value.
loadPack :: ByteString -> IO (Either Text Fan)
loadPack bs = do
  let (header, payload) = splitAt 8 bs
  if header /= magicHeader then
    pure $ Left "Not a Jelly Pack"
  else
    case loadBody V.empty payload of
      Left x -> pure $ Left x
      Right f | Just PACK{top,pinStorage} <- fromNoun f -> do
        p <- reconstruct top pinStorage
        pure $ Right $ p.item
      Right f -> pure $ Left $ "Invalid value in pack: " <> tshow f

{-
    This is only used when evaluation needs the hash before persistance.
    For example, do to jet matching or equality checking.

    This is somewhat time-wasteful, but reduces memory usage.

    This only happens if a function is run before being written to disk.
    Or if two pins (that are not pointer equals and have different
    quick-hash values) are checked for equality (before they have been
    written to disk).

    Persistence should calculate the hash itself (and fill this slot
    itself if it is still unfilled).
-}
computeDagInfo :: Pin -> IO DagInfo
computeDagInfo pin = do
    res <- atomicModifyIORef' pin.node \case
        Just di -> (Just di, di)
        Nothing ->
            let di = unsafePerformIO calcDagInfo
            in (Just di, di)
    evaluate res
  where
    calcDagInfo = do
        Jelly.withContext \ctx ->  do
            (refs, hed, bod) <- saveFan' ctx pin.item
            hash <- Jelly.hash' ctx hed bod
            pure (DAG_INFO hash refs)

getPinDagInfo :: Pin -> IO DagInfo
getPinDagInfo pin = do
    readIORef pin.node >>= \case
        Nothing -> computeDagInfo pin
        Just di -> pure di

getPinHash :: Pin -> IO Hash256
getPinHash = fmap (.hash) . getPinDagInfo

unsafeGetPinDagInfo :: Pin -> DagInfo
unsafeGetPinDagInfo = unsafePerformIO . getPinDagInfo

unsafeGetPinHash :: Pin -> Hash256
unsafeGetPinHash = unsafePerformIO . getPinHash
