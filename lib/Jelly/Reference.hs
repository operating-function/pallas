-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall -Wno-ambiguous-fields #-}

{-|

    Jelly is Plunder's take on Urbit's Jam function.

    ## Jam

    Jam is an extremely simple an elegant serialization system.
    Basically it takes a structure like

        data Noun = ATOM Natural | CELL Noun Noun

    And serializes it using a simple bit-encoding scheme.  A 1 bit
    indicates a cell, and a 0 bit indicates an atom.  Atoms are encoded as
    a unary number (a string of zero bits) to encode the length-of-length,
    the same number of bits to encode the bit-width, and then the actual
    binary data.

    Oh, but I lied!  Because Urbit's trees are massively duplicated,
    we add a concept of a "backreference".

    Now, 1 bit indicates a cell, 00 indicates an atom and 01 indicates
    a back-reference.  Back-references are encoded as the bit-offset of
    a node that appeared earlier in the sequence.

    This is a simple and elegant format, but it has a number of
    performance problems that makes it not well suited to the needs
    of Plunder.

    ### Problems with Jam

    Decoding is Slow.

    -   When decoding, we need to maintain a large table mapping the
        bit-offset of every node into it's value.

    -   All binary data is encoded as natural numbers, and nothing
        is byte-aligned.  We need to manually reconstruct everything,
        we can't use memcpy, and we can't just have values be slices
        into larger buffer.

    Encoding is very expensive.

    -   During jam-encoding, a hash table must be constructed that
        contains every single node in the tree.  Hash collisions on
        similar objects must be resolved with a recursive equality check
        on a large tree.

    -   Because of the above cost, Urbit tags EVERY SINGLE CELL, every
        single heap object, with a fairly good hash.  This makes the
        above tractable, but comes at a fairly extreme cost to overall
        system performance.  Without that hash, the above table becomes
        extremely expensive to construct.

    -   All data is serialized using an expensive bit-encoding system.

    ### Solutions

    First of all, Plunder has the "pins" system, which means that each
    pin has little duplication in practice.  We still want to de-duplicate
    so that repeated fragments will be shared in memory on load, but we
    wont have these massive duplcated sub-trees in practice.

    But our format needs to get a bit more expensive to accomidate pins.

    Second, we don't have a mug-hash on every heap object, so we need
    a system of deduplication that doesn't require us to maintain a
    hash-table keyed on every single sub-tree.

    Third, we will be snapshotting directly into this format, instead
    of just paging the heap directly to disk.  That puts a lot more
    burden on decode performance, especially for large binary blobs.

    In particular, we want to be able to mmap pins directly into memory
    and have binary objects loaded from disk be represented as a direct
    pointer into this blob.

    That means that all of our binary data needs to be byte-aligned.

    ## Jelly

    Jelly serializes trees that look like this:

        type Pin = (Node, Hash256)

        data Node
            = PIN (Node, Hash256)
            | BAR ByteString
            | NAT Natural
            | CEL Node Node

    For PIN nodes, we don't recurse into them, but just serializes them
    as a hash-reference to another Jelly node.  (Jelly is stored in a
    DAG structure).

    We serializes Nodes by first converting that into something like this:

        data Fragment
            = REFR Bits -- Constant-width bit-array
            | CELL Fragment Fragment

        data Context = CONTEXT
            { pins  :: Vector Hash256
            , bars  :: Vector ByteString
            , nats  :: Vector Natural
            , frags :: Vector Fragment
            }

    And then directly serializing that.

    Every leaf of a Fragment is a back-reference to something earlier
    in the context, and every fragment-leaf has a fixed bit-width
    (determined by the number of things that it could refer to).

    As we convert a node to a fragment, we deduplicate.  We walk the
    tree recursively left-side-first, starting from the leaves,
    and we intern every node (assigning it a unique identifier).

    We deduplicate leaves directly using a hash-table, and we
    deduplicate fragments by treaing them as a pair of two unique
    identifiers.  Because each sub-node of a frament CELL has already
    been de-duplicated, if the pair of identifiers is equal, then the
    whole tree struture is equal.  Hash collisions never require a
    traversal of the actual input tree structure.

    We serialize the "Context" structure by simply walking it from
    top-to-bottom and dumping everything into a buffer.

    We use a byte-encoding scheme to encode bars.  The empty bar is
    encoded as the single-byte 0.  Also a single-byte byte-array in the
    range [1..127] is encoded as a single byte.

    Otherwise, the first byte will have it's high bit set, and will
    indicate the byte-length.

    If the first byte has the form 10xxxxxx, then we interpret the low
    bits directly as a byte-length.  (For lengths up to 63 bytes)

    If the first byte has the form 11xxxxxx, then we read $xxxxxx bytes
    of data, interpret that as an LSB number, and read that number of
    bytes of actual data.

    We encode natural numbers by simple dumping them to a byte-array
    (LSB first, final byte is never zero) and dumping that using the
    same scheme.

    Since pin-references are always 32 bytes, we don't need to write a
    lenght, and instead just dump the entire 32 bytes for each hash.

    We encode fragment trees using a bit-encoding scheme similar to the
    one used in `jam`.  A 1 bit indicates a pair, and a 0 bit indicates
    a leaf.  Every leaf is a fixed number of bits, determined by the
    bit-width of the maximum possible back-reference.

    Since each (duplicated, non-leaf) fragment is a cell, we omit the
    outer-most 1 bit.

    In the edge-case where the entire input is a single leaf, we simply
    don't write out any fragments.  The one leaf in the header is
    the value.

    Finally we extend the buffer with enough zero-bits so that the
    entire buffer is a multiple of 64 bits.  This makes bit-encoding
    and bit-decoding trees faster because we can treat the buffer as an
    array of words and do all of the encoding and decoding using register
    operations on words.

    Finally, we dump the overall structure using the following algorithm:

        def DumpNat(n):
            DumpBar(NatBar(n))

        def DumpContext(..):
            DumpNat(pins.length)
            for p in pins:
                DumpPin(p)
            DumpNat(bars.length)
            for b in bars:
                DumpBar(b)
            DumpNat(nats.length)
            for n in nats:
                DumpNat(n)
            DumpNat(frags.length)
            for f in frags:
                DumpFrag(f)
            ZeroPad()

-}
module Jelly.Reference
    ( loadDeps
    , loadBody
    , loadLeavesTable
    , checkZeroPadding
    , splitBlob
    , mkBlob
    , save
    , IsJelly(..)
    , Node(..)
    , FragSt(..)
    , LoadTrees
    , getBit
    , getLeaf
    )
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Vector                      ((!))
import GHC.Natural
import GHC.Word
import Jelly.Types
import PlunderPrelude                   hiding (Builder)

-- ort Text.Show.Pretty
import Data.ByteString.Builder (Builder)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.Vector             as V


-- Types -----------------------------------------------------------------------

data ShowNode
    = W Word64
    | N Nat
    | B [Word8]
    | P Text
    | C ShowNode ShowNode

showSeq :: ShowNode -> [ShowNode] -> String
showSeq (C h t) ts = showSeq h (t:ts)
showSeq h ts       = "[" <> intercalate " " (show <$> (h:ts)) <> "]"

instance Show ShowNode where
    show (C x y)        = showSeq x [y]
    show (W w)          = show w
    show (N n)          = "(NAT " <> show n <> ")"
    show (B b) | null b = "(BAR)"
    show (B b)          = "(BAR " <> intercalate " " (show <$> b) <> ")"
    show (P p)          = "(PIN " <> unpack p <> ")"

showNode :: Node a -> ShowNode
showNode = \case
    WORD w   -> W w
    NAT n    -> N n
    BAR b    -> B (unpack b)
    PIN _ p  -> P (hashToBTC p)
    CONS x y -> C (showNode x) (showNode y)

instance Show (Node a) where
    show = show . showNode

instance IsJelly (Node()) where
    type JellyPin (Node()) = Hash256

    _WORD = WORD . fromIntegral
    _NAT  = NAT
    _BAR  = BAR
    _PIN  = PIN ()
    _CONS = CONS


-- Conversions to NODE should be lazy, think of it as an iterate over
-- Fan-like tree.
data Node a
    = WORD !Word64
    | NAT !Nat
    | BAR !ByteString
    | PIN a Hash256
    | CONS (Node a) (Node a)
  deriving (Eq, Ord, Generic)
  deriving anyclass (NFData)

class IsJelly a where
    type JellyPin a
    _WORD :: Word -> a
    _NAT  :: Nat -> a
    _BAR  :: ByteString -> a
    _PIN  :: JellyPin a -> a
    _CONS :: a -> a -> a


-- Saving ----------------------------------------------------------------------

-- Equivalent to (Vector a) but with O(log(n)) elemIndex and O(log(n))
-- append.
data KeyedSet a = KEYED_SET
    { wid :: Int
    , tab :: Map a Int
    , vec :: IntMap a
    }

instance Show a => Show (KeyedSet a) where
  show = show . zip [0::Int ..] . ksList

ksIns :: Ord a => a -> KeyedSet a -> Either (Int, KeyedSet a) Int
ksIns key ks =
    case lookup key ks.tab of
        Just ix -> Right ix
        Nothing ->
            Left $ (ks.wid,) $
                ks { wid = ks.wid + 1
                   , tab = insertMap key ks.wid ks.tab
                   , vec = insertMap ks.wid key ks.vec
                   }

ksList :: KeyedSet a -> [a]
ksList ks = fmap snd $ mapToList ks.vec

data Item
    = I_PIN Int
    | I_BAR Int
    | I_NAT Int
    | I_BAK Int
    | I_CEL Int Int
  deriving (Eq, Ord, Show)

data Collect a = COL
    { ref :: [a]
    , pin :: KeyedSet Hash256
    , bar :: KeyedSet ByteString
    , nat :: KeyedSet Nat
    , itm :: KeyedSet Item
    , cnt :: IntMap Nat
    , frg :: KeyedSet (Int, Int)
    }
  deriving (Show)

emptyKS :: Ord a => KeyedSet a
emptyKS = KEYED_SET 0 mempty mempty

emptyCollect :: Collect a
emptyCollect = COL mempty emptyKS emptyKS emptyKS emptyKS mempty emptyKS

save :: ∀a. Show a => Node a -> (Vector a, ByteString, ByteString)
save node = flip evalState emptyCollect do
    -- traceM $ ppShow ("save"::Text, node)
    top <- collect node
    -- preFragSt <- get
    -- traceM $ ppShow ("save.preFrag"::Text, preFragSt)
    shatter top
    st <- get
    -- traceM $ ppShow ("save.postFrag"::Text, st)

    pure ( fromList (reverse st.ref)
         , build $ concat ( Builder.word64LE (fromIntegral st.pin.wid)
                          : fmap buildHash (ksList st.pin)
                          )
         , padWord64 (build (dumpPrelude st) <> dumpTrees st)
         )

padWord64 :: ByteString -> ByteString
padWord64 bs =
    let extra = length bs `mod` 8
    in if extra == 0
       then bs
       else bs <> BS.replicate (8-extra) 0

build :: Builder -> ByteString
build = toStrict . Builder.toLazyByteString

bitsBytes :: [Bool] -> [Word8]
bitsBytes []   = []
bitsBytes bits = bitsNum (take 8 bits) : bitsBytes (drop 8 bits)

dumpTrees :: Collect a -> ByteString
dumpTrees st =
    pack $ bitsBytes $ concat $ zipWith fragBits [0..] $ ksList st.frg
  where
    pinOff = 0
    barOff = pinOff + st.pin.wid
    natOff = barOff + st.bar.wid
    frgOff = natOff + st.nat.wid

    leafBits :: Int -> Word64 -> [Bool]
    leafBits width word = go 0
      where
        go i | i == width = []
        go i              = testBit word i : go (i+1)

    fragBits :: Int -> (Int, Int) -> [Bool]
    fragBits nthFrag (h, t) = treeBits nthFrag h <> treeBits nthFrag t

    treeBits :: Int -> Int -> [Bool]
    treeBits nthFrag k =
        let numRefs = fromIntegral (frgOff + nthFrag) :: Word64
            maxRef  = numRefs - 1 -- If there's a fragment, there's
                                  -- always at least one thing to
                                  -- reference.  Safe to decrement.
            refWidth = fromIntegral (64 - countLeadingZeros maxRef)
        in -- trace ("maxRef:" <> show maxRef<> ",refWidth:" <> show refWidth) $
           case lookup k st.itm.vec of
            Nothing          -> error "Impossible: Broken item id"
            Just (I_CEL h t) -> True  : fragBits nthFrag (h, t)
            Just (I_PIN i)   -> False : (leafBits refWidth $ fromIntegral (pinOff + i))
            Just (I_BAR i)   -> False : (leafBits refWidth $ fromIntegral (barOff + i))
            Just (I_NAT i)   -> False : (leafBits refWidth $ fromIntegral (natOff + i))
            Just (I_BAK i)   -> False : (leafBits refWidth $ fromIntegral (frgOff + i))

dumpLen :: Int -> Builder
dumpLen = dumpNat . fromIntegral

dumpNat :: Natural -> Builder
dumpNat = dumpBar . natBytes

dumpBar :: ByteString -> Builder
dumpBar bs =
    let -- crashes in 0 case, but isn't evaluated in that case
        hd = BS.head bs
    in case
        length bs
    of
        0                    -> Builder.word8 0
        1 | (hd>0 && hd<128) -> Builder.word8 (BS.head bs)
        n | n < 64           -> let pre = fromIntegral n `setBit` 7
                                in Builder.word8 pre <> Builder.byteString bs
        n                    -> let wid = natBytes (fromIntegral n)
                                    pre = ((fromIntegral (length wid) `setBit` 6) `setBit` 7)
                                in Builder.word8 pre
                                <> Builder.byteString wid
                                <> Builder.byteString bs

dumpPrelude :: Collect a -> Builder
dumpPrelude c = mconcat $ concat
    [ dumpLen c.bar.wid : [dumpBar b | b <- ksList c.bar]
    , dumpLen c.nat.wid : [dumpNat n | n <- ksList c.nat]
    , [dumpLen c.frg.wid]
    ]

shatter :: ∀a. Int -> State (Collect a) ()
shatter top = do
    let getItem i = do
            st <- get
            pure $ fromMaybe (error "Impossible: Broken item id") do
                refs <- lookup i st.cnt
                item <- lookup i st.itm.vec
                pure (refs, item)

    let goCell :: Nat -> Nat -> Int -> (Int, Int) -> State (Collect a) ()
        goCell parentRefs myRefs i (h,t) = do
            -- traceM $ show ("goCell"::Text, i, (h,t), (parentRefs, myRefs))
            goItem myRefs h
            goItem myRefs t
            when (myRefs > parentRefs) do
                -- traceM ("FRAG:" <> show i)
                st <- get
                case ksIns (h,t) st.frg of
                    Right _ -> error "Impossible: Double Frag"
                    Left (bk, frg') -> do
                        let vec' = insertMap i (I_BAK bk) st.itm.vec
                        put $ st { itm = st.itm { vec = vec' }
                                 , frg = frg'
                                 }
                pure ()

        goItem :: Nat -> Int -> State (Collect a) ()
        goItem parentRefs i = do
            getItem i >>= \case
                (myRefs, I_CEL h t) -> goCell parentRefs myRefs i (h,t)
                _                   -> pure ()

    goItem 0 top

addItem :: Item -> State (Collect a) Int
addItem itm = do
    st <- get
    case ksIns itm st.itm of
        Right ix    -> ix <$ put st{ cnt=(adjustMap succ ix st.cnt) }
        Left(ix,ks) -> ix <$ put st{ itm=ks, cnt=(insertMap ix 1 st.cnt) }

addPin :: Show a => a -> Hash256 -> State (Collect a) Item
addPin i h = do
    st <- get
    case ksIns h st.pin of
        Right ix    -> I_PIN ix <$ pure ()
        Left(ix,ks) -> I_PIN ix <$ put st{pin=ks, ref=(i:st.ref)}

addBar :: ByteString -> State (Collect a) Item
addBar h = do
    st <- get
    case ksIns h st.bar of
        Right ix     -> I_BAR ix <$ pure ()
        Left (ix,ks) -> I_BAR ix <$ put st{bar=ks}

addNat :: Nat -> State (Collect a) Item
addNat h = do
    st <- get
    case ksIns h st.nat of
        Right ix     -> I_NAT ix <$ pure ()
        Left (ix,ks) -> I_NAT ix <$ put st{nat=ks}

collect :: Show a => Node a -> State (Collect a) Int
collect node = do
    item <- case node of
                PIN i h  -> addPin i h
                BAR b    -> addBar b
                NAT b    -> addNat b
                WORD w   -> addNat (fromIntegral w)
                CONS x y -> (I_CEL <$> collect x <*> collect y)
    addItem item

-- Loading ---------------------------------------------------------------------

data FragSt a = FRAG_ST
    { table :: !(Vector a)
    , bits  :: [Bool]
    }

instance Show a => Show (FragSt a) where
    show st = concat [ show (toList st.table)
                     , ":"
                     , concat (st.bits <&> bool "0" "1")
                     ]

type LoadTables = StateT [Word8] (ExceptT Text Identity)

type LoadTrees a = StateT (FragSt a) (ExceptT Text Identity)

loadBody
    :: ∀a. (Show a, IsJelly a)
    => Vector a
    -> ByteString
    -> Either Text a
loadBody tab bodByt = do
    if (length bodByt `mod` 8) /= 0 then
        Left "Input length must be a multiple of 8 bytes"
    else do
        runLoadTables (getJelly tab) bodByt

loadLeavesTable
    :: ∀a. (Show a, IsJelly a)
    => Vector a
    -> ByteString
    -> Either Text (Vector a, Int, [Bool])
loadLeavesTable tab bodByt = do
    if (length bodByt `mod` 8) /= 0 then
        Left "Input length must be a multiple of 8 bytes"
    else do
        runLoadTables (getLeavesTable tab) bodByt

runLoadTables :: LoadTables a -> ByteString -> Either Text a
runLoadTables act bs = runExcept $ evalStateT act $ unpack $ bs

-- Just check the size and run `peek`.
readWord64LSB :: ByteString -> Word64
readWord64LSB =
    go 0 0 . unpack
  where
    go !acc !_ []     = acc
    go !acc  8 _      = acc
    go !acc !i (w:ws) = go (acc .|. (fromIntegral w `shiftL` (i*8))) (i+1) ws

mkBlob :: Vector Hash256 -> ByteString -> ByteString
mkBlob refs bod =
    build ( Builder.word64LE (fromIntegral $ length refs)
         <> concat (buildHash <$> refs)
         <> Builder.byteString bod
          )

splitBlob :: ByteString -> Either Text (ByteString, ByteString)
splitBlob bs = do
    let siz = length bs
    when (siz < 8) do
        Left "Head is not big enough to encode length"

    let numPins = readWord64LSB bs
    let required = (8 + fromIntegral numPins * 32) :: Int

    when (fromIntegral siz < required) do
        Left $ tshow ( "splitBlob: Blob not big enough to encode head"::Text
                     , ("width"::Text, siz)
                     , ("required"::Text, required)
                     )

    pure (take required bs, drop required bs)

loadDeps :: ByteString -> Either Text (Vector Hash256)
loadDeps bs = do
    let siz = length bs

    when (siz < 8) do
        Left "Head is not big enough to encode length"

    let numPins = fromIntegral (readWord64LSB bs)
    let expected = (8 + fromIntegral numPins * 32) :: Int

    when (fromIntegral siz < expected) do
        Left $ tshow ( "loadDeps: Head has wrong size"::Text
                     , ("width"::Text, siz)
                     , ("expected"::Text, expected)
                     )

    let f remain = (toHash256 $ take 32 remain, drop 32 remain)
    pure $ V.unfoldrExactN numPins f $ drop 8 bs

getLeavesTable
    :: (Show a, IsJelly a)
    => Vector a
    -> LoadTables (Vector a, Int, [Bool])
getLeavesTable pinz = do
    numBars <- loadLen
    bars <- replicateM numBars loadBar
    numNats <- loadLen
    nats <- replicateM numNats loadNat
    numFrags <- loadLen

    get <&> \remain -> ( pinz <> bars <> nats
                       , numFrags
                       , wordsBits remain
                       )
  where
    wordsBits []     = []
    wordsBits (w:ws) = fmap (testBit w) [0..7] <> wordsBits ws

checkZeroPadding :: LoadTrees a ()
checkZeroPadding = do
    st <- get
    put (st { bits=[] })
    zeros <- go 0 st.bits
    when (zeros >= 64) do
        lift (throwE "getJelly: Too much zero-padding after frags")
  where
    go !n []           = pure (n :: Int)
    go _  (True : _)   = lift $ throwE "Junk data after frags"
    go !n (False : bs) = go (n+1) bs

getJelly
    :: (Show a, IsJelly a)
    => Vector a
    -> LoadTables a
getJelly pinz = do
    (leaves, numFrags, fragBits) <- getLeavesTable pinz

    finalSt <- lift $ flip execStateT (FRAG_ST leaves fragBits) do
                         frags <- replicateM_ numFrags getFrag
                         checkZeroPadding
                         pure frags

    let tab = finalSt.table
    let wid = length tab

    when (wid == 0) do
        lift (throwE "getJelly: No data")

    pure (tab ! (wid-1))

getFrag :: IsJelly a => LoadTrees a ()
getFrag = do
    res <- getPair
    st <- get
    put (st { table = snoc st.table res })
    pure ()

getPair :: IsJelly a => LoadTrees a a
getPair = _CONS <$> getTree <*> getTree

getTree :: IsJelly a => LoadTrees a a
getTree =
    getBit >>= \case
        True  -> getPair
        False -> getLeaf

bitsNum :: ∀a. (Num a, Bits a) => [Bool] -> a
bitsNum = go 0 . reverse -- lsb
  where
    go !n []         = n
    go !n (True:bs)  = go ((shiftL n 1) `setBit` 0) bs
    go !n (False:bs) = go (shiftL n 1) bs

getBit :: IsJelly a => LoadTrees a Bool
getBit =
    get >>= \case
        FRAG_ST _ []           -> lift $ throwE "EOF: getBit"
        FRAG_ST tab (b:remain) -> put (FRAG_ST tab remain) >> pure b

getBits :: Int -> LoadTrees a [Bool]
getBits num = do
    st <- get

    put (st { bits = drop num st.bits })

    let res = take num st.bits

    when (length res < num) do
        lift $ throwE ( "EOF: getBits: (read "
                     <> tshow (length res)
                     <> " but need "
                     <> tshow num
                      )

    pure res

getLeaf :: LoadTrees a a
getLeaf = do
    st <- get

    -- The table always has at least one thing at this point, so it is
    -- safe to decrement.
    let refs    = length st.table
    let maxRef  = refs - 1
    let leafWid = natBitWidth (fromIntegral maxRef)

    leaf <- bitsNum @Int <$> getBits leafWid

    if leaf < refs then
        pure (st.table ! leaf)
    else
        lift $ throwE "ref is out of bounds"


getVarBytes :: LoadTables ByteString
getVarBytes =
    get >>= \case
        [] -> do
            lift $ throwE "loadLen: EOF"

        0:xs -> do
            put xs
            pure mempty

        x:xs | x<128 -> do
            put xs
            pure (singleton x :: ByteString)

        x:xs | not (x `testBit` 6) -> do
            let len = fromIntegral (x `clearBit` 7)
            let result = take len xs
            when (length result /= len) do
                lift $ throwE "getVarBytes: EOF (short)"
            put (drop len xs)
            pure (pack result)

        x:xs -> do
            let lenLen = fromIntegral ((x `clearBit` 6) `clearBit` 7)
            let lenBytes = take lenLen xs

            when (length lenBytes /= lenLen) do
                lift $ throwE "EOF: loadLen (while getting length)"

            let len = bytesNat $ pack lenBytes
            let rest = drop lenLen xs

            when (len > fromIntegral (maxBound::Int)) do
                lift $ throwE "EOF: loadLen (unreasonable length)"

            let lenInt = fromIntegral len :: Int
            let resBytes = take lenInt rest
            when (length resBytes /= lenInt) do
                lift $ throwE "EOF: loadLen (while getting bytes)"

            put (drop lenInt rest)
            pure (pack resBytes)

loadLen :: LoadTables Int
loadLen = do
    bs <- getVarBytes
    let res = bytesNat bs
    when (res >= fromIntegral (maxBound :: Int)) do
        lift $ throwE ("loadLen: unreasonable: " <> tshow res)
    pure (fromIntegral res)

loadBar :: IsJelly a => LoadTables a
loadBar = _BAR <$> getVarBytes

loadNat :: IsJelly a => LoadTables a
loadNat = do
    nat <- bytesNat <$> getVarBytes
    pure $ case nat of
             NatS# w -> _WORD (W# w)
             _       -> _NAT nat
