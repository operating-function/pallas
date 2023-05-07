-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO Lazy hack, do better (w.r.t re-export list?)

module Fan.Eval
    ( Fan(..)
    , PrimopCrash(..)
    , Nat
    , Pin(..)
    , Law(..)
    , trueArity
    , lawNameText
    , fastValEq
    , natArity
    , isPin
    , LawName(..)
    , valName
    , valTag
    , boom
    , matchData
    , toNat
    , (%%)
    , (^)
    , executeLaw
    , compileLaw
    , mkPin
    , mkPin'
    , mkLaw
    , mkLawPreNormalized
    , appN
    , kloList
    , kloWalk
    , fanIdx
    , mkRow
    , evalArity
    , vTrkFan
    , vShowFan
    , vJetMatch
    , vCrashOnJetFallback
    , vWarnOnJetFallback
    , normalize
    , saveFanReference
    , saveFanPure
    , saveFan
    , trkName
    , loadPinFromBlob
    )
where

import Fan.Prof
import Fan.RunHashes
import Fan.Types
import PlunderPrelude            hiding (hash, (^))

import Control.Monad.Trans.State (State, execState, modify', runState)

import Control.Monad.ST (ST)
import Data.Bits        (xor)
import Data.Char        (isAlphaNum)
import Data.Vector      ((!))
import GHC.Prim         (reallyUnsafePtrEquality#)
import GHC.Word         (Word(..))
import Jelly.Types      (shortHex)
import Rex              (GRex)
import Unsafe.Coerce    (unsafeCoerce)

import {-# SOURCE #-} Fan.Save (getPinHash, saveFan)

import qualified Data.ByteString      as BS
import qualified Data.Char            as C
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as SV
import qualified GHC.Exts             as GHC
import qualified GHC.Natural          as GHC
import qualified GHC.Num.BigNat       as GHC
import qualified Jelly                as Jelly
import qualified Jelly.Reference      as Jelly
import qualified Rex


-- Infix Operators -------------------------------------------------------------


infixl 5 %%;


-- Globals ---------------------------------------------------------------------

-- These should all be overwritten on startup.  These exists to break
-- dependency cycles, and aren't intended to support dynamic changes.

vTrkFan :: IORef (Fan -> IO ())
vTrkFan = unsafePerformIO $ newIORef $ const $ pure ()

vShowFan :: IORef (Fan -> Text)
vShowFan = unsafePerformIO $ newIORef $ const "[PLUN]"

vJetMatch :: IORef (Pin -> IO Pin)
vJetMatch = unsafePerformIO (newIORef pure)

vCrashOnJetFallback :: IORef Bool
vCrashOnJetFallback = unsafePerformIO (newIORef False)

vWarnOnJetFallback :: IORef Bool
vWarnOnJetFallback = unsafePerformIO (newIORef True)


-- Types -----------------------------------------------------------------------

instance Jelly.IsJelly Fan where
    type JellyPin Fan = Pin

    _WORD = NAT . fromIntegral
    _NAT  = NAT
    _BAR  = BAR
    _CONS = (%%)
    _PIN  = PIN

instance Show LawName where
    show = either show show . natUtf8 . (.nat)

instance Show Law where
    show law = concat
               [ "(LAW "
               , unpack (ugly law.name.nat)
               , " "
               , show law.args
               , " "
               , show law.body
               , ")"
               ]

instance Eq Law where
    (==) x@(L n a b _) y@(L nn aa bb _) =
        case reallyUnsafePtrEquality# x y of
            1# -> True
            _  -> a==aa && n==nn && b==bb

{-# INLINE hashMix #-}
hashMix :: Int -> Int -> Int
hashMix h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE natQHash #-}
natQHash :: Nat -> Int
natQHash (GHC.NatS# w) = unsafeCoerce (W# w)
natQHash (GHC.NatJ# w) = unsafeCoerce (W# (GHC.bigNatToWord# (GHC.unBigNat w)))

{-# INLINE lawQHash #-}
lawQHash :: Law -> Int
lawQHash law =
    natQHash law.name.nat `hashMix`
    natQHash law.args `hashMix`
    quickHash law.body

hashSum :: [Fan] -> Int
hashSum = \case
    []     -> 0
    (x:xs) -> hashSum' (quickHash x) xs

hashSum' :: Int -> [Fan] -> Int
hashSum' !acc []     = acc
hashSum' !acc (a:as) = hashSum' (acc `hashMix` quickHash a) as

quickHash :: Fan -> Int
quickHash = \case
    NAT n    -> natQHash n
    PIN p    -> p.quik
    FUN l    -> lawQHash l
    KLO _ xs -> hashSum (toList xs)
    BAR b    -> length b
    ROW r    -> hashSum' (length r) (toList r)
    TAB r    -> hashSum' (length r) (keys r)
    CAb r    -> hashSum' (length r) (toList r)
    COw c    -> natQHash c
    REX{}    -> 5

-- TODO What if the small array has extra shit, and that shit can't
-- safely be forced?  Don't do `mapSmallArray`, do it by hand.
normalize :: Fan -> Fan
normalize top =
    if isNormal top then top else go top
  where
    isNormal (KLO _ xs) =
        case xs^0 of
            KLO{} -> False
            _     -> all isNormal (toList xs)
    isNormal (ROW v) = all isNormal v
    isNormal (TAB t) = all isNormal t
    isNormal !_      = True

    go tp = case tp of
        NAT !_ -> tp
        BAR !_ -> tp
        PIN !_ -> tp
        FUN !_ -> tp
        COw !_ -> tp
        CAb !_ -> tp
        REX r  -> REX (go <$> r)
        ROW r  -> ROW (go <$> r)
        TAB t  -> TAB (go <$> t)
        KLO r eRaw ->
            let e = mapSmallArray' go eRaw in
            case (e ^ 0) of
               KLO _ ee ->
                   let !w  = sizeofSmallArray e
                       !ww = sizeofSmallArray ee
                       len = ww + (w-1)
                   in
                       KLO r $ createSmallArray len (NAT 999) \a -> do
                                   copySmallArray a 0  ee 0 ww
                                   copySmallArray a ww e  1 (w-1)
               _ -> KLO r e

instance Show Pin where
  show pin = unpack (valName pin.item)

instance Eq Pin where
    (==) x y =
        case reallyUnsafePtrEquality# x y of
            1# -> True
            _  -> case x.quik == y.quik of
                      False -> False
                      True  -> unsafePerformIO do
                                   xHash <- getPinHash x
                                   yHash <- getPinHash y
                                   pure (xHash == yHash)

{-
    Comparision of two values that are likely to be pointer-equals.

    We always do this check on laws and pins, since they are big
    structures that are usually pointer-equals if they are equals.

    We don't want to do this on every single value equality check,
    though.  It's expensive.

    TODO: Test to see if we are actually gaining anything from this.

    TODO: Consider getting rid of this, it isn't a huge problem if
    laws comparisons are expensive, and pin comparisons just use the
    hash anyways.
-}
fastValEq :: Fan -> Fan -> Bool
fastValEq x y =
    case reallyUnsafePtrEquality# x y of
        1# -> True
        _  -> x == y

-- TODO Make sure evaluation order is correct.  Don't evaluate more or
-- less than formal impl.
instance Eq Fan where
    NAT n   == NAT m   = (n==m)
    PIN p   == PIN q   = (p==q)
    BAR b   == BAR d   = (b==d)
    ROW r   == ROW s   = (r==s)
    TAB t   == TAB u   = (t==u)
    CAb c   == CAb d   = (c==d)
    COw n   == COw m   = (n==m)
    FUN l   == FUN a   = (l==a)
    v@KLO{} == w@KLO{} = (kloWalk v == kloWalk w)
    REX x   == REX y   = x==y
    _       == _       = False

-- No point in checking (.quik), since that only tells us if neq.
instance Ord Pin where
    compare x y =
        case GHC.reallyUnsafePtrEquality x y of
            1# -> EQ
            _  -> unsafePerformIO do
                      xHash <- getPinHash x
                      yHash <- getPinHash y
                      pure $ if xHash == yHash
                             then EQ
                             else compare x.item y.item

{-
    TODO: This is pretty complicated. Test the shit out of this.

    TODO: Try to reduce code duplication between lawName/etc and `boom`.
        It's difficult because boom is very performance sensitive.

    TODO: This is going to need some explaination.
-}
instance Ord Fan where
    compare (NAT x) (NAT y) = compare x y
    compare (NAT _) _       = LT
    compare _       (NAT _) = GT

    compare (PIN x) (PIN y) = compare x y
    compare (PIN _) _       = LT
    compare _       (PIN _) = GT

    compare x y =
        case (fanLen x, fanLen y) of
            (0, 0) ->
                compare (lawName x) (lawName y)
             <> compare (lawArgs x) (lawArgs y)
             <> compare (lawBody x) (lawBody y)

            (xw, yw) ->
                compare xw yw
             <> concat (zipWith compare (fanSeq x) (fanSeq y))
      where
        fanLen (ROW r)   = length r
        fanLen (TAB t)   = length t
        fanLen (KLO _ k) = (sizeofSmallArray k) - 1
          -- ^ TODO: This is false if the fan is not normalized.  Not a
          -- safe assumption.
        fanLen _         = 0

        nat = fromIntegral

        fanSeq f = case f of
            NAT{}   -> [f]
            FUN{}   -> [f]
            BAR{}   -> [f]
            PIN{}   -> [f]
            CAb{}   -> [f]
            COw{}   -> [f]
            REX{}   -> [f]
            TAB t   -> if null t then [f] else CAb (keysSet t)     : (snd <$> M.toDescList t)
            ROW r   -> if null r then [f] else COw (nat(length r)) : reverse (toList r) -- COw here is never empty
            KLO _ k -> toList k

(^) :: SmallArray a -> Int -> a
(^) = indexSmallArray

{-# INLINE lawName #-}
lawName :: Fan -> Nat
lawName = \case
    FUN l -> l.name.nat
    NAT{} -> 0
    PIN{} -> 0
    KLO{} -> 0
    BAR{} -> 1
    ROW{} -> 0
    TAB{} -> 0
    CAb{} -> 0
    COw{} -> 0
    REX{} -> 82

{-# INLINE lawArgs #-}
lawArgs :: Fan -> Nat
lawArgs = \case
    FUN l -> l.args
    PIN p -> p.args
    BAR{} -> 1
    COw c -> c+1
    ROW r -> if null r then 1 else 0 -- Only a law if empty
    TAB t -> if null t then 1 else 0 -- Only a law if empty
    CAb c -> fromIntegral (length c + 1)
    REX{} -> 3
    KLO{} -> 0 -- Not a function
    NAT{} -> 0 -- Not a function

{-# INLINE lawBody #-}
lawBody :: Fan -> Fan
lawBody = \case
    FUN l -> l.body
    BAR b -> NAT (barBody b)
    TAB k -> if null k then ROW mempty else 0
    CAb k -> ROW $ fromList $ S.toAscList k
    REX r -> (0 %% (0 %% (0 %% rexNoun r %% 1) %% 2) %% 3)
    COw{} -> NAT 0 -- Actual law body is 0
    ROW{} -> NAT 0 -- Actual law body is 0
    NAT{} -> NAT 0 -- Not a law
    KLO{} -> NAT 0 -- Not a law
    PIN{} -> NAT 0 -- Not a law

boom :: Fan -> (Fan, Fan)
boom = \case
    NAT{} ->
        (NAT 0, NAT 0)

    FUN law ->
        rul law.name law.args law.body

    BAR b ->
        rul (LN 1) 1 (NAT $ barBody b)

    PIN p ->
        (NAT 4, p.item)

    COw n ->
        rul (LN 0) (n+1) (NAT 0)

    -- When we take the head of a closure with more than two elements,
    -- we essentially create a lazy-list of width=2 closure nodes.
    KLO arity xs ->
        case sizeofSmallArray xs of
          2   -> ( xs^0 , xs^1 )
          len -> ( let
                     flow !_ !0 = xs^0
                     flow !r !i = KLO (r+1) (a2 (flow (r+1) (i-1)) (xs^i))
                   in
                     flow arity (len-2)
                 , xs^(len-1)
                 )

    -- Builds lazy list of two-element KLO nodes.
    ROW row ->
        let !len = length row in
        case len of
            0 -> boom (COw 0)
            1 -> (COw 1, row!0)
            n -> ( let
                     flow !i !0   = COw (fromIntegral i) -- i is never 0
                     flow !i !ram = KLO i $ a2 (flow (i+1) (ram-1)) (row!i)
                   in
                     flow 1 (n-1)
                 ,
                   row!0
                 )

    -- Builds lazy list of two-element KLO nodes.
    TAB tab ->
        let !len = length tab
            !cab = CAb (M.keysSet tab)
        in
        case len of
            0 -> boom cab
            1 -> (cab, snd (M.findMin tab))
            _ -> let
                    (fir, rest) = case M.elems tab of
                                      []    -> error "impossible"
                                      (f:r) -> (f,r)

                    fill !_ []     = CAb (M.keysSet tab) -- never empty
                    fill !i (v:vs) = KLO i (a2 (fill (i+1) vs) v)
                 in
                 ( fill 1 rest
                 , fir
                 )

    CAb ks ->
        rul (LN 0)
            (fromIntegral (length ks + 1))
            (ROW $ fromList $ S.toAscList ks)

    REX rex ->
        rul (LN rTag) 3
            (0 %% (0 %% (0 %% rexNoun rex %% 1) %% 2) %% 3)

  where
    rTag = bytesNat (encodeUtf8 "R")

    rul :: LawName -> Nat -> Fan -> (Fan, Fan)
    rul (LN n) a b =
        ( KLO 1 (a3 (NAT 0) (NAT n) (NAT a))
        , b
        )

rexNoun :: GRex Fan -> Fan
rexNoun = \case
    Rex.C val ->
        rexEMBD %% val

    Rex.T key style text heir ->
        textStyleConstr style
            %% NAT key
            %% NAT (bytesNat $ encodeUtf8 text)
            %% goHeir heir

    Rex.N key style ryne sons heir ->
        nodeStyleConstr style
            %% NAT key
            %% NAT (bytesNat $ encodeUtf8 ryne)
            %% ROW (fromList (REX <$> sons))
            %% goHeir heir

  where

    goHeir = maybe 0 REX

    nodeStyleConstr = \case
        Rex.OPEN        -> rexOPEN
        Rex.NEST_PREFIX -> rexNEST
        Rex.NEST_INFIX  -> rexINFX
        Rex.SHUT_PREFIX -> rexPREF
        Rex.SHUT_INFIX  -> rexSHUT

    textStyleConstr = \case
        Rex.BARE_WORD -> rexWORD
        Rex.THIN_CORD -> rexCORD
        Rex.THIC_CORD -> rexTAPE
        Rex.THIN_LINE -> rexLINE
        Rex.THIC_LINE -> rexPAGE
        Rex.CURL_CORD -> rexCURL

    rexWORD,rexCORD,rexTAPE,rexLINE,rexPAGE,rexCURL :: Fan
    rexWORD = rexText "WORD"
    rexCORD = rexText "CORD"
    rexTAPE = rexText "TAPE"
    rexLINE = rexText "LINE"
    rexPAGE = rexText "PAGE"
    rexCURL = rexText "CURL"

    rexOPEN,rexNEST,rexINFX,rexPREF,rexSHUT :: Fan
    rexOPEN = rexNode "OPEN"
    rexNEST = rexNode "NEST"
    rexINFX = rexNode "INFX"
    rexPREF = rexNode "PREF"
    rexSHUT = rexNode "SHUT"

    -- WORD=(EMBD valu e l n ? e EMBD value)
    -- WORD=((0 "EMBD" 4) (0 (0 2 0) 1))
    rexEMBD =
       mkLaw "EMBD" 4 rexEmbdBody

    -- WORD=(WORD key text heir e l n ? l WORD key text heir)
    -- WORD=((0 "WORD" 6) (0 (0 (0 (0 5 0) 1) 2) 3)
    rexText :: LawName -> Fan
    rexText tag =
        mkLaw tag 6 rexLeafBody

    -- OPEN=(OPEN key rune sons heir e l n ? n OPEN key rune sons heir)
    -- OPEN=((0 "OPEN" 7) (((((7 0) 1) 2) 3) 4))
    rexNode :: LawName -> Fan
    rexNode tag =
        mkLaw tag 7 rexNodeBody

rexEmbdBody, rexLeafBody, rexNodeBody :: Fan
rexEmbdBody = normalize (0 %% (0 %% 2 %% 0) %% 1)
rexLeafBody = normalize (0 %% (0 %% (0 %% (0 %% 5 %% 0) %% 1) %% 2) %% 3)
rexNodeBody = normalize (0 %% (0 %% (0 %% (0 %% (0 %% 7 %% 0) %% 1) %% 2) %% 3) %% 4)

a2 :: a -> a -> SmallArray a
a2 p q = createSmallArray 2 p \a -> do
    writeSmallArray a 1 q

a3 :: a -> a -> a -> SmallArray a
a3 p q r = createSmallArray 3 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r

a4 :: a -> a -> a -> a -> SmallArray a
a4 p q r s = createSmallArray 4 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r
    writeSmallArray a 3 s

valName :: Fan -> Text
valName = \case
    FUN law -> ugul law.name.nat
    PIN pin -> valName pin.item
    _       -> "_"
 where
    ok '_' = True
    ok c   = C.isAlphaNum c

    ugul :: Nat -> Text
    ugul 0   = "anon"
    ugul nat = case natUtf8 nat of
        Right t | all ok t -> t
        _                  -> tshow nat

valTag :: Fan -> Nat
valTag (FUN law) = law.name.nat
valTag (PIN pin) = valTag pin.item
valTag _         = 0

kloList :: Fan -> [Fan]
kloList = reverse . kloWalk

kloWalk :: Fan -> [Fan]
kloWalk (KLO _ xs) = go (sizeofSmallArray xs - 1)
                       where
                         go !0 = kloWalk (xs^0)
                         go !i = (xs^i) : go (i-1)
kloWalk v          = [v]

instance Show Fan where
    show (NAT n)   = ugly n
    show (KLO _ x) = show (toList x)
    show (FUN l)   = show l
    show (PIN p)   = show p
    show (COw n)   = "R" <> show n
    show (ROW v)   = "(ROW " <> show v <> ")"
    show (TAB t)   = "(TAB " <> show (showTab t) <> ")"
    show (CAb k)   = "(CAB " <> show (toList k) <> ")"
    show (BAR b)   = "(BAR " <> show b <> ")"
    show (REX _)   = "(REX _)" -- TODO

showTab :: Map Fan Fan -> [(Fan,Fan)]
showTab t = mapToList t

-- Utilities -------------------------------------------------------------------

isPin :: Fan -> Bool
isPin PIN{} = True
isPin _     = False

lawNameText :: LawName -> Text
lawNameText (LN 0) = "_"
lawNameText (LN n) =
  case natUtf8 n of
    Left _  -> fallback
    Right t ->
      let cs = unpack t
      in if | all isNameChar cs -> t
            | otherwise         -> fallback
 where
  fallback = "_/" <> tshow n

  isNameChar '_' = True
  isNameChar c   = isAlphaNum c

instance IsString LawName where
  fromString = LN . bytesNat . encodeUtf8 . pack


-- Vector and Table constructors are given an evalArity of one less than
-- there actual arity so that they will be transformed into vectors/tables
-- once saturated.
--
-- Thus, to get the true arity, we need to actually do the scan and see
-- what's at the head.
trueArity :: Fan -> Nat
trueArity = \case
    COw 0          -> error "Should be jet matched as V0"
    COw n          -> succ $ fromIntegral n
    CAb t | null t -> error "Should be jet matched as T0"
    CAb n          -> succ $ fromIntegral (length n)
    KLO _ xs       -> trueArity (xs^0) - fromIntegral (sizeofSmallArray xs - 1)
    FUN l          -> l.args
    NAT n          -> natArity n
    PIN p          -> p.args
    ROW _          -> 1
    TAB _          -> 1
    BAR _          -> 1
    REX _          -> 3

{-# INLINE natArity #-}
natArity :: Nat -> Nat
natArity (GHC.NatS# 0##) = 3 -- FUN
natArity (GHC.NatS# 1##) = 5 -- CAS
natArity (GHC.NatS# 2##) = 3 -- DEC
natArity _               = 1 -- INC, PIN, etc

evalArity :: Fan -> Int
evalArity (FUN l) = natToArity l.args
evalArity (NAT n) = case n of GHC.NatS# 0## -> 3
                              GHC.NatS# 1## -> 5
                              GHC.NatS# 2## -> 3
                              _             -> 1
evalArity (KLO r _) = r
evalArity (PIN p)   = natToArity p.args
evalArity (CAb t)   = if null t then error "Should be jet matched as T0"
                                else length t
evalArity (COw 0)   = error "Should be jet matched as V0"
evalArity (COw n)   = natToArity n
evalArity (ROW _)   = 1
evalArity (TAB _)   = 1
evalArity (BAR _)   = 1
evalArity (REX _)   = 3

--------------------------------------------------------------------------------

barBody :: ByteString -> Nat
barBody bytes =
    -- TODO Make this not slow
    bytesNat (bytes <> BS.singleton 1)

--------------------------------------------------------------------------------

matchData :: LawName -> Nat -> Fan -> Maybe Fan
matchData (LN 0)  1 (NAT 0) = Just $ ROW (fromList [])
matchData (LN 0)  n (NAT 0) = Just $ COw (n-1) -- n-1 is never zero
matchData (LN 0)  n (ROW v) = matchCab n v
matchData (LN 1)  1 (NAT n) = matchBar n
matchData (LN 82) 3 body    = REX <$> matchRex body
matchData (LN _)  _ _       = Nothing

matchBar :: Nat -> Maybe Fan
matchBar n = do
    guard (n /= 0)
    let bitWidth = (natBitWidth n :: Nat) - 1
    guard (0 == (bitWidth `mod` 8))
    let bytWidth = fromIntegral (bitWidth `div` 8)
    pure $ BAR $ take bytWidth $ natBytes n

matchRex :: Fan -> Maybe Rex
matchRex kabc = do
    -- traceM "matchRex.kabc"
    kab <- case readKlo kabc of
             Just [NAT 0, kab, NAT 3] -> Just kab
             _                        -> Nothing

    -- traceM "matchRex.kab"
    ka <- case readKlo kab of
            Just [NAT 0, ka, NAT 2] -> pure ka
            _                       -> Nothing

    -- traceM "matchRex.ka"
    k <- readKlo ka >>= \case
           [NAT 0, k, NAT 1] -> pure k
           _                 -> Nothing

    -- traceM "matchRex.k"
    readKlo k >>= matchRexHead
  where
    readKlo (KLO _ r) = pure (toList r)
    readKlo _         = Nothing

matchRexHead :: [Fan] -> Maybe Rex
matchRexHead =
   \fans -> do
       -- traceM "matchRexHead"
       case go fans of
           Nothing -> do
               _ <- traceM (unlines ("matchRexHead(NOT OK)\n" : fmap show fans))
               Nothing
           Just res -> do
               -- traceM "matchRexHead(OK)"
               pure res
  where
    go = \case
     [FUN embd, valu] -> do
         () <- readEmbd embd
         pure (Rex.C valu)

     [FUN l, i, t, h] -> do
         styl <- readLeaf l
         idnt <- readIdnt i
         text <- readText t
         heir <- readHeir h
         pure (Rex.T idnt styl text heir)

     [FUN n, i, r, s, h] -> do
         idnt <- readIdnt i
         styl <- readNode n
         rune <- readRune r
         sons <- readSons s
         heir <- readHeir h
         pure (Rex.N idnt styl rune sons heir)

     _ -> do
         Nothing

    -- EMBD=(EMBD valu e l n ? e EMBD valu)
    -- EMBD=((0 "EMBD" 4) (0 (0 2 0) 1)
    readEmbd :: Law -> Maybe ()
    readEmbd law = do
        guard (law.args == 4)
        guard (law.name == "EMBD")
        guard (law.body == rexEmbdBody)
        pure ()

    -- WORD=(WORD key text heir r l n ? l WORD key text heir)
    -- WORD=((0 "WORD" 6) (0 (0 (0 (0 5 0) 1) 2) 3)
    readLeaf :: Law -> Maybe Rex.TextShape
    readLeaf law = do
        guard (law.args == 6)
        guard (law.body == rexLeafBody)
        case law.name of
             "WORD" -> Just Rex.BARE_WORD
             "CORD" -> Just Rex.THIN_CORD
             "TAPE" -> Just Rex.THIC_CORD
             "LINE" -> Just Rex.THIN_LINE
             "PAGE" -> Just Rex.THIC_LINE
             "CURL" -> Just Rex.CURL_CORD
             _      -> Nothing

    -- OPEN=(OPEN key rune sons heir r l n ? n OPEN key rune sons heir)
    -- OPEN=((0 "OPEN" 7) (((((7 0) 1) 2) 3) 4))
    readNode :: Law -> Maybe Rex.RuneShape
    readNode law = do
        guard (law.args == 7)
        guard (law.body == rexNodeBody)
        case law.name of
            "OPEN" -> Just Rex.OPEN
            "NEST" -> Just Rex.NEST_PREFIX
            "INFX" -> Just Rex.NEST_INFIX
            "PREF" -> Just Rex.SHUT_PREFIX
            "SHUT" -> Just Rex.SHUT_INFIX
            _      -> Nothing

    readHeir (NAT 0) = Just Nothing
    readHeir (REX x) = Just (Just x)
    readHeir _       = Nothing

    readIdnt (NAT n) = pure n
    readIdnt _       = Nothing

    -- TODO Make sure it's valid text (no decodeLenient nonsense)
    readText (NAT t) = pure (decodeUtf8 $ natBytes t)
    readText _       = Nothing

    -- TODO Make sure it's a valid rune
    readRune (NAT t) = pure (decodeUtf8 $ natBytes t)
    readRune _       = Nothing

    readSons (ROW xs) = traverse readRex (toList xs)
    readSons _        = Nothing

    readRex (REX x) = pure x
    readRex _       = Nothing

-- #? (WORD idnt text heir e l n) #| l WORD idnt text heir
-- #? (CORD idnt text heir e l n) #| l CORD idnt text heir
-- #? (TAPE idnt text heir e l n) #| l TAPE idnt text heir
-- #? (LINE idnt text heir e l n) #| l LINE idnt text heir
-- #? (PAGE idnt text heir e l n) #| l PAGE idnt text heir
-- #? (CURL idnt text heir e l n) #| l CURL idnt text heir

-- #? (OPEN idnt rune sons heir e l n) #| n OPEN idnt rune sons heir
-- #? (NEST idnt rune sons heir e l n) #| n NEST idnt rune sons heir
-- #? (INFX idnt rune sons heir e l n) #| n INFX idnt rune sons heir
-- #? (PREF idnt rune sons heir e l n) #| n PREF idnt rune sons heir
-- #? (SHUT idnt rune sons heir e l n) #| n SHUT idnt rune sons heir


matchCab :: Nat -> Vector Fan -> Maybe Fan
matchCab arity vs = do
    guard (arity == (1 + fromIntegral (length vs)))
    case toList vs of
        []   -> TAB <$> pure mempty
        a:es -> CAb <$> collect mempty a es -- nevery empty
  where
    collect !acc i []           = pure (insertSet i acc)
    collect !acc i (w:ws) | w>i = collect (insertSet i acc) w ws
    collect _    _ _            = Nothing


-- Constructing Pins and Laws --------------------------------------------------

mkLawPreNormalized :: LawName -> Nat -> Fan -> Fan
mkLawPreNormalized nam arg bod = sel
  where
    prg = compileLaw nam arg bod
    law = L nam arg bod prg
    sel =
        if arg==0
          then
            let res = executeLaw res prg prg
                    $ createSmallArray 1 res \_ -> do
                          pure ()
            in res
          else
            fromMaybe (FUN law) $ matchData nam arg bod

mkLaw :: LawName -> Nat -> Fan -> Fan
mkLaw nam arg bod = mkLawPreNormalized nam arg (normalize bod)

mkPin :: Fan -> Fan
mkPin = PIN . unsafePerformIO . mkPin'

frameSize :: Fan -> Int
frameSize (KLO _ e) = frameSize (e^0)
frameSize v         = 1 + evalArity v

{-
        These are called extremely often, and we don't want to bog down
        the system by emiting profiling events fror them.
-}
highFreqLaws :: Set Nat
highFreqLaws = setFromList
    (
        [ "dec", "add", "mul", "sub", "bex", "lte", "lth", "div"
        , "mod", "aeq", "lsh", "rsh", "met", "mix", "dis", "con"
        , "if", "ifNot", "eql", "trk", "idx", "get", "len"
        , "weld", "map", "put", "mut", "take", "drop", "cat", "rev"
        , "w32", "add32", "mul32", "div32", "and32", "or32", "xor32"
        , "lsh32", "rsh32", "sub32", "ror32", "rol32", "isBar", "barIdx"
        , "barWeld", "barCat", "barFlat", "natBar", "barDrop", "barTake"
        , "barLen", "barNat", "cabSingleton", "cabIns", "cabDel", "cabMin"
        , "cabLen", "cabUnion", "cabHas", "cabSplitAt", "cabSplitLT"
        , "cabIntersection", "tabSingleton", "tabIdx", "tabElem"
        , "tabLookup", "tabToPairs", "gth", "gte", "bit", "not", "and"
        , "neq", "isNat"
        ] :: [Nat]
    )

addProfilingToPin :: Pin -> IO Pin
addProfilingToPin pin = do
    enab <- lawProfilingEnabled

    let shouldProfile =
            case pin.item of
                FUN l -> not $ member l.name.nat highFreqLaws
                _     -> False

    if not (enab && shouldProfile) then do
        pure pin
    else do
        hash <- getPinHash pin
        let nam = encodeUtf8 (valName pin.item)
        let key = nam <> "-(" <> shortHex hash <> ")"
        pure (setExec (profWrap key pin.exec) pin)
  where
    profWrap tag fun args =
        seq args $ unsafePerformIO do
            withSimpleTracingEvent tag "fan" $ evaluate (fun args)


mkPin' :: Fan -> IO Pin
mkPin' inp = do
    -- let putNam tx = putStrLn ("\n\n==== [[[" <> tx <> "]]] ====\n")
    -- putNam (valName inp)

    item  <- evaluate (normalize inp)
    match <- readIORef vJetMatch

    res <- mdo let exe = pinExec (PIN res) item
               let qik = quickHash item
               let ari = trueArity item
               dag <- newIORef Nothing
               res <- addProfilingToPin =<< match (P dag qik ari item exe)
               pure res

    -- hack that causes functions to be serialized/hashed immediately.
    case item of
      FUN{} -> evaluate res.exec >> pure ()
      _     -> pure ()

    {-
        We do not do deduplication here.  Instead, we should deduplicate
        the heap occasionally, after each snapshot.

        Heap deduplication as a pass is relatively cheap, because we
        are merely walking the pin-DAG and looking at the hashes.

        If this were ever of significant cost, we can have a flag on
        each thing to mark it as the canonical version.
    -}
    evaluate res

loadPinFromBlob :: DagInfo -> Fan -> IO Pin
loadPinFromBlob dagInfo item = do
    match <- readIORef vJetMatch

    res <- mdo let slf = (PIN res)
               let exe = case item of
                             FUN law -> \e -> executeLaw slf law.code law.code e
                             _       -> \e -> foldl' (%%) item (toList e)
                                           -- TODO is this correct?
                                           -- Doesn't the passed environment
                                           -- include ourselves?
               let qik = quickHash item
               let !ari = trueArity item
               dag <- newIORef (Just dagInfo)
               res <- addProfilingToPin =<< match (P dag qik ari item exe)
               pure res

    evaluate res.exec

    pure res

{-# INLINE pinExec #-}
pinExec :: Fan -> Fan -> (SmallArray Fan -> Fan)
pinExec self = \case
    FUN law -> executeLaw self law.code law.code
    item    -> foldl' (%%) item . toList


-- Evaluation ------------------------------------------------------------------


natToArity :: Nat -> Int
natToArity !n =
    if n>fromIntegral(maxBound::Int)
    then maxBound
    else fromIntegral n

(%%) :: Fan -> Fan -> Fan
(%%) = app2

data APPLY = APPLY (Int, Int) [Fan]
  deriving (Show)

app2 :: Fan -> Fan -> Fan
app2 f x =
    case evalArity f of
        1    -> eval2 f x
        args -> KLO (args-1) (a2 f x)

app3 :: Fan -> Fan -> Fan -> Fan
app3 f x y =
    case evalArity f of
        1    -> app2 (eval2 f x) y
        2    -> eval3 f x y
        args -> KLO (args-2) (a3 f x y)

app4 :: Fan -> Fan -> Fan -> Fan -> Fan
app4 f x y z =
    case evalArity f of
        1    -> app3 (eval2 f x) y z
        2    -> app2 (eval3 f x y) z
        3    -> eval4 f x y z
        args -> KLO (args-3) (a4 f x y z)

appN :: SmallArray Fan -> Fan
appN xs =
    case sizeofSmallArray xs of
       2 -> app2 (xs^0) (xs^1)
       3 -> app3 (xs^0) (xs^1) (xs^2)
       4 -> app4 (xs^0) (xs^1) (xs^2) (xs^3)
       !wid ->
            let !arity = evalArity (xs^0)
                !need  = arity+1
            in
            -- trace (ppShow $ APPLY (wid,need) (toList xs))
            case compare wid need of
                EQ -> evalN (KLO 0 xs)
                LT -> KLO (need-wid) xs
                GT -> let
                          !hed = evalN $ KLO 0 (cloneSmallArray xs 0 need)
                          !xtr = wid - need
                      in
                          appN $ createSmallArray (xtr+1) hed \buf -> do
                                     copySmallArray buf 1 xs need xtr

cow :: Nat -> Fan
cow 0 = ROW mempty
cow n = COw n -- never zero

execFrame :: SmallArray Fan -> Fan
execFrame buf =
    let x = buf^0 in
    case x of
        FUN l -> executeLaw x l.code l.code buf
        PIN p -> p.exec buf
        ROW v -> cow (fromIntegral $ length v)
        KLO{} -> error "Invalid stack frame, closure as head"
        NAT n -> execNat n buf
        BAR b -> if null b then buf^1 else NAT (barBody b)
        REX b -> rexNoun b %% (buf^1) %%(buf^2) %%(buf^3)
        TAB t -> ROW $ fromList $ M.elems t
        COw n ->
            let !las = fromIntegral n in
            ROW $ V.generate (fromIntegral n) \i ->
                      (buf ^ (las - i))


        CAb ks -> TAB $ M.fromDistinctAscList (zip keyList valList)
          where
            getV 0 = []
            getV n = (buf^n) : getV (n-1)

            valList = getV (length ks)
            keyList = S.toAscList ks

eval2 :: Fan -> Fan -> Fan
eval2 fn x1 =
    case fn of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a2 k x1)
               func  -> let !w = sizeofSmallArray x in
                        valCode func $ createSmallArray (w+1) x1 \buf -> do
                                           copySmallArray buf 0 x 0 w
        _ ->
            valCode fn (a2 fn x1)

eval3 :: Fan -> Fan -> Fan -> Fan
eval3 fn x1 x2 =
    case fn of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a3 k x1 x2)
               func  -> let !w = sizeofSmallArray x in
                        valCode func $ createSmallArray (w+2) x2 \buf -> do
                                           copySmallArray buf 0 x 0 w
                                           writeSmallArray buf w     x1
        _ ->
            valCode fn (a3 fn x1 x2)

eval4 :: Fan -> Fan -> Fan -> Fan -> Fan
eval4 fn x1 x2 x3 =
    case fn of
        k@(KLO _ x) ->
            case x^0 of
               KLO{} -> evalN (KLO 0 $ a4 k x1 x2 x3)
               func  -> let !w = sizeofSmallArray x in
                        valCode func $ createSmallArray (w+3) x3 \buf -> do
                                           copySmallArray buf 0 x 0 w
                                           writeSmallArray buf w     x1
                                           writeSmallArray buf (w+1) x2
        _ ->
            valCode fn (a4 fn x1 x2 x3)



-- For example, to eval (f x y) do `evalN (KLO 0 3 [f,x,y])`.
evalN :: Fan -> Fan
evalN env =
    -- trace ("evalN: " <> show env)
    -- trace ("evalN: " <> show (frameSize env))
    execFrame $ createSmallArray (frameSize env) (NAT 0) \a -> do
                    void (fill a env)
  where
    fill :: ∀s. SmallMutableArray s Fan -> Fan -> ST s Int
    fill buf = \case
        KLO _ e -> do
            !i <- fill buf (e^0)
            let !w = sizeofSmallArray e
            let !v = w-1
            copySmallArray buf i e 1 v
            pure (i+v)
        hed ->
            writeSmallArray buf 0 hed $> 1

deriving instance Eq PrimopCrash
deriving instance Ord PrimopCrash
deriving instance Show PrimopCrash
instance Exception PrimopCrash where
    displayException (PRIMOP_CRASH n x) =
        unsafePerformIO do
            s <- readIORef vShowFan
            pure $ concat [ "Evaluation crashed by calling the number "
                          , show n
                          , " with this argument:\n\n"
                          , unpack (s x)
                          ]

execNat :: Nat -> SmallArray Fan -> Fan
execNat 0 e = mkLaw (LN $ toNat $ e^1) (toNat $ e^2) (e^3)
execNat 1 e = wut (e^1) (e^2) (e^3) (e^4) (e^5)
execNat 2 e = case toNat (e^3) of 0 -> e^1
                                  n -> (e^2) %% NAT(n-1)
execNat 3 e = NAT (toNat(e^1) + 1)
execNat 4 e = mkPin (e^1)
execNat n e = unsafePerformIO do
    let arg = (e^1)
    evaluate (force arg) -- If arg crashes, throw that instead
    Fan.Prof.recordInstantEvent "crash" "fan" $
        M.singleton "op" (Right $ tshow n)
    throwIO (PRIMOP_CRASH n arg)

wut :: Fan -> Fan -> Fan -> Fan -> Fan -> Fan
wut p l a n = \case
    x@NAT{} -> n %% x
    x@KLO{} -> let (hd,tl) = boom x in app3 a hd tl

    PIN pin -> p %% pin.item

    FUN law ->
        let nm = NAT law.name.nat
            ar = NAT law.args
        in app4 l nm ar law.body

    BAR b -> app4 l (NAT 1) (NAT 1) (NAT $ barBody b)
    COw m -> wutCow m

    v@(TAB t) ->
        if null t then wutCab mempty else
        let (hd,tl) = boom v
        in app3 a hd tl

    CAb k -> wutCab k

    x@(ROW v) ->
        if null v
        then wutCow 0
        else app3 a h t where (h,t) = boom x

    x@REX{} ->
        app4 l (NAT $ bytesNat $ encodeUtf8 "R") 3 (snd $ boom x)
  where
    wutCow m = app4 l (NAT 0) (NAT (m+1)) (NAT 0)

    wutCab k =
        let args = NAT $ fromIntegral(length k) + 1
            keyz = ROW $ fromList $ S.toAscList k
        in app4 l (NAT 0) args keyz

{-
    PIN p -> rul (LN 0) args (pinBody args p.item)
               where args = (trueArity p.item)
    ROW v -> case reverse (toList v) of
                    []   -> rul (LN 0) 1 (AT 0)
                    x:xs -> apple (DAT $ COw sz) (x :| xs)
               where sz = fromIntegral (length v)
    TAB d -> tabWut d
    BAR b -> rul (LN 1) 1 (AT $ barBody b)
    COw n -> rul (LN 0) (n+1) (AT 0)
    CAB k -> cabWut k
-}

{-
    -- DAT dj    -> dataWut goLaw goApp dj
      -- where
        -- goApp g y      = a %% g %% y
        -- goLaw nm ar bd = f %% NAT (lawNameNat nm) %% NAT ar %% bd

dataWut
    :: ∀a
     . (LawName -> Nat -> Pln -> a)
    -> (Pln -> Pln -> a)
    -> Dat
    -> a
dataWut rul cel = \case
-}


cnsName :: Fan -> String
cnsName v =
  let res = valName v
  in if (null res || any (not . C.isPrint) res)
     then show v
     else unpack res

showCns :: Fan -> String
showCns v@KLO{}  = "(KLO " <> intercalate " " (showCns <$> kloList v) <> ")"
showCns v@FUN{}  = cnsName v
showCns v@PIN{}  = cnsName v
showCns (ROW xs) = "(row " <> intercalate " " (fmap showCns xs) <> ")"
showCns COw{}    = "COW"
showCns TAB{}    = "TAB"
showCns CAb{}    = "CAB"
showCns BAR{}    = "BAR"
showCns REX{}    = "REX"
showCns (NAT n)  = show n

instance Show Prog where
    show p = "(PROG "
          <> "{ arity="  <> show p.arity
          <> ", stkSz="  <> show p.stkSz
          <> ", prgrm=(" <> show p.prgrm <> ")"
          <> "})"

instance Show Run where
    show (CNS c) = showCns c
    show (ARG i) = "_" <> show i
    show (VAR i) = "_v" <> show i
    show (KAL xs) = "(KAL " <> intercalate " " (show <$> toList xs) <> ")"

    show (EXE _ _ f xs) =
        "(EXE " <> showCns f <> " " <> intercalate " " (show <$> toList xs) <> ")"

    show (PAR n xs) =
         "(PAR arity" <> show n <> intercalate " " (show <$> toList xs) <> ")"

    show (REC xs) = "(REC " <> intercalate " " (show <$> toList xs) <> ")"

    show (TRK v b) = "(TRK " <> show v <> " " <> show b <> ")"

    show (MK_ROW es) = "(MKROW " <> intercalate " " (show <$> es) <> ")"

    show (MK_TAB es) = "(MKTAB " <> intercalate " " (show <$> mapToList es) <> ")"

    show (LET i x v) =
        "(LET " <> show (VAR i) <> " " <> show x <> " " <> show v <> ")"

    show (IF_ c t e) =
        "(IF " <> show c <> " " <> show t <> " " <> show e <> ")"

    show (SEQ x b) =
        "(SEQ " <> show x <> " " <> show b <> ")"

    show (SWI x f v) =
        parencalate ["ROW_SWITCH", show x, show f, show v]

    show (JMP_WORD x f ks vs) =
        parencalate ["TAB_SWITCH_WORDS", show x, show f, show v]
      where
        v = mapFromList (zip (toList ks) (toList vs)) :: Map Word Run

    show (LAZ exe arg) =
        parencalate ("LAZ" : show exe : (show <$> toList arg))

    show (JMP x f v) =
        parencalate ["TAB_SWITCH", show x, show f, show v]

    show (OP2 name _ a b) =
        parencalate ["OP2", show name, show a, show b]

parencalate :: [String] -> String
parencalate xs = "(" <> intercalate " " xs <> ")"

{-
    -   TODO Optimize tail recursion (no call overhead at all, just
        overwrite the arguments and re-enter the code).

    -   TODO: Calling convention should just be `SmallArray Fan`

        we should execute directly against that.  If there are let
        bindings, then lawExec should allocate it's own buffer for them
        (separate REF into VAR and ARG)

        This means that all functions without LET bindings (including
        jets) just execute directly against the closure environment.

        A thunk is then represented in the runtime system as `(exec env)`.

        We don't have to care about overflow when converting to `Int`.
        If the arity is that big, this will never be run.

    TODO What about partial applications?  Like we want (trk "hi")
         to match, not just `(trk x y)`.

         As a nonsense example, I think (if 1 a b) will not optimize
         with this code.
-}
optimizeSpine :: Run -> Run
optimizeSpine = go
  where
    tryMatchSwitch exe r =
        case r^2 of
            MK_ROW vrun ->
                SWI (go $ r^0)
                    (go $ r^1)
                    (go <$> (smallArrayFromList $ toList vrun))
            _ -> exe

    isWord (NAT (GHC.NatS# _)) = True
    isWord _                   = False

    mkJumpTable :: Run -> Run -> Map Fan Run -> Run
    mkJumpTable key fal tab =
        JMP_WORD key fal jmpKeys jmpVals
      where
        jmpKeys = fromList (fst <$> pairs)
        jmpVals = smallArrayFromList (snd <$> pairs)
        pairs   = wordify (mapToList tab)

        wordify :: [(Fan, Run)] -> [(Word, Run)]
        wordify []                            = []
        wordify ((NAT (GHC.NatS# k), v) : vs) = (GHC.W# k, v) : wordify vs
        wordify ((_,                 _) : _ ) = error "no good"

    tryMatchTabSwitch exe r = case r^2 of
        MK_TAB vrun ->
            if all isWord (keys vrun) && (M.size vrun < 100) then
                mkJumpTable (go (r^0))
                            (go (r^1))
                            (go <$> vrun)
            else
                JMP (go (r^0))
                    (go (r^1))
                    (go <$> vrun)
        _ -> exe

    -- We are on the law spine, so everything we see is demanded, safe
    -- to replace calls to functions like `if` with control flow like `IF_`
    go = \case
        exe@(EXE _ _ (PIN p) r) ->
            unsafePerformIO $
            getPinHash p <&> \haz ->
            if | haz == ifHash        -> IF_ (go(r^0)) (go(r^1)) (go(r^2))
               | haz == ifNotHash     -> IF_ (go(r^0)) (go(r^2)) (go(r^1))
               | haz == switchHash    -> tryMatchSwitch exe r
               | haz == tabSwitchHash -> tryMatchTabSwitch exe r
               | haz == seqHash       -> SEQ (go(r^0)) (go(r^1))
               | haz == trkHash       -> TRK (go(r^0)) (go(r^1))
               | otherwise            -> exe
        LET i v b            -> LET i (goLazy v) (go b)
        exe                  -> goLazy exe

    -- We are no longer on the spine, the expressions that we are looking
    -- at may not be demanded.
    goLazy run =
        case run of
            -- These branches are only introduced by this pass, if we
            -- see them in the input, something has run afowl.
            IF_{}      -> error "impossible"
            SWI{}      -> error "impossible"
            JMP{}      -> error "impossible"
            JMP_WORD{} -> error "impossible"
            SEQ{}      -> error "impossible"
            LAZ{}      -> error "impossible"
            TRK{}      -> error "impossible"

            CNS{} -> run
            ARG{} -> run
            VAR{} -> run

            OP2 f o x y -> OP2 f o (goLazy x) (goLazy y)

            REC vs    -> REC (goLazy <$> vs)
            KAL vs    -> KAL (goLazy <$> vs)
            PAR r vs  -> PAR r (goLazy <$> vs)
            LET i v b -> LET i (goLazy v) (goLazy b)
            MK_ROW xs -> MK_ROW (goLazy <$> xs)
            MK_TAB xs -> MK_TAB (goLazy <$> xs)

            -- If we see something that we want to turn into control
            -- flow, we can't do that safely on the main spine, because it
            -- may not be demanded.  However, if we shatter it into a
            -- broken-out sub-spine, we can optimize that and then run
            -- it with `LAZ`.
            EXE x s (PIN p) r ->
                unsafePerformIO $
                getPinHash p <&> \haz ->
                if | haz == ifHash        -> shatterIt x s p r
                   | haz == ifNotHash     -> shatterIt x s p r
                   | haz == switchHash    -> shatterIt x s p r
                   | haz == tabSwitchHash -> shatterIt x s p r
                   | haz == seqHash       -> shatterIt x s p r
                   | haz == trkHash       -> shatterIt x s p r
                   | otherwise            -> EXE x s (PIN p) (goLazy <$> r)

            EXE x s f r -> EXE x s f (goLazy <$> r)

      where

        shatterIt x s p r =
            if runSize (EXE x s (PIN p) r) < 16 then
                EXE x s (PIN p) (goLazy <$> r)
            else
                let (pro, arg) = shatterSpine run
                    res = LAZ (pro { prgrm = go pro.prgrm })
                              (goLazy <$> arg)
                in res -- trace (ppShow res) res

runSize :: Run -> Int
runSize = w
  where
    w = \case
        CNS{}            -> 1
        ARG{}            -> 1
        VAR{}            -> 1
        LET _ x y        -> 1 + w x + w y
        IF_ c t e        -> 1 + w c + w t + w e
        EXE _ _ _ x      -> 1 + sum (w <$> x)
        OP2 _ _ x y      -> 1 + w x + w y
        SWI c f v        -> 1 + w c + w f + sum (w <$> v)
        JMP c f v        -> 1 + w c + w f + sum (w <$> v)
        JMP_WORD c f _ v -> 1 + w c + w f + sum (w <$> v)
        SEQ x y          -> 1 + w x + w y
        REC xs           -> 1 + sum (w <$> xs)
        KAL xs           -> 1 + sum (w <$> xs)
        PAR _ xs         -> 1 + sum (w <$> xs)
        TRK x y          -> 1 + w x + w y
        MK_ROW xs        -> 1 + sum (w <$> xs)
        MK_TAB xs        -> 1 + sum (w <$> xs)
        LAZ _ arg        -> 1 + sum (w <$> arg)

runFree :: Run -> [Either Int Int]
runFree =
    \top -> toList (execState (go mempty top) mempty)
  where
    go :: Set (Either Int Int) -> Run -> State (Set (Either Int Int)) ()
    go z = \case
        ARG 0            -> pure () -- self reference
        ARG v            -> addRef (Left v)
        VAR v            -> addRef (Right v)
        CNS{}            -> pure ()
        SEQ a b          -> go z a >> go z b
        TRK a b          -> go z a >> go z b
        KAL xs           -> traverse_ (go z) xs
        PAR _ xs         -> traverse_ (go z) xs
        REC xs           -> traverse_ (go z) xs
        MK_ROW xs        -> traverse_ (go z) xs
        MK_TAB xs        -> traverse_ (go z) xs
        LAZ _prg xs      -> traverse_ (go z) xs
        SWI c f v        -> go z c >> go z f >> traverse_ (go z) v
        JMP c f v        -> go z c >> go z f >> traverse_ (go z) v
        JMP_WORD c f _ v -> go z c >> go z f >> traverse_ (go z) v
        EXE _ _ _ r      -> traverse_ (go z) r
        OP2 _ _ a b      -> go z a >> go z b
        IF_ i t e        -> go z i >> go z t >> go z e
        LET i v b        -> do let z' = insertSet (Right i) z
                               go z' v
                               go z' b
      where
        addRef :: Either Int Int -> State (Set (Either Int Int)) ()
        addRef x = unless (x `member` z) (modify' $ insertSet x)


-- TODO: BADLY need to switch to SYB or similar here.  This is absurd.
spineFragment :: Map (Either Int Int) Int -> Run -> (Run, Int)
spineFragment freeTable =
    \top -> runState (go (0,mempty) top) 0
  where
    go :: (Int, Map Int Int) -> Run -> State Int Run
    go z@(next, loc) = \case
        CNS c            -> pure (CNS c)
        SEQ a b          -> SEQ <$> go z a <*> go z b
        TRK a b          -> TRK <$> go z a <*> go z b
        KAL xs           -> KAL <$> traverse (go z) xs
        PAR r xs         -> PAR r <$> traverse (go z) xs
        REC xs           -> REC <$> traverse (go z) xs
        MK_ROW xs        -> MK_ROW <$> traverse (go z) xs
        MK_TAB xs        -> MK_TAB <$> traverse (go z) xs
        LAZ prg xs       -> LAZ prg <$> traverse (go z) xs
        SWI c f v        -> SWI <$> go z c <*> go z f <*> traverse (go z) v
        JMP c f v        -> JMP <$> go z c <*> go z f <*> traverse (go z) v
        JMP_WORD c f k v -> JMP_WORD <$> go z c <*> go z f <*> pure k <*> traverse (go z) v
        OP2 f o a b      -> OP2 f o <$> go z a <*> go z b
        EXE x s f xs     -> EXE x s f <$> traverse (go z) xs
        IF_ i t e        -> IF_ <$> go z i <*> go z t <*> go z e
        LET i v b        -> do let z' = (next+1, insertMap i next loc)
                               modify' (max next)
                               LET next <$> go z' v <*> go z' b

        ARG 0 -> pure (ARG 0)
        ARG x -> pure $ ARG
                      $ fromMaybe (error "impossible")
                      $ lookup (Left x)
                      $ freeTable

        VAR x -> case (lookup (Right x) freeTable, lookup x loc) of
                   (Just f, _) -> pure (ARG f)
                   (_, Just l) -> pure (VAR l)
                   _           -> error "impossible"

shatterSpine :: Run -> (Prog, SmallArray Run)
shatterSpine top =
    (PROG{arity,stkSz,prgrm}, args)
  where
    args           = smallArrayFromList (either ARG VAR <$> freeRefs)
    freeRefs       = runFree top
    argsMap        = mapFromList (zip freeRefs [1..]) -- arg 0 is self
    (prgrm, nmVar) = spineFragment argsMap top
    stkSz          = nmVar + arity + 1
    arity          = length argsMap

-- Match row/tab constructors: (c2 y x) -> MK_ROW [x,y]
matchConstructors :: Run -> Run
matchConstructors = go
  where
    go = \case
        LAZ{}               -> error "impossible"
        SEQ v x             -> SEQ (go v) (go x)
        REC vs              -> REC (go <$> vs)
        KAL vs              -> KAL (go <$> vs)
        PAR r vs            -> PAR r (go <$> vs)
        TRK m x             -> TRK (go m) (go x)
        MK_ROW rs           -> MK_ROW (go <$> rs)
        MK_TAB rs           -> MK_TAB (go <$> rs)
        r@CNS{}             -> r
        r@ARG{}             -> r
        r@VAR{}             -> r
        LET i v b           -> LET i (go v) (go b)
        IF_ c t e           -> IF_ (go c) (go t) (go e)
        SWI c f v           -> SWI (go c) (go f) (go <$> v)
        JMP c f v           -> JMP (go c) (go f) (go <$> v)
        JMP_WORD c f ks vs  -> JMP_WORD (go c) (go f) ks (go <$> vs)
        OP2 f op a b        -> OP2 f op (go a) (go b)

        EXE _ _ COw{} r -> do
            go $ MK_ROW $ reverse $ fromList $ toList r

        EXE _ _ (PIN p) r
            | sizeofSmallArray r == 2
            , Just (name, fun) <- matchPin p op2Table
                -> go $ OP2 name fun (r^0) (r^1)

        EXE _ _ (KLO 1 n) r
            | [PIN p, a] <- F.toList n
            , [b] <- F.toList r
            , Just (name, fun) <- matchPin p op2Table
                -> go $ OP2 name fun (CNS a) b

        EXE _ _ (CAb s) r ->
            go $ MK_TAB $ mapFromList $ zip (S.toDescList s) (toList r)

        EXE x s (KLO n e) r ->
            case e^0 of
                COw{} ->
                    go $ MK_ROW
                       $ fromList
                       $ reverse
                       $ ((fmap CNS $ drop 1 $ toList e) <>)
                       $ toList r
                CAb tabKeys ->
                    go $ MK_TAB
                       $ mapFromList
                       $ zip (S.toDescList tabKeys)
                       $ ((fmap CNS $ drop 1 $ toList e) <>)
                       $ toList r
                _ ->
                    EXE x s (KLO n e) (go <$> r)

        EXE x s f r -> EXE x s f (go <$> r)

matchPin :: Pin
         -> Map Hash256 (String, (Fan -> Fan -> Fan))
         -> Maybe (String, (Fan -> Fan -> Fan))
matchPin p tbl = unsafePerformIO $ getPinHash p <&> (flip M.lookup tbl)

valCode :: Fan -> (SmallArray Fan -> Fan)
valCode = \case
    KLO _ x   -> valCode (x^0)
    x@(FUN f) -> executeLaw x f.code f.code
    PIN p     -> p.exec
    NAT n     -> execNat n
    ROW{}     -> execFrame
    BAR{}     -> execFrame
    TAB{}     -> execFrame
    COw{}     -> execFrame
    CAb{}     -> execFrame
    REX{}     -> execFrame

-- Saturated calls become EXE nodes, undersaturated calls become KLO nodes.
resaturate :: Int -> Run -> Run
resaturate selfArgs = go
  where
    go LAZ{}       = error "impossible"
    go EXE{}       = error "impossible"
    go PAR{}       = error "impossible"
    go MK_ROW{}    = error "impossible"
    go MK_TAB{}    = error "impossible"
    go IF_{}       = error "impossible"
    go SWI{}       = error "impossible"
    go JMP{}       = error "impossible"
    go JMP_WORD{}  = error "impossible"
    go SEQ{}       = error "impossible"
    go REC{}       = error "impossible"
    go TRK{}       = error "impossible"
    go OP2{}       = error "impossible"

    go c@CNS{}     = c
    go r@VAR{}     = r
    go a@ARG{}     = a

    -- go (EXE f xs)  = EXE f xs
    -- go (PAR i xs)  = PAR i xs
    go (LET i v b) = LET i (go v) (go b)
    go (KAL xs)    = kal (toList xs)

    kal (KAL ks : xs) = kal (toList ks <> xs)
    kal (CNS c  : xs) = cns c (go <$> xs)
    kal (ARG 0  : xs) = sel (go <$> xs)
    kal xs            = KAL (smallArrayFromList $ go <$> xs)

    cns :: Fan -> [Run] -> Run
    cns f xs =
        let len = fromIntegral (length xs)
            r   = evalArity f
        in
        case compare r len of
          -- TODO work harder to keep these flat?
          GT -> PAR (r-len) (smallArrayFromList (CNS f : xs))
          EQ -> EXE (valCode f) (frameSize f) f (smallArrayFromList xs)
          LT -> KAL $ smallArrayFromList
                    $ (EXE (valCode f) (frameSize f) f (smallArrayFromList $ take r xs) : drop r xs)

    sel :: [Run] -> Run
    sel xs =
        let len = fromIntegral (length xs)
            r   = selfArgs
        in
        case compare r len of
          -- TODO work harder to keep these flat?
          GT -> PAR (r-len) (smallArrayFromList (ARG 0 : xs))
          EQ -> REC $ smallArrayFromList xs
          LT -> KAL $ smallArrayFromList
                    $ ((REC $ smallArrayFromList $ take r xs) : drop r xs)

ugly :: Nat -> String
ugly 0 = "0"
ugly nat =
    let ok '_' = True
        ok c   = C.isAlphaNum c
    in
    case natUtf8 nat of
        Right t | all ok t -> show t
        _                  -> show nat

compileLaw :: LawName -> Nat -> Fan -> Prog
compileLaw _lawName numArgs lBod =
    let (maxRef, code) = go numArgs lBod
        opt = resaturate (natToArity numArgs) code
        run = optimizeSpine (matchConstructors opt)
    in
{-
    if True || _lawName == "flushDownwards" then
       trace (ppShow ( ("lawName"::Text, _lawName)
                     , ("rawCode"::Text, code)
                     , ("semiOptimized"::Text, opt)
                     , ("optimizedCode"::Text, run)
                     ))
       PROG (fromIntegral numArgs)
            (fromIntegral maxRef + 1)
            run
    else
-}
       PROG (fromIntegral numArgs)
            (fromIntegral maxRef + 1)
            run
  where
    go :: Nat -> Fan -> (Nat, Run)
    go maxRef fan =
        case (kloList fan) of
            [NAT n] | n<=maxRef ->
                if n<=numArgs
                then (,) maxRef (ARG $ fromIntegral n)
                else (,) maxRef (VAR $ fromIntegral (n-(numArgs+1)))

            [NAT 0, f, x] ->
                    (,) (max fMax xMax) (KAL $ a2 fRun xRun)
                  where (fMax, fRun) = go maxRef f
                        (xMax, xRun) = go maxRef x

            [NAT 1, v, b] -> (,) (max vMax bMax) (LET idx vRun bRun)
                                  where (vMax, vRun) = go (maxRef+1) v
                                        (bMax, bRun) = go (maxRef+1) b
                                        idx          = fromIntegral ((maxRef+1)-(numArgs+1))
            [NAT 2, x]    -> (maxRef, CNS x)
            _             -> (maxRef, CNS fan)

{-
    recPro is different from exePro because, in a shattered-spine, we
    recurse into a different program (the outermost one) than the one
    we are running (the fragment).
-}
executeLaw :: Fan -> Prog -> Prog -> SmallArray Fan -> Fan
executeLaw self recPro exePro args =
    unsafePerformIO do
        let numArgs = length args
        let numVars = exePro.stkSz - numArgs

        -- traceM ("EXECUTING: " <> show pro <> "\n" <>
                -- "AGAINST: " <> show(toList args)
               -- )

        -- traceM ("EXECUTE LAW: " <> show self)
        -- traceM ("\t" <> show self)
        -- traceM ("\t" <> show pro)
        -- traceM ("\t" <> show (numArgs, numVars))
        -- traceM ("\t" <> show (toList args))
        let err = error ("UNINITIALIZED" <> show exePro.prgrm)
        vs <- newSmallArray numVars err
        go vs exePro.prgrm
  where
    go :: SmallMutableArray RealWorld Fan -> Run -> IO Fan
    go vs = \case
        CNS v  -> pure v
        ARG 0  -> pure self -- TODO Does this still need to be special-cased?
        ARG i  -> indexSmallArrayM args i
        VAR i  -> readSmallArray vs i
        KAL xs -> do
            -- traceM "KAL"
            cs <- traverse (go vs) xs
            pure (appN cs)

        LET i v b -> mdo
            -- traceM "LET"
            vRes <- (writeSmallArray vs i vRes >> go vs v)
            go vs b

        PAR r xs -> do
            -- traceM "PAR"
            env <- traverse (go vs) xs
            pure (KLO r env)

        -- TODO Maybe `trk` should take two arguments, name and data.
        TRK x b -> do
            xv <- go vs x
            evaluate (force xv)
            trk <- readIORef vTrkFan
            trk xv
            case trkName xv of
                Nothing -> go vs b
                Just (encodeUtf8 -> nm) ->
                    withAlwaysTrace nm "trk" do
                        res <- go vs b
                        evaluate res

        MK_ROW es -> do
            ROW <$> traverse (go vs) es

        MK_TAB es -> do
            -- print ("mk_tab"::Text, res)
            TAB <$> traverse (go vs) es

        EXE x sz (KLO _ e) xs -> do
            -- traceM "EXE_KLO"
            !buf <- newSmallArray sz (error "dur")
            let w = sizeofSmallArray e
            copySmallArray buf 0 e 0 w
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+w) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (x env)

        EXE x sz f xs -> do
            !buf <- newSmallArray sz f
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+1) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (x env)

        REC xs -> do
            let recArgs    = length xs
            let recEnvSize = recArgs + 1
            !buf <- newSmallArray recEnvSize self
            let fill i = unless (i==recArgs) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+1) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (executeLaw self recPro recPro env)

        SEQ x b -> do
            -- traceM "SEQ"
            xv <- go vs x
            _ <- evaluate xv
            go vs b

        IF_ i t e -> do
            -- traceM "IF_"
            go vs i >>= \case
                NAT 0 -> go vs e
                NAT _ -> go vs t
                _     -> go vs e

        SWI i f c -> do
          idx <- go vs i >>= \case
              NAT x -> pure $ fromIntegral x
              _     -> pure 0
          if idx >= sizeofSmallArray c
          then go vs f
          else go vs (c^idx)

        JMP i f c -> do
            key <- go vs i
            case lookup key c of
                Just x  -> go vs x
                Nothing -> go vs f

        -- TODO Rewrite `search` to use raw pointer manipulation.
        -- TODO Try to avoid the per-iteration bounds check by putting
        --      some sort of sentinal value at the end.
        JMP_WORD i f keyVec branches -> do
            go vs i >>= \case
                NAT (GHC.NatS# w) ->
                    let
                        !key = GHC.W# w
                        !end = length keyVec

                        search ix | ix>=end                 = go vs f
                        search ix | (keyVec SV.! ix == key) = go vs (branches^ix)
                        search ix                           = search (ix+1)
                    in
                        search 0
                _ ->
                    go vs f

        LAZ subroutine xs -> do
            let lazArgs = length xs
            !buf <- newSmallArray (lazArgs + 1) self
            let fill i = unless (i==lazArgs) do
                             v <- go vs (xs^i)
                             writeSmallArray buf (i+1) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf

            -- putStrLn "<LAZ>"
            -- pPrint ("self"::Text, self)
            -- pPrint ("prog"::Text, subroutine)
            -- pPrint ("envr"::Text, env)
            pure (executeLaw self recPro subroutine env)
            -- putStrLn "</LAZ>"
            -- pure res

        OP2 _ f a b -> do
          af <- go vs a
          bf <- go vs b
          pure $ f af bf

trkName :: Fan -> Maybe Text
trkName fan = do
    res <- case fan of
        NAT n  -> either (const Nothing) pure (natUtf8 n)
        BAR n  -> pure (decodeUtf8 n)
        ROW xs -> guard (not $ null xs) >> trkName (xs ! 0)
        _      -> Nothing
    guard (all C.isPrint res)
    pure res

-- WHAT EVEN -------------------------------------------------------------------

mkRow :: [Fan] -> Fan
mkRow = ROW . fromList

fanIdx :: Nat -> Fan -> Fan
fanIdx idxNat fan =
    if idxNat > fromIntegral (maxBound::Int) then
        -- We can't build structures big enough to index with non-int
        -- keys.
        0
    else
        go (fromIntegral idxNat) fan
  where
    go idx = \case
        ROW vec | idx < length vec -> vec ! idx
        TAB tab | idx < length tab -> snd (M.elemAt idx tab)
        KLO _ env                  -> idxKlo idx env
        _                          -> 0

    -- {{f 4 3} 2 1 0}
    idxKlo idx env =
        if arrIdx > 0
        then indexSmallArray env arrIdx
        else go (idx - (arrWid-1)) (indexSmallArray env 0)
      where
        arrWid  = sizeofSmallArray env
        arrIdx = (arrWid - idx) - 1

op2Table :: Map Hash256 (String, (Fan -> Fan -> Fan))
op2Table = mapFromList
  [ ( idxHash,    ("idx", op2Idx) )
  , ( getHash,    ("get", op2Get) )
  , ( addHash,    ("add", op2Add) )
  , ( subHash,    ("sub", op2Sub) )
  , ( mulHash,    ("mul", op2Mul) )
  , ( eqlHash,    ("eql", op2Eql) )
  , ( eqlNatHash, ("eqlNat", op2EqlNat) )
  , ( lteNatHash, ("lteNat", op2LteNat) )
  , ( lthNatHash, ("lthNat", op2LthNat) )
  , ( gteNatHash, ("gteNat", op2GteNat) )
  , ( gthNatHash, ("gthNat", op2GthNat) )
  ]

op2Idx :: Fan -> Fan -> Fan
op2Idx a b = fanIdx (toNat a) b

op2Get :: Fan -> Fan -> Fan
op2Get a b = fanIdx (toNat b) a

op2Add :: Fan -> Fan -> Fan
op2Add a b = NAT $ toNat a + toNat b

op2Sub :: Fan -> Fan -> Fan
op2Sub a b =
    let (x, y) = (toNat a, toNat b)
    in NAT (if y>x then 0 else (x-y))

op2Mul :: Fan -> Fan -> Fan
op2Mul a b = NAT $ toNat a * toNat b

op2Eql :: Fan -> Fan -> Fan
op2Eql a b = fromBit $ (fastValEq a b)

op2EqlNat :: Fan -> Fan -> Fan
op2EqlNat a b = fromBit $ toNat a == toNat b

op2LteNat :: Fan -> Fan -> Fan
op2LteNat a b = fromBit $ toNat a <= toNat b

op2LthNat :: Fan -> Fan -> Fan
op2LthNat a b = fromBit $ toNat a < toNat b

op2GteNat :: Fan -> Fan -> Fan
op2GteNat a b = fromBit $ toNat a >= toNat b

op2GthNat :: Fan -> Fan -> Fan
op2GthNat a b = fromBit $ toNat a > toNat b

-- Where should utility stuff like this go, which isn't directly related to
-- anything?
fromBit :: Bool -> Fan
fromBit True  = NAT 1
fromBit False = NAT 0

-- Jar for Vals ----------------------------------------------------------------

instance IsString Fan where
   fromString = NAT . utf8Nat . pack


--------------------------------------------------------------------------------

{-# INLINE saveFanPure #-}
saveFanPure :: Fan -> (Vector Pin, ByteString, ByteString)
saveFanPure fan = unsafePerformIO (saveFan fan)

{-# INLINE saveFanReference #-}
saveFanReference :: Fan -> IO (Vector Pin, ByteString, ByteString)
saveFanReference !top = do
    node <- fanToJellyNode top
    Jelly.saveSlow node
  where
    -- TODO Make this lazy again
    fanToJellyNode :: Fan -> IO (Jelly.Node Pin)
    fanToJellyNode = \case
        BAR bar ->
            pure (Jelly.BAR bar)

        NAT n ->
            case n of
                GHC.NatS# w -> pure $ Jelly.WORD (fromIntegral (W# w))
                _           -> pure $ Jelly.NAT n

        PIN pin -> do
            pHash <- getPinHash pin
            pure (Jelly.PIN pin pHash)

        fan -> do
            let (x,y) = boom fan
            Jelly.CONS <$> fanToJellyNode x <*> fanToJellyNode y
