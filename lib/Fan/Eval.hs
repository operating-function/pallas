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
    , executeLaw
    , compileLaw
    , mkPin
    , mkPin'
    , mkLaw
    , mkLawPreNormalized
    , appN
    , kloList
    , kloWalk
    , kloArgs
    , fanIdx
    , mkRow
    , evalArity
    , vTrkFan
    , vTrkRex
    , vShowFan
    , vJetMatch
    , vRtsConfig
    , normalize
    , trkName
    , loadPinFromBlob
    , tabValsRow
    , setToRow
    , lawName
    , lawArgs
    , lawBody
    )
where

import Data.Sorted
import Fan.Prof
import Fan.RunHashes
import Fan.Types
import Fan.Util
import PlunderPrelude hiding (hash)

import Control.Exception   (throw)
import Control.Monad.ST    (ST)
import Data.Char           (isAlphaNum)
import Fan.Eval.Strictness (optimizeSpine)
import Fan.FFI             (c_revmemcmp)
import Fan.PinRefs         (pinRefs)
import GHC.Prim            (reallyUnsafePtrEquality#)
import Hash256             (shortHex)

import {-# SOURCE #-} Fan.Hash (fanHash)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Char              as C
import qualified Data.Foldable          as F
import qualified Data.Map               as M
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as SV
import qualified Fan.Eval.LetRec        as LetRec
import qualified GHC.Exts               as GHC


-- Infix Operators -------------------------------------------------------------


infixl 5 %%;


-- Globals ---------------------------------------------------------------------

-- These should all be overwritten on startup.  These exists to break
-- dependency cycles, and aren't intended to support dynamic changes.

vTrkFan :: IORef (Fan -> IO ())
vTrkFan = unsafePerformIO $ newIORef $ const $ pure ()

vTrkRex :: IORef (Rex -> IO ())
vTrkRex = unsafePerformIO $ newIORef $ const $ pure ()

vShowFan :: IORef (Fan -> Text)
vShowFan = unsafePerformIO $ newIORef $ const "[PLUN]"

vJetMatch :: IORef (Pin -> IO Pin)
vJetMatch = unsafePerformIO (newIORef pure)

vRtsConfig :: IORef RtsConfig
vRtsConfig = unsafePerformIO $ newIORef $ RTS_CONFIG
    { onJetFallback = WARN
    , onJetMismatch = WARN
    }

-- Types -----------------------------------------------------------------------

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

-- TODO What if the small array has extra shit, and that shit can't
-- safely be forced?  Don't do `mapSmallArray`, do it by hand.
normalize :: Fan -> Fan
normalize top =
    if isNormal top then top else go top
  where
    isNormal (KLO _ xs) =
        case xs .! 0 of
            KLO{} -> False
            _     -> all isNormal (toList xs)
    isNormal (ROW v) = all isNormal v
    isNormal (TAb t) = all isNormal t
    isNormal !_      = True

    go tp = case tp of
        NAT !_ -> tp
        BAR !_ -> tp
        PIN !_ -> tp
        FUN !_ -> tp
        COw !_ -> tp
        SET !_ -> tp
        ROW r  -> ROW (go <$> r)
        TAb t  -> TAb (go <$> t)
        KLO r eRaw ->
            let e = mapSmallArray' go eRaw in
            case (e .! 0) of
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
            _  -> x.hash == y.hash

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
    TAb t   == TAb u   = (t==u)
    SET c   == SET d   = (c==d)
    COw n   == COw m   = (n==m)
    FUN l   == FUN a   = (l==a)
    v@KLO{} == w@KLO{} = (kloWalk v == kloWalk w)
    _       == _       = False

instance Ord Pin where
    compare x y =
        case GHC.reallyUnsafePtrEquality x y of
            1# -> EQ
            _  -> unsafePerformIO do
                      pure $ if x.hash == y.hash
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

    compare (BAR x) (BAR y) =
        let !xw = length x in
        let !yw = length y in
        case compare xw yw of
            LT -> LT
            GT -> GT
            EQ -> unsafePerformIO $
                  BS.unsafeUseAsCString x \xBuf ->
                  BS.unsafeUseAsCString y \yBuf -> do
                      i <- c_revmemcmp xBuf yBuf (fromIntegral xw)
                      pure (compare i 0)

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
        fanLen TAb{}     = 1
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
            SET{}   -> [f]
            COw{}   -> [f]
            ROW r   -> if null r then [f] else COw (nat(length r)) : reverse (toList r) -- COw here is never empty
            KLO _ k -> toList k
            TAb t   -> [SET (tabKeysSet t), ROW (fromList $ toList t)]

{-# INLINE lawName #-}
lawName :: Fan -> Nat
lawName = \case
    FUN l -> l.name.nat
    NAT{} -> 0
    PIN{} -> 0
    KLO{} -> 0
    BAR{} -> 1
    ROW{} -> 0
    TAb{} -> 0
    SET{} -> 1
    COw{} -> 0

{-# INLINE lawArgs #-}
lawArgs :: Fan -> Nat
lawArgs = \case
    FUN l -> l.args
    PIN p -> p.args
    BAR{} -> 1
    COw c -> c+1
    ROW r -> if null r then 1 else 0 -- Only a law if empty
    SET{} -> 2
    TAb{} -> 0 -- Not a function
    KLO{} -> 0 -- Not a function
    NAT{} -> 0 -- Not a function

setToRow :: ArraySet Fan -> Fan
setToRow set = ROW (ssetToArray set)

{-# INLINE lawBody #-}
lawBody :: Fan -> Fan
lawBody = \case
    FUN l -> l.body
    BAR b -> NAT (barBody b)
    SET k -> setToRow k
    COw{} -> NAT 0 -- Actual law body is 0
    ROW{} -> NAT 0 -- Actual law body is 0
    TAb{} -> NAT 0 -- Not a law
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
          2   -> ( xs.!0 , xs.!1 )
          len -> ( let
                     flow !_ !0 = xs.!0
                     flow !r !i = KLO (r+1) (a2 (flow (r+1) (i-1)) (xs.!i))
                   in
                     flow arity (len-2)
                 , xs.!(len-1)
                 )

    -- Builds lazy list of two-element KLO nodes.
    ROW row ->
        let !len = length row in
        case len of
            0 -> boom (COw 0)
            1 -> (COw 1, row ! 0)
            n -> ( let
                     flow !i !0   = COw (fromIntegral i) -- i is never 0
                     flow !i !ram = KLO i $ a2 (flow (i+1) (ram-1)) (row ! i)
                   in
                     flow 1 (n-1)
                 ,
                   row ! 0
                 )

    TAb tab ->
        ( SET $ tabKeysSet tab
        , ROW $ tabElemsArray tab
        )

    SET ks ->
        rul (LN 1) 2 (ROW $ ssetToArray ks)

  where
    rul :: LawName -> Nat -> Fan -> (Fan, Fan)
    rul (LN n) a b =
        ( KLO 1 (a3 (NAT 0) (NAT n) (NAT a))
        , b
        )

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

instance Show Fan where
    show (NAT n)   = ugly n
    show (KLO _ x) = show (toList x)
    show (FUN l)   = show l
    show (PIN p)   = show p
    show (COw n)   = "R" <> show n
    show (ROW v)   = "(ROW " <> show v <> ")"
    show (TAb t)   = "(TAB " <> show (showTab t) <> ")"
    show (SET k)   = "(SET " <> show (toList k) <> ")"
    show (BAR b)   = "(BAR " <> show b <> ")"

showTab :: Tab Fan Fan -> [(Fan,Fan)]
showTab t = tabToAscPairsList t

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


--------------------------------------------------------------------------------

barBody :: ByteString -> Nat
barBody bytes =
    -- TODO Make this not slow
    bytesNat (bytes <> BS.singleton 1)

--------------------------------------------------------------------------------

matchData :: LawName -> Nat -> Fan -> Maybe Fan
matchData (LN 0) 1 (NAT 0) = Just $ ROW mempty
matchData (LN 0) n (NAT 0) = Just $ COw (n-1) -- n-1 is never zero
matchData (LN 1) 2 (ROW v) = matchSet v
matchData (LN 1) 1 (NAT n) = matchBar n
matchData (LN _) _ _       = Nothing

matchBar :: Nat -> Maybe Fan
matchBar n = do
    guard (n /= 0)
    let bitWidth = (natBitWidth n :: Nat) - 1
    guard (0 == (bitWidth `mod` 8))
    let bytWidth = fromIntegral (bitWidth `div` 8)
    pure $ BAR $ take bytWidth $ natBytes n

matchSet :: Array Fan -> Maybe Fan
matchSet vs = do
    case toList vs of
        []   -> Just (SET mempty)
        a:es -> collect mempty a es
  where
    collect !acc i []           = pure (SET $ insertSet i acc)
    collect !acc i (w:ws) | w>i = collect (insertSet i acc) w ws
    collect _    _ _            = Nothing


-- Constructing Pins and Laws --------------------------------------------------

mkLawPreNormalized :: LawName -> Nat -> Fan -> Fan
mkLawPreNormalized nam arg bod =
    if arg==0
    then throw (PRIMOP_CRASH 0 0)
    else fromMaybe (FUN $ L nam arg bod $ compileLaw nam arg bod)
           $ matchData nam arg bod

mkLaw :: LawName -> Nat -> Fan -> Fan
mkLaw nam arg bod = mkLawPreNormalized nam arg (normalize bod)

mkPin :: Fan -> Fan
mkPin = PIN . unsafePerformIO . mkPin'

frameSize :: Fan -> Int
frameSize (KLO _ e) = frameSize (e.!0)
frameSize v         = 1 + evalArity v

{-
        These are called extremely often, and we don't want to bog down
        the system by emiting profiling events for them.
-}
highFreqLaws :: Set Nat
highFreqLaws = setFromList
    (
        [ "dec", "add", "mul", "sub", "bex", "lte", "lth", "div"
        , "mod", "aeq", "lsh", "rsh", "met", "mix", "dis", "con"
        , "if", "eql", "trk", "idx", "get", "len"
        , "weld", "map", "put", "mut", "take", "drop", "cat", "rev"
        , "w32", "add32", "mul32", "div32", "and32", "or32", "xor32"
        , "lsh32", "rsh32", "sub32", "ror32", "rol32", "isBar", "barIdx"
        , "barWeld", "barCat", "barFlat", "natBar", "barDrop", "barTake"
        , "barLen", "barNat", "setSingleton", "setIns", "setDel", "setMin"
        , "setLen", "setUnion", "setHas", "setSplitAt", "setSplitLT"
        , "setIntersection", "tabSingleton", "tabIdx", "tabElem"
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
        let nam = encodeUtf8 (valName pin.item)
        let key = nam <> "-(" <> shortHex pin.hash <> ")"
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
               let ari = trueArity item
               let hax = fanHash item
               let ref = pinRefs item
               res <- addProfilingToPin =<< match (P ref hax ari item exe)
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

loadPinFromBlob :: Vector Pin -> Hash256 -> Fan -> IO Pin
loadPinFromBlob refs hax item = do
    match <- readIORef vJetMatch

    res <- mdo let slf = (PIN res)
               let exe = case item of
                             FUN law -> \e -> executeLaw slf law.code law.code e
                             _       -> \e -> foldl' (%%) item (toList e)
                                           -- TODO is this correct?
                                           -- Doesn't the passed environment
                                           -- include ourselves?
               let !ari = trueArity item
               res <- addProfilingToPin =<< match (P refs hax ari item exe)
               pure res

    evaluate res.exec

    pure res

{-# INLINE pinExec #-}
pinExec :: Fan -> Fan -> (SmallArray Fan -> Fan)
pinExec self = \case
    FUN law -> executeLaw self law.code law.code
    item    -> foldl' (%%) item . drop 1 . toList


-- Evaluation ------------------------------------------------------------------

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
       2 -> app2 (xs.!0) (xs.!1)
       3 -> app3 (xs.!0) (xs.!1) (xs.!2)
       4 -> app4 (xs.!0) (xs.!1) (xs.!2) (xs.!3)
       !wid ->
            let !arity = evalArity (xs.!0)
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

execFrame :: SmallArray Fan -> Fan
execFrame buf =
    let x = buf.!0 in
    case x of
        FUN l -> executeLaw x l.code l.code buf
        PIN p -> p.exec buf
        ROW v -> mkCow (fromIntegral $ length v)
        KLO{} -> error "Invalid stack frame, closure as head"
        NAT n -> execNat n buf
        BAR b -> if null b then buf.!1 else NAT (barBody b)
        TAb t -> ROW (tabKeysArray t) -- tabs return keys row
        COw n ->
            let !las = fromIntegral n in
            ROW $ rowGenerate (fromIntegral n) \i ->
                      (buf .! (las - i))

        SET ks ->
            if sizeofSmallArray buf == 3 -- (set badVals arg)
            then
                -- This only happens if the first arguments was not a
                -- valid values-row.  Here we run the actual legal
                -- behavior, which is to return the keys row.
                ROW (ssetToArray ks)
            else
                case buf.!1 of
                    ROW vals | (length vals == length ks) ->
                        TAb (mkTab ks vals)
                    _ ->
                        KLO 1 buf

eval2 :: Fan -> Fan -> Fan
eval2 fn x1 =
    case fn of
        k@(KLO _ x) ->
            case x.!0 of
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
            case x.!0 of
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
            case x.!0 of
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
            !i <- fill buf (e.!0)
            let !w = sizeofSmallArray e
            let !v = w-1
            copySmallArray buf i e 1 v
            pure (i+v)
        hed ->
            writeSmallArray buf 0 hed $> 1

deriving instance Eq PrimopCrash
deriving instance Ord PrimopCrash
instance Exception PrimopCrash where
    displayException (PRIMOP_CRASH n x) =
        unsafePerformIO do
            s <- readIORef vShowFan
            pure $ concat [ "Evaluation crashed by calling the number "
                          , show n
                          , " with this argument:\n\n"
                          , unpack (s x)
                          ]

instance Show PrimopCrash where
    show = displayException

execNat :: Nat -> SmallArray Fan -> Fan
execNat 0 e = mkLaw (LN $ toNat $ e.!1) (toNat $ e.!2) (e.!3)
execNat 1 e = wut (e.!1) (e.!2) (e.!3) (e.!4) (e.!5)
execNat 2 e = case toNat (e.!3) of 0 -> e.!1
                                   n -> (e.!2) %% NAT(n-1)
execNat 3 e = NAT (toNat(e.!1) + 1)
execNat 4 e = mkPin (e.!1)
execNat n e = unsafePerformIO do
    let arg = (e.!1)
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

    -- Always a pair
    v@TAb{} -> let (hd,tl) = boom v in app3 a hd tl

    SET k -> wutSet k

    x@(ROW v) ->
        if null v
        then wutCow 0
        else app3 a h t where (h,t) = boom x
  where
    wutCow m = app4 l (NAT 0) (NAT (m+1)) (NAT 0)

    wutSet k =
        app4 l (NAT 1) (NAT 2) (ROW $ ssetToArray k)

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
    SET k -> setWut k
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
showCns TAb{}    = "TAB"
showCns SET{}    = "SET"
showCns BAR{}    = "BAR"
showCns (NAT n)  = show n

instance Show Prog where
    show p = "(PROG "
          <> "{ arity =" <> show p.arity
          <> ", varsSz=" <> show p.varsSz
          <> ", prgrm=(" <> show p.prgrm <> ")"
          <> "})"

showBind :: (Int, Run) -> String
showBind (i,x) = show (VAR i) <> " " <> show x

instance Show Run where
    show (CNS c) = showCns c
    show (ARG i) = "_" <> show i
    show (VAR i) = "_v" <> show i
    show (KAL xs) = "(KAL " <> intercalate " " (show <$> toList xs) <> ")"

    show (EXE _ _ f xs) =
        "(EXE " <> showCns f <> " " <> intercalate " " (show <$> toList xs) <> ")"

    show (PAR n xs) =
         "(PAR arity_is_" <> show n <> intercalate " " (show <$> toList xs) <> ")"

    show (REC xs) = "(REC " <> intercalate " " (show <$> toList xs) <> ")"

    show (TRK v b) = "(TRK " <> show v <> " " <> show b <> ")"

    show (MK_ROW es) = "(MKROW " <> intercalate " " (show <$> es) <> ")"

    show (MK_TAB vs) = "(MKTAB " <> intercalate " " (show <$> (tabToAscPairsList vs)) <> ")"

    show (LETREC vs v) =
        "(LETREC [" <> intercalate " " (showBind <$> vs) <> "] " <> show v <> ")"
    show (LET i x v) =
        "(LET " <> showBind (i,x) <> " " <> show v <> ")"

    show (IF_ c t e) =
        "(IF " <> show c <> " " <> show t <> " " <> show e <> ")"

    show (IFZ c t e) =
        "(IFZ " <> show c <> " " <> show t <> " " <> show e <> ")"

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

    show (JMP x f vs) =
        parencalate ["TAB_SWITCH", show x, show f, show vs]

    show (OP2 name _ a b) =
        parencalate ["OP2", show name, show a, show b]

parencalate :: [String] -> String
parencalate xs = "(" <> intercalate " " xs <> ")"

-- Match row/tab constructors: (c2 y x) -> MK_ROW [x,y]
matchConstructors :: Run -> Run
matchConstructors = go
  where
    go = \case
        LAZ{}               -> error "matchConstructors: impossible"
        SEQ v x             -> SEQ (go v) (go x)
        REC vs              -> REC (go <$> vs)
        KAL vs              -> KAL (go <$> vs)
        PAR r vs            -> PAR r (go <$> vs)
        TRK m x             -> TRK (go m) (go x)
        MK_ROW rs           -> MK_ROW (go <$> rs)
        MK_TAB vs           -> MK_TAB (go <$> vs)
        r@CNS{}             -> r
        r@ARG{}             -> r
        r@VAR{}             -> r
        LET i v b           -> LET i (go v) (go b)
        LETREC vs b         -> LETREC (fmap go <$> vs) (go b)
        IF_ c t e           -> IF_ (go c) (go t) (go e)
        IFZ c t e           -> IFZ (go c) (go t) (go e)
        SWI c f v           -> SWI (go c) (go f) (go <$> v)
        JMP c f vs          -> JMP (go c) (go f) (go <$> vs)
        JMP_WORD c f ks vs  -> JMP_WORD (go c) (go f) ks (go <$> vs)
        OP2 f op a b        -> OP2 f op (go a) (go b)

        EXE _ _ COw{} r -> do
            go $ MK_ROW $ reverse $ fromList $ toList r

        EXE _ _ (PIN p) r
            | sizeofSmallArray r == 2
            , Just (name, fun) <- matchPin p op2Table
                -> go $ OP2 name fun (r.!0) (r.!1)

        EXE _ _ (KLO 1 n) r
            | [PIN p, a] <- F.toList n
            , [b] <- F.toList r
            , Just (name, fun) <- matchPin p op2Table
                -> go $ OP2 name fun (CNS a) b

        EXE x s (SET ks) r ->
            if sizeofSmallArray r /= 1 then
                error "TODO: Remove this check, since this should never happen"
            else
            case go (r.!0) of
                MK_ROW vs | length vs == length ks ->
                    MK_TAB (mkTab ks $ V.toArray vs)
                _ ->
                    EXE x s (SET ks) (go <$> r)

        EXE x s (KLO n e) r ->
            case e.!0 of
                COw{} ->
                    go $ MK_ROW
                       $ fromList
                       $ reverse
                       $ ((fmap CNS $ drop 1 $ toList e) <>)
                       $ toList r
                _ ->
                    EXE x s (KLO n e) (go <$> r)

        EXE x s f r -> EXE x s f (go <$> r)

matchPin :: Pin
         -> Map Hash256 (String, (Fan -> Fan -> Fan))
         -> Maybe (String, (Fan -> Fan -> Fan))
matchPin p tbl =  M.lookup p.hash tbl

valCode :: Fan -> (SmallArray Fan -> Fan)
valCode = \case
    KLO _ x   -> valCode (x.!0)
    x@(FUN f) -> executeLaw x f.code f.code
    PIN p     -> p.exec
    NAT n     -> execNat n
    ROW{}     -> execFrame
    BAR{}     -> execFrame
    TAb{}     -> execFrame
    COw{}     -> execFrame
    SET{}     -> execFrame

-- Saturated calls become EXE nodes, undersaturated calls become KLO nodes.
resaturate :: Int -> Run -> Run
resaturate selfArgs = go
  where
    go LAZ{}       = error "resaturate: impossible"
    go EXE{}       = error "resaturate: impossible"
    go PAR{}       = error "resaturate: impossible"
    go MK_ROW{}    = error "resaturate: impossible"
    go MK_TAB{}    = error "resaturate: impossible"
    go IF_{}       = error "resaturate: impossible"
    go IFZ{}       = error "resaturate: impossible"
    go SWI{}       = error "resaturate: impossible"
    go JMP{}       = error "resaturate: impossible"
    go JMP_WORD{}  = error "resaturate: impossible"
    go SEQ{}       = error "resaturate: impossible"
    go REC{}       = error "resaturate: impossible"
    go TRK{}       = error "resaturate: impossible"
    go OP2{}       = error "resaturate: impossible"

    go c@CNS{}     = c
    go r@VAR{}     = r
    go a@ARG{}     = a

    -- go (EXE f xs)  = EXE f xs
    -- go (PAR i xs)  = PAR i xs
    go (LET i v b)   = LET i (go v) (go b)
    go (LETREC vs b) = LETREC (fmap go <$> vs) (go b)
    go (KAL xs)      = kal (toList xs)

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
    in case natUtf8 nat of
        Right t | all ok t -> show t
        _                  -> show nat

-- TODO: Review potential for overflow of `numArgs`
compileLaw :: LawName -> Nat -> Fan -> Prog
compileLaw _lawName numArgs lBod =
    let lxp    = LetRec.loadLawBody numArgs lBod
        lxpOpt = LetRec.optimize (fromIntegral numArgs) lxp
        (code, maxVar) = LetRec.compile (fromIntegral numArgs) lxpOpt
        opt    = resaturate (natToArity numArgs) code
        run    = optimizeSpine (matchConstructors opt)
        prog   = PROG (fromIntegral numArgs)
                      (fromIntegral (maxVar + 1))
                      run
    in
    {-
    if True || _lawName == "flushDownwards" then
       trace (ppShow ( ("lawName"::Text, _lawName)
                     , ("rawLxp"::Text, lxp)
                     , ("optLxp"::Text, lxpOpt)
                     , (("rawRun"::Text, code), ("maxVar"::Text, maxVar))
                     , ("semiOptimized"::Text, opt)
                     , ("finalProg"::Text, prog)
                     ))
       prog
    else
    -}
       prog
  where

{-
    recPro is different from exePro because, in a shattered-spine, we
    recurse into a different program (the outermost one) than the one
    we are running (the fragment).
-}
executeLaw :: Fan -> Prog -> Prog -> SmallArray Fan -> Fan
executeLaw self recPro exePro args =
    unsafePerformIO do
        let numVars = exePro.varsSz

        -- traceM ( "EXECUTING: " <> show pro <> "\n"
        --       <> "AGAINST: " <> show (toList args)
        --        )

        -- traceM ("EXECUTE LAW: " <> show self)
        -- traceM ("\t" <> show self)
        -- traceM ("\t" <> show pro)
        -- traceM ("\t" <> show (numArgs, numVars))
        -- traceM ("\t" <> show (toList args))
        let err = error ("UNINITIALIZED" <> show exePro.prgrm)
        vs <- newSmallArray (numVars + 1) err
                -- TODO: Figure out why this is wrong!!  This is no good!
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

        LETREC binds b ->
            if sizeofSmallArray binds == 1 then do
                let (i, r) = binds .! 0
                rec res <- (writeSmallArray vs i res >> go vs r)
                go vs b
            else do
                rec for_ (zip [0..] $ toList binds) \(ix,(slot,_)) ->
                        writeSmallArray vs slot (results .! ix)
                    results <- for binds \(_,v) -> go vs v
                go vs b

        LET i v b -> mdo
            -- traceM "LET"
            when (i < 0) do
                error "bad index"
            when (i >= sizeofSmallMutableArray vs) do
                error $ concat [ "out of bounds: "
                                , show i
                                , ">="
                                , show (sizeofSmallMutableArray vs)
                                , "\n"
                                , ppShow exePro
                                ]
            go vs v >>= writeSmallArray vs i
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
            ROW . V.toArray <$> traverse (go vs) es

        MK_TAB es -> do
            -- print ("mk_tab"::Text, res)
            TAb <$> traverse (go vs) es

        EXE x sz (KLO _ e) xs -> do
            -- traceM "EXE_KLO"
            !buf <- newSmallArray sz (error "dur")
            let w = sizeofSmallArray e
            copySmallArray buf 0 e 0 w
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs.!i)
                             writeSmallArray buf (i+w) v
                             fill (i+1)
            fill 0
            env <- unsafeFreezeSmallArray buf
            pure (x env)

        EXE x sz f xs -> do
            !buf <- newSmallArray sz f
            let !nar = sizeofSmallArray xs
            let fill i = unless (i==nar) do
                             v <- go vs (xs.!i)
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
                             v <- go vs (xs.!i)
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

        IFZ i t e -> do
            go vs i >>= \case
                NAT 0 -> go vs t
                _     -> go vs e


        SWI i f c -> do
          idx <- go vs i >>= \case
              NAT x -> pure $ fromIntegral x
              _     -> pure 0
          if idx >= sizeofSmallArray c
          then go vs f
          else go vs (c.!idx)

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
                NAT (NatS# w) ->
                    let
                        !key = GHC.W# w
                        !end = length keyVec

                        search ix | ix>=end                 = go vs f
                        search ix | (keyVec SV.! ix == key) = go vs (branches.!ix)
                        search ix                           = search (ix+1)
                    in
                        search 0
                _ ->
                    go vs f

        LAZ subroutine xs -> do
            let lazArgs = length xs
            !buf <- newSmallArray (lazArgs + 1) self
            let fill i = unless (i==lazArgs) do
                             v <- go vs (xs.!i)
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
        TAb tab | idx < length tab -> if idxNat==0 then tabValsRow tab else 0
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
  [ ( idxHash, ("idx", op2Idx) )
  , ( getHash, ("get", op2Get) )
  , ( addHash, ("add", op2Add) )
  , ( subHash, ("sub", op2Sub) )
  , ( mulHash, ("mul", op2Mul) )
  , ( eqlHash, ("eql", op2Eql) )
  , ( lteHash, ("lte", op2Lte) )
  , ( lthHash, ("lth", op2Lth) )
  , ( gteHash, ("gte", op2Gte) )
  , ( gthHash, ("gth", op2Gth) )
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
op2Eql a b = fromBit (fastValEq a b)

op2Lte :: Fan -> Fan -> Fan
op2Lte a b = fromBit (a <= b)

op2Lth :: Fan -> Fan -> Fan
op2Lth a b = fromBit (a < b)

op2Gte :: Fan -> Fan -> Fan
op2Gte a b = fromBit (a >= b)

op2Gth :: Fan -> Fan -> Fan
op2Gth a b = fromBit (a > b)
