-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{- |
We want to produce a mapping from names to rules.  We'd *like* to
just assign to each rule it's tag decoded as Utf8.  And indeed, in the
common case we would do just that.  But the edge cases must be handled:

-   Tags can be any nat, most of which are not valid identifiers.
-   Multiple rules may share the same tag.
-   Some rules with non-unique tags are best shown with specific names.
    In particular, it would be nice to show a three-item vectors as
   `(V3 x y z)`

Here are the rules we use to resolve this problem:

-   First, we map every tag to an initial name:

    -   If a rule is a vector constructor (Having the noun `(0 0 5 0)`),
        then we assign the name `V4`.

-   Second, we expect all tags to be valid identifiers [IDN]. (Note that
    rule identifiers may not contain `_`)

    Anything that doesn't follow these rules is simply assigned the
    (pre-disambiguation) name "".

-   We disambiguate by simply assigning a numeric suffix to each
    ambiguous name.  We do this in "environment order" so that we can
    achieve deterministic output:  `x_1` `x_2` `x_3` `x-y_1` `x-y_2`.

-   After this disambiguation process, we replace the empty name with "_".

-   At this point, all rules have a unique name.

    Since the initial names do not contain `_`, we don't have to worry
    about our disambiguation process producing a name that is already taken.

[IDN]:

    -   A valid "basic identifier" is a non-empty sequence of alpha
        numeric characters.  (Note that `_` is not accepted here).

    -   Identifiers are a non-empty sequence of basic identifiers
        separated by the `-`.

    -   Identifiers may not begin with a digit.

    Accepted: `{a a-b a0 a-0}`

    Not Accepted: `{_ a_ 5 0a a- -foo -foo-bar}`
-}

module Loot.Backend
    ( Closure(..)
    , NamedClosure(..)
    , nameClosure
    , ruleFanRaw
    , ruleFanOpt
    , valBod
    , bodVal
    , optimizeRul
    , Fan
    , valFan
    , plunVal
    , loadClosure
    , loadShallow
    , isFanCodeShaped
    , isValCodeShaped
    )
where

import Hash256
import Loot.Types
import PlunderPrelude

import Control.Monad.State (State, evalState, get, put, runState)
import Fan                 (Fan, Pin(..), (%%))
import Fan.PlanRex         (PlanRex(..), pexNoun)
import Loot.Syntax         (keyRex)
import Loot.Util           (lootRexToPex)
import Rex.Print           (RexColorScheme(NoColors), rexLine)

import Data.HashMap.Strict ()

import qualified Fan.PlanRex as PR
import qualified Data.Vector as V
import qualified Fan         as F

--------------------------------------------------------------------------------

--  Every pin in `closureEnv` must come before all references to that pin.
--  Printing the pins and reloading them one-by-one should produce the
--  same noun.
--
--  Furthermore, in order to get deterministic output.  We expect the rule
--  order to be defined by a depth-first, head-first traversal of the
--  entire noun.
--
--  Shallow loads can be done by loading some of them pins, and loading
--  only the names of the pins loading the names of the unloaded pins
--  that they reference.
--
--  The "name" of a pin is the name of the rule in it, or just 0.
data Closure = CLOSURE
    { closureEnv :: Vector (F.Pin, Maybe (Val Int))
    , closureVal :: Val Int
    }
  deriving (Show, Generic, NFData)

data NamedClosure = NAMED_CLOSURE
    { nmz :: Vector Symb
    , env :: Map Symb (Val Symb, Hash256)
    , val :: Val Symb
    }
  deriving (Show, Generic, NFData)

ruleFanRaw :: Rul Fan -> Fan
ruleFanRaw = valFan . rulVal

ruleFanOpt :: Rul Fan -> Fan
ruleFanOpt = valFan . rulVal . optimizeRul

rulVal :: Rul a -> Val a
rulVal (RUL (LN nm) ar bd) =
    NAT 0 `APP` (NAT $ fromIntegral nm)
          `APP` (NAT ar)
          `APP` (bodVal bd)


{-
    Removes extraneous CNS
-}
optimizeRul :: Rul Fan -> Rul Fan
optimizeRul (RUL nm ar bd) =
    RUL nm ar (go ar bd)
  where
    go :: Nat -> Bod Fan -> Bod Fan
    go maxArg = \case
        BLET v b -> BLET (go (maxArg+1) v) (go (maxArg+1) b)
        BAPP f x -> BAPP (go maxArg f) (go maxArg x)
        BVAR v   -> BVAR v
        BBAD v   -> BBAD v
        BCNS v   -> if isValCodeShaped maxArg v then BCNS v else BBAD v

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isValCodeShaped :: Nat -> Val Fan -> Bool
isValCodeShaped maxArg = loop
  where
    loop = \case
        NAT n                 -> n <= maxArg
        APP (APP (NAT 0) _) _ -> True
        APP (APP (NAT 1) _) _ -> True
        APP (NAT 2)         _ -> True

        REF (plunAlias -> Just rv) -> loop rv
        REF _                      -> False

        APP (REF r) a         -> case plunAlias r of
                                     Just rv -> loop (rv `APP` a)
                                     Nothing -> False
        APP (APP (REF r) a) b -> case plunAlias r of
                                     Just rv -> loop (rv `APP` a `APP` b)
                                     Nothing -> False

        _                     -> False

bodVal :: Bod a -> Val a
bodVal = \case
    BVAR v   -> NAT (fromIntegral v)
    BCNS c   -> APP (NAT 2) c
    BBAD v   -> v
    BAPP a b -> NAT 0 `APP` bodVal a `APP` bodVal b
    BLET v k -> NAT 1 `APP` bodVal v `APP` bodVal k

valBod :: Nat -> Val a -> Bod a
valBod maxArg = \case
    NAT v | v<=maxArg     -> BVAR v
    APP (NAT 2) v         -> BCNS v
    APP (APP (NAT 0) f) x -> BAPP (valBod (maxArg) f)   (valBod (maxArg) x)
    APP (APP (NAT 1) x) b -> BLET (valBod (maxArg+1) x) (valBod (maxArg+1) b)
    v                     -> BBAD v

{-
okIdn :: Text -> Bool
okIdn txt =
    fromMaybe False $ do
        (c, _ ) <- T.uncons txt
        guard (not (C.isDigit c))
        let parts = T.splitOn "-" txt
        guard (not $ null parts)
        guard (all okIdnFragment parts)
        pure True
  where
    okIdnFragment :: Text -> Bool
    okIdnFragment "" = False
    okIdnFragment tx = all okIdnChar tx

    okIdnChar '_' = True
    okIdnChar c   = C.isAlphaNum c
-}

nameClosure :: Closure -> NamedClosure
nameClosure (CLOSURE env val) =
    NAMED_CLOSURE names envir value
  where
    toMap :: (Int, (Pin, Maybe (Val Int)))
          -> Maybe (Symb, (Val Symb, Hash256))
    toMap (i, (p, mV)) = do
        v <- mV
        pure (names V.! i, ((names V.!) <$> v, p.hash))

    value = (names V.!) <$> val
    names = assignNames (F.valTag . (.item) . fst <$> env)
    envir = mapFromList $ catMaybes
                        $ fmap toMap
                        $ toList
                        $ V.imap (,) env

assignNames :: Vector Symb -> Vector Symb
assignNames initialNms =
    evalState (traverse f initialNms) (mempty :: Map Symb Nat)
  where
    f :: Symb -> State (Map Symb Nat) Symb
    f nm = do
        tab :: Map symb Nat <- get
        let used = fromMaybe 1 (lookup nm nmCount)
        let sufx = fromMaybe 1 (lookup nm tab)
        let rend = let ?rexColors = NoColors
                   in rexLine (keyRex nm)
        put (insertMap nm (sufx+1) tab)

        let taken :: Nat -> Bool
            taken c = member c nmCount || member c tab

        let loop :: Nat -> State (Map Symb Nat) Symb
            loop n = do
                let candidate = utf8Nat (rend <> "_" <> tshow n)
                if (taken candidate)
                then loop (n+1)
                else do put $ insertMap nm (n+1)
                            $ insertMap candidate 1
                            $ tab
                        pure candidate

        if used==1
            then pure nm
            else loop sufx

    nmCount :: Map Symb Int
    nmCount = foldr (alterMap (Just . maybe 1 succ)) mempty initialNms

{-
            ( 1 , "" ) -> "_"
            ( 1 , _  ) -> nm
            ( _ , "" ) -> "_/" <> tshow sufx
            ( _ , _  ) -> nm <> "/" <> tshow sufx
            -- TODO Can't be a string.
-}

-- Fan Backend -----------------------------------------------------------------

type Load = State (Int, HashMap Hash256 Int, [(Pin, Maybe (Val Int))])

valFan :: Val Fan -> Fan
valFan (NAT a)          = F.NAT a
valFan (REF v)          = v
valFan (APP f x)        = valFan f %% valFan x
valFan (BAR b)          = F.BAR b
valFan (ROW r)          = F.ROW (valFan <$> V.toArray r)
valFan (LAW n a b)      = F.mkLaw n a (valFan $ bodVal b)
valFan (COW 0)          = F.ROW mempty
valFan (COW n)          = F.COw n -- Never 0
valFan (TAB t)          = F.TAb $ mapFromList (pairFan <$> t)
valFan (SET k)          = F.SET $ setFromList $ valFan <$> k
valFan (ROX x)          = pexNoun (lootRexToPex (valFan <$> x))

pairFan :: (Val Fan, Val Fan) -> (Fan, Fan)
pairFan (k, v) = (valFan k, valFan v)

plunAlias :: Fan -> Maybe (Val Fan)
plunAlias F.PIN{} = Nothing
plunAlias topNod  = Just (go topNod)
  where
    pair (k, v) = (go k, go v)

    go = \case
        F.NAT a   -> NAT a
        n@F.PIN{} -> REF n
        F.BAR b   -> BAR b
        F.ROW r   -> ROW (V.fromArray (go <$> r))
        F.TAb t   -> TAB (pair <$> mapToList t)
        F.COw n   -> COW n
        F.SET n   -> SET (go <$> toList n)
        v@F.KLO{} -> let (h,t) = F.boom v in APP (go h) (go t)
        F.FUN l   -> LAW l.name l.args (valBod l.args $ go l.body)

goRex :: (Fan -> Load a) -> PR.PlanRex -> Load (LootRex a)
goRex go PR{n,v=Nothing}  = EVIL <$> go n
goRex go PR{v=(Just prn)} = goRexNode go prn

goRexNode :: (Fan -> Load a) -> PR.PlanRexNode -> Load (LootRex a)
goRexNode go = \case
    PR.EMBD_ val              -> EMBD <$> go val
    PR.LEAF_ s text heir      -> LEAF s text <$> traverse (goRex go) heir
    PR.NODE_ s rune sons heir -> NODE s rune <$> traverse (goRex go) sons
                                             <*> traverse (goRex go) heir

loadShallow :: Fan -> Closure
loadShallow inVal =
    let (top, (_,_,stk)) = runState (goTop inVal) (0, mempty, [])
    in CLOSURE (fromList $ reverse stk) top
  where
    goTop :: Fan -> Load (Val Int)
    goTop (F.PIN i) = REF <$> goTopPin i
    goTop vl        = go vl

    goTopPin :: F.Pin -> Load Int
    goTopPin pin = do
        kor <- (pin,) . Just <$> goTop pin.item
        (nex, tab, stk) <- get
        let haz  = pin.hash
        let tab' = insertMap haz nex tab
        put (nex+1, tab', kor:stk)
        pure nex

    goPair :: (Fan, Fan) -> Load (Val Int, Val Int)
    goPair (k, v) = (,) <$> go k <*> go v

    go :: Fan -> Load (Val Int)
    go = \case
        F.NAT a         -> pure (NAT a)
        F.PIN b         -> REF <$> goPin b
        F.BAR b         -> pure (BAR b)
        F.ROW r         -> ROW <$> traverse go (V.fromArray r)
        F.TAb t         -> TAB <$> traverse goPair (mapToList t)
        F.COw n         -> pure (COW n)
        F.SET n         -> SET <$> traverse go (toList n)
        v@F.KLO{}       ->
            case (PR.nounPex v).v of
                Just prn -> ROX <$> goRexNode go prn
                Nothing  -> let (h,t) = F.boom v in APP <$> go h <*> go t
        F.FUN law -> do
            b <- go law.body
            pure $ LAW law.name law.args (valBod law.args b)

    goPin :: F.Pin -> Load Int
    goPin pin = do
        (_, tabl, _) <- get
        let pHash = pin.hash
        case lookup pHash tabl of
            Just i -> pure i
            Nothing -> do
                let kor = (pin, Nothing)
                (nex, tab, stk) <- get
                let tab' = insertMap pHash nex tab
                put (nex+1, tab', kor:stk)
                pure nex

loadClosure :: Fan -> Closure
loadClosure inVal =
    let (top, (_,_,stk)) = runState (go inVal) (0, mempty, [])
    in CLOSURE (fromList $ reverse stk) top
  where
    goPair :: (Fan, Fan) -> Load (Val Int, Val Int)
    goPair (k, v) = (,) <$> go k <*> go v

    go :: Fan -> Load (Val Int)
    go = \case
        F.NAT a   -> pure (NAT a)
        F.PIN b   -> REF <$> goPin b
        F.BAR b   -> pure (BAR b)
        F.ROW r   -> ROW . V.fromArray <$> traverse go r
        F.TAb t   -> TAB <$> traverse goPair (mapToList t)
        F.COw n   -> pure (COW n)
        F.SET n   -> SET <$> traverse go (toList n)
        v@F.KLO{}       ->
            case (PR.nounPex v).v of
                Just prn -> ROX <$> goRexNode go prn
                Nothing  -> let (h,t) = F.boom v in goCel h t

        F.FUN law -> do
            b <- go law.body
            pure $ LAW law.name law.args (valBod law.args b)

    goCel x y = APP <$> go x <*> go y

    goPin :: F.Pin -> Load Int
    goPin pin = do
        (_, tabl, _) <- get
        let pHash = pin.hash
        case lookup pHash tabl of
            Just i -> pure i
            Nothing -> do
                kor <- (pin,) . Just <$> go pin.item
                (nex, tab, stk) <- get
                let tab' = insertMap pHash nex tab
                put (nex+1, tab', kor:stk)
                pure nex

-- 0, 1, ...
-- (0 f x)
-- (1 v b)
-- (2 c)
isFanCodeShaped :: Nat -> Fan -> Bool
isFanCodeShaped maxArg = loop
  where
    loop f = case F.kloList f of
        [F.NAT n]       -> n <= maxArg
        [F.NAT 0, _, _] -> True
        [F.NAT 1, _, _] -> True
        [F.NAT 2, _]    -> True
        _               -> False

-- TODO Better to return (Val F.Pin), but dont want to add another
-- type varibale to the Backend abstration.  Once we kill the abstraction,
-- then we should simplify this.
plunVal :: Fan -> Val Fan
plunVal = go
  where
    goPair :: (Fan, Fan) -> (Val Fan, Val Fan)
    goPair (k, v) = (go k, go v)

    go :: Fan -> Val Fan
    go = \case
        F.NAT a   -> NAT a
        v@F.KLO{} -> let (h,t) = F.boom v in APP (go h) (go t)
        F.FUN law -> LAW law.name law.args (valBod law.args (go law.body))
        p@F.PIN{} -> REF p
        F.BAR b   -> BAR b
        F.COw n   -> COW n
        F.SET ks  -> SET (go <$> toList ks)
        F.ROW xs  -> ROW (go <$> V.fromArray xs)
        F.TAb t   -> TAB (goPair <$> mapToList t)
