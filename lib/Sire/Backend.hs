-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Backend
    ( Bind(..), BindData(..), Sire(..), Lam(..)
    , eval, hasRefTo
    )
where

import PlunderPrelude hiding (to)

import Fan                        (trueArity, (%%), Fan(NAT), kloList)
import Control.Monad.State.Strict (get, modify', put, runState, State)
import Data.List                  (foldl1, (!!))
import Optics                     (_3)


-- Types -----------------------------------------------------------------------

data Sire
    = V Nat
    | K Fan
    | G Bind
    | A Sire Sire
    | L Sire Sire
    | R [Sire] Sire
    | M Sire
    | F Lam

data Lam = LAM
    { pin  :: Bool
    , mark :: Bool
    , recr :: Bool
    , tag  :: Nat
    , args :: Nat
    , body :: Sire
    }

-- TODO:: (Key, Ctx, Name, Value, Expr, Props)
-- TODO:: maybe eliminate the key.
data BindData = BIND_DATA
    { key      :: Nat
    , value    :: Fan
    , code     :: Sire
    , location :: Fan
    , name     :: Fan
    , props    :: Fan
    }

-- This indirection is so that the front-end can cheaply reproduce the
-- PLAN value that this was loaded from.
data Bind = BIND { bd :: BindData, noun :: Fan }


-- Inlining --------------------------------------------------------------------

data Arg = ARG { d::Int, x::Sire }
data Pot = POT { fn::Lam, marked::Bool, deep::Int, need::Int, args::[Arg] }
data Res = RES { out::Sire, pot::Maybe Pot }

hasRefTo :: Nat -> Sire -> Bool
hasRefTo d = \case
    V v   -> v==d
    L v b -> hasRefTo d v || hasRefTo (1+d) b
    R v b -> any (hasRefTo $ d+fromIntegral (length v)) (b:v)
    A f x -> hasRefTo d f     || hasRefTo d x
    M x   -> hasRefTo d x
    F l   -> hasRefTo (d + 1 + l.args) l.body
    _     -> False

moveTo :: Int -> Int -> Nat -> Sire -> Sire
moveTo from to alreadyBound topExp =
    if from == to then topExp else go alreadyBound topExp
  where
    go :: Nat -> Sire -> Sire
    go l e = case e of
       V v | v>=l -> V ((v + fromIntegral to) - fromIntegral from)
       M x        -> M (go l x)
       A f x      -> A (go l f) (go l x)
       L v b      -> L (go l v) (go (l+1) b)
       R v b      -> R (go l' <$> v) (go l' b)
                       where l' = l + fromIntegral (length v)
       F fn       -> F (fn { body = go (l + 1 + fn.args) fn.body })
       _          -> e

inline :: Int -> [Maybe Pot] -> [Arg] -> Sire -> Res
inline d env params inp = case inp of
    K _   -> reapply params $ RES inp Nothing
    V v   -> reapply params $ RES inp (env !! fromIntegral v)
    G b   -> reapply params $ RES inp (inline d [] [] b.bd.code).pot
    M b   -> reapply params $ RES r (me <&> \e -> e{marked=True})
               where RES r me = inline d env [] b
    F fn  -> reapply params $ RES sire pot
               where
             need = fromIntegral fn.args
             env' = replicate (1 + need) Nothing <> env
             d'   = 1 + need + d
             sire = F fn{body=(inline d' env' [] fn.body).out}
             pot  = do guard (not fn.recr)
                       Just POT{fn, marked=fn.mark, deep=d, need, args=[]}
    L v b -> RES (L vr.out br.out) Nothing
               where vr = inline d     env             []     v
                     br = inline (d+1) (vr.pot  : env) params b
    R v b -> RES (R ((.out) <$> vr) br.out) Nothing
               where d'   = d + length v
                     env' = (const Nothing <$> v) <> env
                     vr   = inline d' env' [] <$> v
                     br   = inline d' env' params b
    A f x -> inline d env (ARG d x' : params) f
               where x' = (inline d env [] x).out
  where
    reapply args f@(RES _ me) = case (args, me) of
        (_, Just e) | e.need==0 && e.marked -> inline d env args (expand e)
        ([], _)                             -> f
        (r@(ARG rd rx) : rs, _)             ->
            reapply rs $ RES (A f.out $ moveTo rd d 0 rx) do
                fe <- me
                guard (fe.need > 0)
                pure fe { need = fe.need - 1, args = r : fe.args }

    expand fe = foldr L body $ renum 0 (ARG d (K 0) : reverse fe.args)
                  where body = moveTo fe.deep d (1 + fe.fn.args) fe.fn.body

    renum :: Int -> [Arg] -> [Sire]
    renum _  []     = []
    renum !n (a:as) = moveTo a.d (d+n) 0 a.x : renum (n+1) as


-- Compiling -------------------------------------------------------------------

data Exp = VAL Fan | VAR Int | APP Exp Exp

data Fun = FUN
    { pin :: Bool
    , tag :: Nat
    , slf :: Int
    , arg :: [Int]
    , bin :: (IntMap Exp)
    , bod :: Exp
    }

ingest :: Sire -> (Exp, (IntMap Exp, Int))
ingest = \top -> runState (go [] top) (mempty, 0)
  where
    gensym = do { (s,v) <- get; put (s,v+1); pure v }

    go s = \case
        V i   -> pure $ (s !! fromIntegral i)
        M x   -> go s x
        G g   -> pure $ VAL g.bd.value
        K x   -> pure $ VAL x
        A f x -> ((,) <$> go s f <*> go s x) <&> \case
            (VAL fv, VAL xv) | trueArity fv > 1 -> VAL (fv %% xv)
            (fr, xr)                            -> APP fr xr

        L v b -> go s v >>= \case
            vr@APP{} -> do k <- gensym
                           modify' (over _1 $ insertMap k vr)
                           go (VAR k : s) b
            vr -> go (vr:s) b

        R vs b -> do
            ks <- replicateM (length vs) gensym
            let ss = map VAR ks <> s
            for_ (zip vs ks) \(vx,k) -> do
                vr <- go ss vx
                modify' $ over _1 $ insertMap k vr
            go ss b

        F fn@LAM{tag,pin} -> do
            slf <- gensym
            arg <- replicateM (fromIntegral fn.args) gensym
            nex <- snd <$> get
            let env             = map VAR (reverse arg) <> [VAR slf] <> s
            let (bod,(bin,key)) = runState (go env fn.body) (mempty, nex)
            let (cns,free)      = compile key FUN{tag,pin,bod,slf,arg,bin}
            pure $ foldl' APP (VAL cns) (VAR <$> free)

stats :: Fun -> (Set Int, Map Int Int, [Int])
stats pam =
    over _3 reverse $ snd $ runState (go pam.bod) (mempty, mempty, [])
  where
    go :: Exp -> State (Set Int, Map Int Int, [Int]) ()
    go VAL{}     = pure ()
    go (APP f x) = go f >> go x
    go (VAR k)   = do
        alreadySeen <- view _1 <$> get
        unless (k `member` alreadySeen) do
            modify' (over _1 $ insertSet k)
            maybe (pure()) go (lookup k pam.bin)
        modify' \(seen, tab, lis) ->
            let c = fromMaybe 0 (lookup k tab)
            in (seen, (insertMap k (c+1) tab), (if c==0 then k:lis else lis))

isCodeShaped :: Nat -> Fan -> Bool
isCodeShaped maxArg f = case kloList f of
    [NAT n]       -> n <= maxArg
    [NAT 0, _, _] -> True
    [NAT 1, _, _] -> True
    [NAT 2, _]    -> True
    _             -> False

codeGen :: Fun -> (Set Int, Map Int Int, [Int]) -> Fan
codeGen fn (_, refcounts, refSeq) =
    (if fn.pin then (4 %%) else id)
    (0 %% NAT fn.tag %% NAT numArgs %% foldr bind (gen fn.bod) binds)
  where
    look k v      = fromMaybe (error "impossible") (lookup k v)
    keep k        = fromMaybe 0 (lookup k fn.bin >> lookup k refcounts) > 1
    binds         = filter keep refSeq
    numBinds      = fromIntegral (length binds)
    numArgs       = fromIntegral (length fn.arg)
    maxRef        = numArgs + numBinds
    scope         = fn.slf : (fn.arg <> binds)
    table         = mapFromList (zip scope [0..]) :: Map Int Nat
    bind k rest   = 1 %% gen (look k fn.bin) %% rest
    gen (VAL k)   = if isCodeShaped maxRef k then (2 %% k) else k
    gen (APP f x) = 0 %% gen f %% gen x
    gen (VAR v)   = case (lookup v refcounts, lookup v fn.bin) of
                        (Just 1, Just bx) -> gen bx
                        _                 -> NAT (look v table)

compile :: Int -> Fun -> (Fan, [Int])
compile nex f1 = (codeGen f3 stat3, free)
  where
    stat1@(_, _, !refs1) = stats f1
    free                 = filter (isFree f1) refs1
    (f3, stat3)          = if null free then (f1, stat1) else (f2, stats f2)
    isFree FUN{..} k     = not $ or [k == slf, (k `member` bin), (k `elem` arg)]
    f2 = f1 { slf = nex
            , arg = free <> f1.arg
            , bin = insertMap f1.slf (foldl1 APP $ map VAR (nex : free)) f1.bin
            }

eval :: Sire -> Fan
eval raw = fst (compile (n+2) fun) %% 0
  where (bod, (bin, n)) = ingest (inline 0 [] [] raw).out
        fun = FUN{pin=False, tag=0, slf=n, arg=[n+1], bin, bod}
