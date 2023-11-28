-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Compile (compileSire) where

import Loot.Backend
import PlunderPrelude hiding (to)
import Sire.Types

import Control.Monad.State.Strict (get, modify', put, runState)
import Data.List                  (foldl1, (!!))
import Fan                        ((%%), trueArity, Fan(NAT))
import Optics                     (set)


-- Inlining --------------------------------------------------------------------

data XArg = XARG { d::Int, x::Sire }
data Expo = EXPO { lam::Lam, mark::Bool, deep::Int, need::Int, args::[XArg] }
data Expr = EXPR { sire::Sire, expo::Maybe Expo }

inline :: Int -> [Maybe Expo] -> [XArg] -> Sire -> Expr
inline d s params syr = case syr of
    S_VAL _   -> reapply params $ EXPR syr Nothing
    S_VAR v   -> reapply params $ EXPR syr (s !! fromIntegral v)
    S_GLO b   -> reapply params $ EXPR syr (inline d [] [] b.d.code).expo
    S_LIN b   -> reapply params $ EXPR r (me <&> \e -> e{mark=True})
                   where EXPR r me = inline d s [] b
    S_LAM lam -> reapply params $ EXPR sire expo
                   where
                 need = fromIntegral lam.args
                 s'   = replicate (1 + need) Nothing <> s
                 d'   = 1 + need + d
                 sire = S_LAM lam{ body = (inline d' s' [] lam.body).sire }
                 expo = do guard (not $ hasRefTo lam.args lam.body)
                           let mark = lam.inline
                           Just $ EXPO { lam, mark, deep=d, need, args=[] }
    S_LET v b -> EXPR (S_LET vr.sire br.sire) Nothing
                   where vr = inline (d+1) (Nothing  : s) []     v
                         br = inline (d+1) (noCyc vr : s) params b
    S_APP f x -> inline d s (XARG d x' : params) f
                   where x' = (inline d s [] x).sire
  where
    noCyc x = guard (not $ hasRefTo 0 x.sire) >> x.expo

    reapply args f@(EXPR _ me) = case (args, me) of
        (_, Just e) | e.need==0 && e.mark -> inline d s args (expand e)
        ([], _)                           -> f
        (r@(XARG rd rx) : rs, _)          ->
            reapply rs $ EXPR (S_APP f.sire $ moveTo rd d 0 rx) do
                fe <- me
                guard (fe.need > 0)
                pure fe { need = fe.need - 1, args = r : fe.args }

    expand fe = foldr S_LET body $ renum 1 (XARG d (S_VAL 0) : reverse fe.args)
                  where body = moveTo fe.deep d (1 + fe.lam.args) fe.lam.body

    renum :: Int -> [XArg] -> [Sire]
    renum _  []     = []
    renum !n (a:as) = moveTo a.d (d+n) 0 a.x : renum (n+1) as

moveTo :: Int -> Int -> Nat -> Sire -> Sire
moveTo from to alreadyBound topExp =
    if from == to then topExp else go alreadyBound topExp
  where
    go :: Nat -> Sire -> Sire
    go l e = case e of
       S_VAR v | v>=l -> S_VAR ((v + fromIntegral to) - fromIntegral from)
       S_LIN x        -> S_LIN (go l x)
       S_APP f x      -> S_APP (go l f) (go l x)
       S_LET v b      -> S_LET (go (l+1) v) (go (l+1) b)
       S_LAM lam      -> S_LAM (lam { body = go (l + 1 + lam.args) lam.body })
       _              -> e

hasRefTo :: Nat -> Sire -> Bool
hasRefTo d = \case
    S_VAR v   -> v==d
    S_LET v b -> hasRefTo (d+1) v || hasRefTo (d+1) b
    S_APP f x -> hasRefTo d f     || hasRefTo d x
    S_LIN x   -> hasRefTo d x
    S_LAM l   -> hasRefTo (d + 1 + l.args) l.body
    _         -> False


-- Compiler --------------------------------------------------------------------

data Exp = VAL Fan | VAR Int | APP Exp Exp

data Fun = FUN
    { pin :: Bool
    , tag :: Nat
    , slf :: Int
    , arg :: [Int]
    , bin :: (IntMap Exp)
    , bod :: Exp
    }

app :: Exp -> Exp -> Exp
app (VAL f) (VAL x) | trueArity f > 1 = VAL (f %% x)
app f x                               = APP f x

ingest :: Sire -> (Exp, (IntMap Exp, Int))
ingest = \top -> runState (go [] top) (mempty, 0)
  where
    gensym = do { (s,v) <- get; put (s,v+1); pure v }

    go s = \case
        S_VAR i   -> pure $ (s !! fromIntegral i)
        S_LIN x   -> go s x
        S_GLO g   -> pure $ VAL g.d.value
        S_VAL x   -> pure $ VAL x
        S_APP f x -> app <$> go s f <*> go s x
        S_LET v b -> do
            k  <- gensym
            vr <- go (VAR k : s) v
            let keep = case vr of { VAR w -> w==k; APP{} -> True; _ -> False }
            when keep do modify' (over _1 $ insertMap k vr)
            go ((if keep then VAR k else vr) : s) b

        S_LAM lam@LAM{tag,pin} -> do
            slf      <- gensym
            arg      <- replicateM (fromIntegral lam.args) gensym
            (_, nex) <- get
            let env             = (VAR <$> reverse arg) <> [VAR slf] <> s
            let (bod,(bin,key)) = runState (go env lam.body) (mempty, nex)
            let (cns,free)      = compile key FUN{tag,pin,bod,slf,arg,bin}
            modify' (set _2 key) $> foldl' APP (VAL cns) (VAR <$> free)

stats :: Fun -> (Map Int Int, [Int])
stats pam = over _2 reverse $ snd $ runState (go pam.slf pam.bod) (mempty, [])
  where
    go _ VAL{}     = pure ()
    go e (APP f x) = go e f >> go e x
    go e (VAR k)   = do
        unless (k == e) do { maybe (pure()) (go k) (lookup k pam.bin) }
        modify' \(tab, lis) -> case lookup k tab of
            Nothing -> ( insertMap k 1 tab,      k:lis )
            Just cn -> ( insertMap k (cn+1) tab, lis   )

codeGen :: Fun -> (Map Int Int, [Int]) -> Fan
codeGen fn (refcounts, refSeq) =
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
    gen (VAL k)   = if isFanCodeShaped maxRef k then (2 %% k) else k
    gen (APP f x) = 0 %% gen f %% gen x
    gen (VAR v)   = case (lookup v refcounts, lookup v fn.bin) of
                        (Just 1, Just bx) -> gen bx
                        _                 -> NAT (look v table)

compile :: Int -> Fun -> (Fan, [Int])
compile nex f1 = (codeGen f3 stat3, free)
  where
    stat1@(_, !refs1) = stats f1
    free              = filter (isFree f1) refs1
    (f3, stat3)       = if null free then (f1, stat1) else (f2, stats f2)
    isFree FUN{..} k  = not $ or [k == slf, (k `member` bin), (k `elem` arg)]
    f2 = f1 { slf = nex
            , arg = free <> f1.arg
            , bin = insertMap f1.slf (foldl1 app $ map VAR (nex : free)) f1.bin
            }

compileSire :: Sire -> Fan
compileSire raw = fst (compile (n+2) fun) %% 0
  where (bod, (bin, n)) = ingest (inline 0 [] [] raw).sire
        fun = FUN{pin=False, tag=0, slf=n, arg=[n+1], bin, bod}
