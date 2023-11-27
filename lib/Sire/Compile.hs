-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Compile (compileSire) where

import Loot.Backend
import PlunderPrelude
import Sire.Types

import Control.Monad.State.Strict (State, get, modify', put, runState)
import Data.List                  (foldl1, (!!))
import Fan                        ((%%), trueArity, Fan(NAT))
import Optics                     (set)


-- Inlining --------------------------------------------------------------------

data Expo = EXPO
    { lam   :: Lam
    , mark  :: Bool
    , depth :: Int
    , need  :: Int
    , args  :: [(Int, Sire)]
    }

data Expr = EXPR { sire :: Sire, expo :: Maybe Expo }

exprApp :: Int -> Expr -> (Int, Sire) -> Maybe (Either Expo Sire)
exprApp depth (EXPR _ me) dx = do
    f <- me
    guard f.mark
    let fx = f { need = f.need - 1, args = dx : f.args }
    pure if fx.need > 0
         then Left fx
         else Right (inlineApp fx)
  where
    renum :: Int -> [(Int, Sire)] -> [Sire]
    renum _  []         = []
    renum !n ((d,s):ss) = (:) (migrate (fromIntegral(n+(depth-d))) 0 s)
                              (renum (n+1) ss)

    inlineApp :: Expo -> Sire
    inlineApp x =
        let offset = fromIntegral (depth - x.depth)
            body   = migrate offset (1 + x.lam.args) x.lam.body
            args   = reverse x.args
        in foldr S_LET body $ renum 1 ((depth, S_VAL 0) : args)

reapply :: Int -> [Maybe Expo] -> [(Int,Sire)] -> Expr -> Expr
reapply _     _ []             f = f
reapply depth s ((d,x) : args) f =
    case exprApp depth f (d,x) of
        Nothing         -> fall Nothing
        Just (Left fxe) -> fall (Just fxe)
        Just (Right ex) -> inline depth s args ex
  where
    offset = fromIntegral (depth - d)
    s_app  = S_APP f.sire (migrate offset 0 x)
    fall   = reapply depth s args . EXPR s_app

inline :: Int -> [Maybe Expo] -> [(Int, Sire)] -> Sire -> Expr
inline d s args = \case
    x@(S_VAL _) -> rap $ EXPR x Nothing
    x@(S_VAR v) -> rap $ EXPR x (s !! fromIntegral v)
    x@(S_GLO b) -> rap $ EXPR x (inline d [] [] b.d.code).expo

    S_LAM lam ->
        let need = fromIntegral lam.args
            s'   = replicate (1 + need) Nothing <> s
            d'   = 1 + need + d
            sire = S_LAM lam{ body = (inline d' s' [] lam.body).sire }
            expo = do guard (not $ isRecursive lam)
                      let mark = lam.inline
                      Just $ EXPO { lam, mark, depth=d, need, args=[] }
        in rap (EXPR sire expo)

    S_LET v b -> let vr = inline (d+1) (Nothing:s)  []   v
                     br = inline (d+1) (noCyc vr:s) args b
                 in EXPR (S_LET vr.sire br.sire) Nothing

    S_LIN b -> let EXPR r me = inline d s [] b
               in rap $ EXPR r (me <&> \x -> x{mark=True})

    S_APP f x -> let x' = (inline d s [] x).sire
                 in inline d s ((d,x') : args) f
  where
    rap = reapply d s args
    noCyc x = do { e <- x.expo; guard (not $ references 0 x.sire); pure e }

migrate :: Nat -> Nat -> Sire -> Sire
migrate offset alreadyBound topExp =
    if offset==0 then topExp else go alreadyBound topExp
  where
    go :: Nat -> Sire -> Sire
    go l e = case e of
       S_VAR v | v >= l -> S_VAR (v+offset)
       S_VAR{}          -> e
       S_VAL{}          -> e
       S_GLO{}          -> e
       S_LIN x          -> S_LIN (go l x)
       S_APP f x        -> S_APP (go l f) (go l x)
       S_LET v b        -> S_LET (go (l+1) v) (go (l+1) b)
       S_LAM lam        -> S_LAM (lam { body = go ll lam.body })
                             where ll = l + 1 + lam.args

isRecursive :: Lam -> Bool -- lambdas with self-reference cannot be inlined.
isRecursive topLam = references topLam.args topLam.body

references :: Nat -> Sire -> Bool
references d = \case
    S_VAR v   -> v==d
    S_VAL{}   -> False
    S_GLO{}   -> False
    S_LET v b -> references (d+1) v || references (d+1) b
    S_APP f x -> references d f     || references d x
    S_LIN x   -> references d x
    S_LAM l   -> references (d + 1 + l.args) l.body


-- Compiler --------------------------------------------------------------------

data Fun = FUN
    { pin :: !Bool
    , tag :: !Nat
    , slf :: !Int
    , arg :: ![Int]
    , bin :: !(IntMap Exp)
    , bod :: !Exp
    }

data Exp
    = VAL !Fan
    | VAR !Int
    | APP !Exp !Exp
    | SUB !Fun

app :: Exp -> Exp -> Exp
app (VAL f) (VAL x)  | trueArity f > 1 = VAL (f %% x)
app f x                                = APP f x

ingest :: Sire -> (Exp, (IntMap Exp, Int))
ingest = \top -> runState (go [] top) (mempty, 0)
  where
    genSym = do { (s,v) <- get; put (s,v+1); pure v }

    go s = \case
        S_VAR i   -> pure $ (s !! fromIntegral i)
        S_LIN x   -> go s x
        S_GLO g   -> pure $ VAL g.d.value
        S_VAL x   -> pure $ VAL x
        S_APP f x -> app <$> go s f <*> go s x
        S_LET v b -> do
            k  <- genSym
            vr <- go (VAR k : s) v
            let keep = case vr of VAR w | w==k -> True
                                  APP{}        -> True
                                  _            -> False
            when keep do modify' (over _1 $ insertMap k vr)
            go ((if keep then VAR k else vr) : s) b

        S_LAM lam@LAM{tag,pin} -> do
            slf      <- genSym
            arg      <- replicateM (fromIntegral lam.args) genSym
            (_, nex) <- get

            let env             = (VAR <$> reverse arg) <> [VAR slf] <> s
            let (bod,(bin,key)) = runState (go env lam.body) (mempty, nex)

            modify' (set _2 key)
            pure (SUB FUN{tag,pin,bod,slf,arg,bin})

stats :: Fun -> (Map Int Int, [Int])
stats pam = over _2 reverse $ snd $ runState (go pam.slf pam.bod) (mempty, [])
  where
    go :: Int -> Exp -> State (Map Int Int, [Int]) ()
    go e = \case
        SUB{}   -> error "lift sub-functions first"
        VAL{}   -> pure ()
        APP f x -> go e f >> go e x
        VAR k   -> do
            unless (k == e) do { maybe (pure()) (go k) (lookup k pam.bin) }
            modify' \(tab, lis) -> case lookup k tab of
                Nothing -> ( insertMap k 1 tab,      k:lis )
                Just cn -> ( insertMap k (cn+1) tab, lis   )

liftAll :: Int -> Fun -> Fun
liftAll n pam = pam { bin = (go <$> pam.bin), bod = go pam.bod }
  where
    go (APP f x) = app (go f) (go x)
    go (SUB f)   = let (v,xs) = compile n f in foldl1 app (VAL v:map VAR xs)
    go p         = p

codeGen :: Fun -> (Map Int Int, [Int]) -> Fan
codeGen fn (refcounts, refSeq) =
    (if fn.pin then (4 %%) else id) $
    0 %% NAT fn.tag
      %% NAT (fromIntegral numArgs)
      %% foldr bind (gen $ clean fn.bod) usedBinds
  where
    clean :: Exp -> Exp -- inlines single-use binds, folds constants apps.
    clean SUB{}     = error "lift lambdas first (codeGen)"
    clean (VAL v)   = VAL v
    clean (APP f x) = app (clean f) (clean x)
    clean (VAR v)   = case (lookup v refcounts, lookup v fn.bin) of
                         (Just 1, Just bx) -> clean bx
                         _                 -> VAR v

    keep k = case (lookup k refcounts, lookup k fn.bin) of
                 (_, Nothing) -> False
                 (Nothing, _) -> False
                 (Just i, _)  -> i>1

    usedBinds = filter keep refSeq
    numBinds  = length usedBinds
    numArgs   = length fn.arg
    maxRef    = fromIntegral (numArgs + numBinds)
    scope     = fn.slf : (fn.arg <> usedBinds)
    table     = mapFromList (zip scope [0..]) :: Map Int Nat

    bind :: Int -> Fan -> Fan
    bind k rest = 1 %% gen (clean vx) %% rest
      where vx = fromMaybe (error "impossible") $ lookup k fn.bin

    gen :: Exp -> Fan
    gen SUB{}     = error "lift lambdas first (codeGen)"
    gen (VAL k)   = if isFanCodeShaped maxRef k then (2 %% k) else k
    gen (APP f x) = 0 %% gen f %% gen x
    gen (VAR v)   = NAT $ fromMaybe (error "impossible") $ lookup v table

compile :: Int -> Fun -> (Fan, [Int])
compile nex f1 = (codeGen f3 stat3, free)
  where
    f2                = liftAll nex f1
    stat2@(_, !refs2) = stats f2
    free              = filter (isFree f2) refs2

    selfExp = foldl1 app $ VAR <$> (nex : free)
    newArgs = free <> f2.arg
    newBind = insertMap f2.slf selfExp f2.bin
    res     = f2 { slf = nex, arg=newArgs, bin=newBind }

    (f3, stat3) = if null free then (f2, stat2) else (res, stats res)

    isFree FUN{..} k = not $ or [k == slf, (k `member` bin), (k `elem` arg)]

compileSire :: Sire -> Fan
compileSire raw = fst (compile (n+2) fun) %% 0
  where (bod, (bin, n)) = ingest (inline 0 [] [] raw).sire
        fun = FUN{pin=False, tag=0, slf=n, arg=[n+1], bin, bod}
