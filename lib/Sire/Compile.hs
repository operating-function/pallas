{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile
    ( Inliner
    , Refr(..)
    , Global(..)
    , compileSire
    , showSymb
    )
where

import Loot.Backend
import PlunderPrelude
import Rex
import Sire.Types

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State  (StateT(..), evalStateT, get)
import Loot.Syntax          (symbRex)
import Loot.Types           (Bod(..), LawName(LN), Rul(..), Val(..))
import Optics               (_3)
import Optics.Zoom          (zoom)

import qualified Data.Map as M
import qualified Fan      as F


-- Utils -----------------------------------------------------------------------

showSymb :: Symb -> Text
showSymb =
    let ?rexColors = NoColors
    in rexLine . symbRex


-- Types -----------------------------------------------------------------------

data Global = G { val :: Fan, inliner :: Inliner }
  deriving (Generic, Eq)

instance Show Global where
  show (G (F.NAT n) _) = "(AT " <> show n <> ")"
  show (G pln _)       = "(G " <> show (F.valName pln) <> ")"

type Inliner = Maybe (Fun Refr Global)

data Refr = REFR {name :: !Symb, key  :: !Int}

instance Eq  Refr where (==)    x y = (==)    x.key y.key
instance Ord Refr where compare x y = compare x.key y.key

instance Show Refr where
    show r = unpack (showSymb r.name) <> "_" <> show r.key
    -- TODO Doesn't handle all cases correctly


-- Gensym ----------------------------------------------------------------------

vCompilerGenSym :: IORef Int
vCompilerGenSym = unsafePerformIO (newIORef 0)
  -- TODO: Input should already have a unique identity assigned to
  -- each symbol.  Use that and make this stateless.

gensym :: MonadIO m => Symb -> m Refr
gensym nam = do
    key <- readIORef vCompilerGenSym
    nex <- evaluate (key+1)
    writeIORef vCompilerGenSym nex
    pure (REFR nam key)

refreshRef
    :: Refr
    -> StateT (Int, Map Int Refr) IO Refr
refreshRef ref =
    (lookup ref.key . snd <$> get) >>= \case
        Just r  -> pure r
        Nothing -> pure ref

refreshBinder
    :: Refr
    -> StateT (Int, Map Int Refr) IO Refr
refreshBinder ref = do
    tab <- snd <$> get
    let key = ref.key
    case lookup key tab of
        Just r ->
            pure r
        Nothing -> do
            r <- zoom _1 (gensym ref.name)
            modifying _2 (insertMap key r)
            pure r

duplicateFun
    :: Fun Refr b
    -> StateT (Int, Map Int Refr) IO (Fun Refr b)
duplicateFun (FUN self name args body) = do
    self' <- refreshBinder self
    args' <- traverse refreshBinder args
    body' <- duplicateExp body
    pure (FUN self' name args' body')

-- Duplicates an expression, creating fresh Refrs for each binding.
duplicateExp :: Exp Refr b -> StateT (Int, Map Int Refr) IO (Exp Refr b)
duplicateExp = go
  where
    go  :: Exp Refr b
        -> StateT (Int, Map Int Refr) IO (Exp Refr b)
    go expr = case expr of
        EVAR x     -> EVAR <$> refreshRef x
        EREC v e b -> EREC <$> refreshBinder v <*> go e <*> go b
        ELET v e b -> ELET <$> refreshBinder v <*> go e <*> go b
        EAPP f x   -> EAPP <$> go f <*> go x
        ELAM p f   -> ELAM p <$> duplicateFun f
        ELIN xs    -> ELIN <$> traverse go xs
        EVAL{}     -> pure expr
        EREF{}     -> pure expr

duplicateExpTop :: Exp Refr b -> IO (Exp Refr b)
duplicateExpTop e = evalStateT (duplicateExp e) (0, mempty)


-- Name Resolution -------------------------------------------------------------

resolveFun :: Map Symb Refr -> Fun Symb Symb -> IO (Fun Refr Symb)
resolveFun env (FUN self tag args body) = do
    selfR <- gensym self
    argsR <- traverse gensym args
    envir <- pure (M.union
                   (mapFromList
                    (zip (self : toList args)
                         (selfR : toList argsR)))
                      env)
    bodyR <- resolveExp envir body
    pure (FUN selfR tag argsR bodyR)

resolveExp
    :: MonadIO m
    => Map Symb Refr
    -> Exp Symb Symb
    -> m (Exp Refr Symb)
resolveExp e = liftIO . \case
    EVAL b     -> pure (EVAL b)
    EREF r     -> pure (maybe (EREF r) EVAR (lookup r e))
    EVAR v     -> pure (maybe (EREF v) EVAR (lookup v e))
    EAPP f x   -> EAPP <$> go e f <*> go e x
    ELIN xs    -> ELIN <$> traverse (go e) xs
    ELAM p f   -> ELAM p <$> resolveFun e f
    EREC n v b -> goLet True n v b
    ELET n v b -> goLet False n v b
  where
    go = resolveExp
    goLet isRec n v b = do
        r <- gensym n
        let e2   = insertMap n r e
        let con  = if isRec then EREC else ELET
        let vEnv = if isRec then e2 else e
        con r <$> go vEnv v <*> go e2 b


-- Optimization ----------------------------------------------------------------

numRefs :: Int -> Exp Refr b -> Int
numRefs k = \case
    EVAR r               -> if r.key == k then 1 else 0
    EVAL{}               -> 0
    EREF{}               -> 0
    EAPP f x             -> go f + go x
    ELIN xs              -> sum (go <$> xs)
    EREC _ v b           -> go v + go b
    ELET _ v b           -> go v + go b
    ELAM _ (FUN _ _ _ b) -> min 1 (go b)
    --- Multiple references from a sub-functions only counts as one because
    --- it will be lambda-lifted (hence only used once).
  where
    go = numRefs k

-- Trivial if duplicating is cheaper than a let-reference.
-- TODO: All atom literals should be considered trivial.
trivialExp :: Exp a b -> Bool
trivialExp = \case
    EVAL{} -> True
    EREF{} -> True
    EVAR{} -> True
    _      -> False

optimizeLet
    :: (Int, IntMap (Int, Bod Global, Maybe (Fun Refr Global)))
    -> Refr
    -> Exp Refr Global
    -> Exp Refr Global
    -> ExceptT Text IO (Int, Bod Global, Inliner)
optimizeLet s@(nex, tab) refNam expr body = do
  let recurRef = numRefs k expr > 0
      multiRef = numRefs k body >= 2
  if
    trivialExp expr || (not recurRef && not multiRef)
  then do
    (varg, vv, mBodLin) <- expBod s expr
    let s' = (nex, insertMap k (varg, vv, mBodLin) tab)
    (barg, bb, mArgLin) <- expBod s' body
    let rarg = if (varg == 0) then 0 else barg
    pure (rarg, bb, mArgLin)
  else
    if not recurRef
    then do
      let s' = (nex+1, insertMap k (0, var nex, Nothing) tab)
      (varg, vv, mBodLin) <- expBod s' expr
      let s'' = (nex+1, insertMap k (0, var nex, mBodLin) tab)
      (barg, bb, _) <- expBod s'' body
      let rarg = if (varg == 0 || barg == 0) then 0 else barg-1
          -- TODO Why barg-1?  Shouldn't it just be `barg`?
      pure (rarg, BLET vv bb, Nothing)
    else do
      let s' = (nex+1, insertMap k (0, var nex, Nothing) tab)
      (varg, vv, _) <- expBod s' expr
      (barg, bb, _) <- expBod s' body
      let rarg = if (varg == 0 || barg == 0) then 0 else barg-1
          -- TODO Why barg-1?  Shouldn't it just be `barg`?
      pure (rarg, BLET vv bb, Nothing)
  where
    k = refNam.key
    var = BVAR . fromIntegral


-- Lambda Lifting --------------------------------------------------------------

freeVars :: ∀b. Fun Refr b -> Set Refr
freeVars = goFun mempty
 where
    goFun :: Set Refr -> Fun Refr b -> Set Refr
    goFun ours (FUN self _ args body) =
      let keyz = setFromList (self : toList args)
      in go (ours <> keyz) body

    go :: Set Refr -> Exp Refr b -> Set Refr
    go ours = \case
        EVAL{}     -> mempty
        EREF{}     -> mempty
        EVAR r     -> if (r `elem` ours)
                      then mempty
                      else singleton r
        ELAM _ f   -> goFun ours f
        EAPP f x   -> go ours f <> go ours x
        ELIN xs    -> concat (go ours <$> xs)
        EREC n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b
        ELET n v b -> let ours' = insertSet n ours
                      in go ours' v <> go ours' b


--  TODO Don't lift trivial aliases, just inline them (small atom, law)
--  TODO If we lifted anything, need to replace self-reference with a
--       new binding.
lambdaLift
    :: (Int, IntMap (Int, Bod Global, Inliner))
    -> Bool
    -> Fun Refr Global
    -> ExceptT Text IO (Exp Refr Global)
lambdaLift _s pinned f@(FUN self tag args body) = do
    let lifts = toList (freeVars f) :: [Refr]
    let liftV = EVAR <$> lifts
    let self' = self {key=2348734}
    let body' = EREC self (app (EVAR self') liftV)  body
    let funct = FUN self' tag (derpConcat lifts args) body'
    lam <- injectFun funct
    let pln = if pinned then F.mkPin lam else lam
    pure $ app (EREF (G pln Nothing)) liftV
  where
    app fn []     = fn
    app fn (x:xs) = app (EAPP fn x) xs

    derpConcat :: [a] -> NonEmpty a -> NonEmpty a
    derpConcat xs ys = case xs <> toList ys of
                           []   -> error "Not possible"
                           z:zs -> z :| zs

{-
    Variables that simply rebind constant values are replaced by constant
    values.

    TODO: When does this matter?

    TODO: Doesn't the normal optimization pass eliminate these?

    TODO This only looks at `EVAR`, would be much shorter to write using
    `uniplate` or whatever.
-}
inlineTrivial
    :: IntMap (Int, Bod Global, Inliner)
    -> Fun Refr Global
    -> Fun Refr Global
inlineTrivial tab (FUN self tag args body) =
    FUN self tag args (go body)
  where
    go = \case
        EVAL b     -> EVAL b
        EREF r     -> EREF r
        ELAM p f   -> ELAM p (inlineTrivial tab f)
        EAPP f x   -> EAPP (go f) (go x)
        ELIN xs    -> ELIN (go <$> xs)
        EREC n v b -> EREC n (go v) (go b)
        ELET n v b -> ELET n (go v) (go b)
        EVAR v     -> case lookup v.key tab of
                        Just (0, _, _)      -> EVAR v -- TODO: does this matter?
                        Just (_, BCNS c, _) -> EVAL (valFan ((.val) <$> c))
                        _                   -> EVAR v


-- Inlining --------------------------------------------------------------------

inlineExp
    :: IntMap (Int, Bod Global, Inliner)
    -> Exp Refr Global
    -> NonEmpty (Exp Refr Global)
    -> ExceptT Text IO (Exp Refr Global)
inlineExp tab f xs =
    case f of
        ELAM _ l             -> doFunc l xs
        EREF (G _ Nothing)   -> noInline "Not inlinable"
        EREF (G _ (Just fn)) -> doFunc fn xs
        EVAR v               -> doVar v
        _                    -> unknownFunction
  where
    noInline = throwError

    unknownFunction =
        noInline "Head of ! expression is not a known function"

    doVar v =
        case (lookup v.key tab >>= view _3) of
            Nothing -> noInline "Not a function"
            Just fn -> doFunc fn xs

    doFunc (FUN self _ args body) params = do
        case ( compare (length params) (length args)
             , numRefs self.key body
             )
          of
            (EQ, 0) -> do res <- liftIO $ duplicateExpTop
                                        $ foldr (uncurry ELET) body
                                        $ zip args params
                          pure res
            (EQ, _) -> noInline "Cannot inline recursive functions"
            (GT, _) -> noInline "Inline Expression has too many arguments"
            (LT, _) -> noInline "Inline Expression has too few arguments"


-- Sire Expression to Law Body -------------------------------------------------

expBod
    :: (Int, IntMap (Int, Bod Global, Inliner))
    -> Exp Refr Global
    -> ExceptT Text IO (Int, Bod Global, Inliner)
expBod s@(_, tab) = \case
    EVAL b         -> do let ari = F.trueArity b
                         pure ( fromIntegral ari
                              , BCNS (REF (G b Nothing))
                              , Nothing
                              )
    ELAM p f       -> do lifted <- lambdaLift s p (inlineTrivial tab f)
                         bodied <- expBod s lifted
                         let (arity, bod, _) = bodied
                         pure (arity, bod, Just f)
                           -- TODO Inlining Flag
    EVAR r         -> pure $ fromMaybe (error "Internal Error")
                           $ lookup r.key tab
    EREF (G t i)   -> pure ( fromIntegral (F.trueArity t)
                           , BCNS (REF (G t i))
                           , i
                           )

    EAPP f x -> do
        fR <- expBod s f
        xR <- expBod s x
        pure $ case (fR, xR) of
            ((_, fv, _),     (0,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((0, fv, _),     (_,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((1, fv, _),     (_,xv,_)     ) -> (0,   BAPP fv xv,       Nothing)
            ((a, BCNS fk, _),(_,BCNS xk,_)) -> (a-1, BCNS (APP fk xk), Nothing)
            ((a, fv, _),     (_,xv,_)     ) -> (a-1, BAPP fv xv,       Nothing)

    ELIN (f :| []) -> expBod s f

    ELIN (f :| (x:xs)) ->
        (lift $ runExceptT $ inlineExp tab f (x:|xs)) >>= \case
            Left _  -> expBod s (apple f (x:xs))
            Right e -> expBod s e

    EREC n v b -> optimizeLet s n v b
    ELET n v b -> optimizeLet s n v b
  where
    apple f []    = f
    apple f (b:c) = apple (EAPP f b) c


-- Sire Function to PLAN Value -------------------------------------------------

injectFun :: Fun Refr Global -> ExceptT Text IO Fan
injectFun (FUN self nam args exr) = do
    (_, b, _) <- expBod (nexVar, environ) exr
    let rul = RUL nam (fromIntegral ari) b
    pure (ruleFanOpt ((.val) <$> rul))
  where
    ari = length args
    nexVar = ari+1
    argKeys = (.key) <$> (self : toList args)
    argRefs = ((0,,Nothing) . BVAR <$> [0..])
    environ = mapFromList (zip argKeys argRefs)


-- The Sire Compiler -----------------------------------------------------------

compileSire :: Map Symb Global -> XExp -> ExceptT Text IO Global
compileSire scope ast = do
    body <- resolveExp mempty ast
    expr <- traverse (getRef scope) body
    (_, _, inliner) <- expBod (1, mempty) expr

    self <- gensym (utf8Nat "self")
    argu <- gensym (utf8Nat "argu")
    let rawFun = FUN self (LN 0) (argu:|[]) body
    func <- traverse (getRef scope) rawFun
    fanFun <- injectFun func
    let fan = (fanFun F.%% 0)

    pure (G fan inliner)
  where
    getRef :: Map Symb Global -> Symb -> ExceptT Text IO Global
    getRef env nam = do
        maybe (unresolved nam) pure (lookup nam env)

    unresolved :: Symb -> ExceptT Text IO a
    unresolved nam =
        throwError ("Unresolved Reference: " <> showSymb nam)
