{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile
    ( Inliner
    , Refr(..)
    , Global(..)
    , Expr
    , Func
    , compileSire
    , showSymb
    , gensym
    , duplicateExpTop
    )
where

import Loot.Backend
import PlunderPrelude
import Sire.Compile.Types
import Sire.Types

import Control.Monad.Except (ExceptT(..), runExceptT)
import Loot.Types           (Bod(..), LawName(LN), Rul(..), Val(..))
import Sire.Inline          (inlineGlobals)

import qualified Data.Map as M
import qualified Fan      as F


-- Internal Types --------------------------------------------------------------

{-
    The compilation result:

    -   [args]: If the expression is a constant, this is the number
        of extra arguments that can be applied before the expression is
        no longer constant.  If the expression is not constant, this is
        set to 0.

    -   [code]: The actual PLAN law-code that this compiles into.

    -   [inline]: This it the function that would be used for inlining
        if inlining were requested against this.  If this is a reference
        to a known function (local or global), then the value will be
        set to (Just fun) otherwise Nothing.
-}
data CRes = CR
    { arity  :: Nat
    , code   :: Bod Global
    , inline :: Inliner
    }


-- How many times is a variable referenced? ------------------------------------

numRefs :: Int -> Exp Refr b -> Int
numRefs k = \case
    EVAR r                 -> if r.key == k then 1 else 0
    EVAL{}                 -> 0
    EREF{}                 -> 0
    EAPP f x               -> go f + go x
    ELIN f                 -> go f
    EREC _ v b             -> go v + go b
    ELET _ v b             -> go v + go b
    ELAM _ (FUN _ _ _ _ b) -> min 1 (go b)
    --- Multiple references from a sub-functions only counts as one because
    --- it will be lambda-lifted (hence only used once).
  where
    go = numRefs k


-- Name Resolution -------------------------------------------------------------

resolveFun :: Map Symb Refr -> Fun Symb Symb -> IO (Fun Refr Symb)
resolveFun env (FUN iline self tag args body) = do
    selfR <- gensym self
    argsR <- traverse gensym args
    envir <- pure (M.union
                   (mapFromList
                    (zip (self : toList args)
                         (selfR : toList argsR)))
                      env)
    bodyR <- resolveExp envir body
    pure (FUN iline selfR tag argsR bodyR)

resolveExp :: MonadIO m => Map Symb Refr -> Exp Symb Symb -> m (Exp Refr Symb)
resolveExp e = liftIO . \case
    EVAL b     -> pure (EVAL b)
    EREF r     -> pure (maybe (EREF r) EVAR (lookup r e))
    EVAR v     -> pure (maybe (EREF v) EVAR (lookup v e))
    EAPP f x   -> EAPP <$> go e f <*> go e x
    ELIN f     -> ELIN <$> go e f
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

optimizeLet :: (Int, IntMap CRes) -> Refr -> Expr -> Expr -> ExceptT Text IO CRes
optimizeLet s@(nex, tab) refNam expr body = do
  let recurRef = numRefs k expr > 0
      multiRef = numRefs k body >= 2
  if
    trivialExp expr || (not recurRef && not multiRef)
  then do
    v <- expBod s expr
    let s' = (nex, insertMap k v tab)
    b <- expBod s' body
    pure if (v.arity==0) then b{arity=0} else b
  else
    if not recurRef
    then do
      let s' = (nex+1, insertMap k (CR 0 (var nex) Nothing) tab)
      v <- expBod s' expr
      let s'' = (nex+1, insertMap k (CR 0 (var nex) v.inline) tab)
      b <- expBod s'' body
      pure CR { arity  = if (v.arity == 0 || b.arity == 0) then 0 else b.arity-1
              , code   = BLET v.code b.code
              , inline = Nothing
              }
          -- TODO Why b.arity-1?  Shouldn't it just be `b.arity`?
    else do
      let s' = (nex+1, insertMap k (CR 0 (var nex) Nothing) tab)
      v <- expBod s' expr
      b <- expBod s' body
      pure CR { arity  = if (v.arity == 0 || b.arity == 0) then 0 else b.arity-1
              , code   = BLET v.code b.code
              , inline = Nothing
              }
          -- TODO Why b.arity-1?  Shouldn't it just be `b.arity`?
  where
    k = refNam.key
    var = BVAR . fromIntegral

    -- Trivial if duplicating is cheaper than a let-reference.
    -- TODO: All atom literals should be considered trivial.
    trivialExp :: Exp a b -> Bool
    trivialExp = \case
        EVAL{} -> True
        EREF{} -> True
        EVAR{} -> True
        _      -> False


-- Lambda Lifting --------------------------------------------------------------


{-
    TODO Don't lift trivial aliases, just inline them (small atom, law)

        TODO: Doesn't the normal let-optimization pass already inline
        all of thoese?

    TODO If we lifted anything, need to replace self-reference with a
         new binding.
-}
lambdaLift :: Bool -> Func -> ExceptT Text IO Expr
lambdaLift = doLift
  where
    doLift pinned f@(FUN iline self tag arity body) = do
        let lifts = toList (freeVars f) :: [Refr]
        let liftV = EVAR <$> lifts
        let self' = self {key=234873455} -- TODO gensym
        let body' = EREC self (app (EVAR self') liftV)  body
        let funct = FUN iline self' tag (lifts <> arity) body'
        fan <- injectFun pinned funct
        pure $ app (EREF (G fan Nothing)) liftV

    app fn []     = fn
    app fn (x:xs) = app (EAPP fn x) xs

    injectFun :: Bool -> Func -> ExceptT Text IO Fan
    injectFun pinned (FUN _iline self nam arity exr) = do
        x <- expBod (nexVar, environ) exr
        let rul = RUL nam (fromIntegral ari) x.code
        let fan = ruleFanOpt $ fmap (.val) rul
        pure (if pinned then F.mkPin fan else fan)
      where
        ari = length arity
        nexVar = ari+1
        argKeys = (.key) <$> (self : toList arity)
        argRefs = (\k -> CR 0 k Nothing) . BVAR <$> [0..]
        environ = mapFromList (zip argKeys argRefs)

    freeVars :: forall b. Fun Refr b -> Set Refr
    freeVars = goFun mempty
     where
        goFun :: Set Refr -> Fun Refr b -> Set Refr
        goFun ours (FUN _ self _ arity body) =
          let keyz = setFromList (self : toList arity)
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
            ELIN f     -> go ours f
            EREC n v b -> let ours' = insertSet n ours
                          in go ours' v <> go ours' b
            ELET n v b -> let ours' = insertSet n ours
                          in go ours' v <> go ours' b


-- Inlining --------------------------------------------------------------------

inlineExp :: IntMap CRes -> Expr -> [Expr] -> ExceptT Text IO (Expr, [Expr])
inlineExp tab f xs =
    case f of
        ELAM _ l             -> doFunc l
        EREF (G _ Nothing)   -> noInline "Not inlinable"
        EREF (G _ (Just fn)) -> doFunc fn
        EVAR v               -> doVar v
        _                    -> unknownFunction
  where
    noInline = throwError

    unknownFunction =
        noInline "Head of ! expression is not a known function"

    doVar v =
        case (lookup v.key tab >>= (.inline)) of
            Nothing -> noInline "Not a function"
            Just fn -> doFunc fn

    doFunc (FUN _ self _ args body) =
        if numParams < arity || isRecursive
        then
            noInline "too few params or is recursive"
        else do
            res <- liftIO $ duplicateExpTop
                          $ foldr (uncurry ELET) body
                          $ zip args usedParams
            pure (res, extraParams)
      where
        usedParams  = take arity xs
        extraParams = drop arity xs
        isRecursive = numRefs self.key body > 0
        arity       = length args
        numParams   = length xs


-- Sire Expression to Law Body -------------------------------------------------

expBod :: (Int, IntMap CRes) -> Expr -> ExceptT Text IO CRes
expBod = flip go []
  where
    bap :: (Int, IntMap CRes) -> [Expr] -> CRes -> ExceptT Text IO CRes
    bap _ []        f = pure f
    bap s (xRaw:xs) f = do
        x <- go s [] xRaw

        let dynApp arity     = CR arity (BAPP f.code x.code) Nothing
        let cnsApp arity a b = CR arity (BCNS $ APP a b)     Nothing

        bap s xs $ case (f, x) of
              ( _               , CR{arity=0}     ) -> dynApp 0
              ( CR{arity=0}     , _               ) -> dynApp 0
              ( CR{arity=1}     , _               ) -> dynApp 0
              ( CR{code=BCNS a} , CR{code=BCNS b} ) -> cnsApp (f.arity - 1) a b
              ( _               , _               ) -> dynApp (f.arity - 1)

    go :: (Int, IntMap CRes) -> [Expr] -> Expr -> ExceptT Text IO CRes
    go s@(_nex, tab) xs xpr = do
      case xpr of
        EVAL b ->
            bap s xs CR{ arity  = fromIntegral (F.trueArity b)
                       , code   = BCNS $ REF $ G b Nothing
                       , inline = Nothing
                       }

        ELAM p f -> do
            lifted <- lambdaLift p (inlineTrivial tab f)
            bodied <- go s [] lifted
            bap s xs $ bodied{inline=Just f}

        EVAR r ->
            bap s xs $ fromMaybe (error ("Internal Error: `"
                                      <> show r
                                      <> "` missing."))
                     $ lookup r.key tab

        EREF (G t i) ->
            bap s xs CR{ arity  = fromIntegral (F.trueArity t)
                       , code   = BCNS $ REF $ G t i
                       , inline = i
                       }

        EAPP f x -> go s (x:xs) f

        ELIN (ELIN f) -> go s xs (ELIN f)

        ELIN f -> do
            lift (runExceptT $ inlineExp tab f xs) >>= \case
                Left _reason    -> go s xs f
                Right (e,extra) -> go s extra e

        EREC n v b -> optimizeLet s n v b >>= bap s xs
        ELET n v b -> optimizeLet s n v b >>= bap s xs

    {-
        Variables that simply rebind constant values are replaced by constant
        values.

        TODO: When does this matter?

        TODO: Doesn't the normal optimization pass eliminate these?

        TODO This only looks at `EVAR`, would be much shorter to write using
        `uniplate` or whatever.
    -}
    inlineTrivial :: IntMap CRes -> Func -> Func
    inlineTrivial tab (FUN iline self tag args body) =
        FUN iline self tag args (loop body)
      where
        loop = \case
            EVAL b     -> EVAL b
            EREF r     -> EREF r
            ELAM p f   -> ELAM p (inlineTrivial tab f)
            EAPP f x   -> EAPP (loop f) (loop x)
            ELIN f     -> ELIN (loop f)
            EREC n v b -> EREC n (loop v) (loop b)
            ELET n v b -> ELET n (loop v) (loop b)
            EVAR v     ->
                case lookup v.key tab of
                   Just CR{arity=0}     -> EVAR v -- TODO: does this matter?
                   Just CR{code=BCNS c} -> EVAL (valFan ((.val) <$> c))
                   _                    -> EVAR v


-- The Sire Compiler -----------------------------------------------------------

-- TODO: Why do we pass 1 in @expBod (1, mempty) expr@?
compileSire :: Map Symb Global -> XExp -> ExceptT Text IO Global
compileSire scope ast = do
    body <- resolveExp mempty ast
    expr <- traverse (getRef scope) body
    cRes <- expBod (1, mempty) (inlineGlobals expr)
    let fan = ruleFanOpt $ RUL (LN 0) 0
                         $ ((.val) <$> cRes.code)
    pure (G fan cRes.inline)
  where
    getRef :: Map Symb Global -> Symb -> ExceptT Text IO Global
    getRef env nam = do
        maybe (unresolved nam) pure (lookup nam env)

    unresolved :: Symb -> ExceptT Text IO a
    unresolved nam =
        throwError ("Unresolved Reference: " <> showSymb nam)
