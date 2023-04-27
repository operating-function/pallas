{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Common
    ( Inliner
    , Refr(..)
    , Global(..)
    , Expr
    , Func
    , showSymb
    , gensym
    , duplicateExpTop
    , numRefs
    )
where

import PlunderPrelude
import Sire.Types
import Sire.Compile.Types (Inliner, Refr(..), Global(..), Expr, Func, showSymb)

import Control.Monad.State (StateT(..), evalStateT, get)
import Optics.Zoom         (zoom)


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

refreshRef :: Refr -> StateT (Int, Map Int Refr) IO Refr
refreshRef ref =
    (lookup ref.key . snd <$> get) >>= \case
        Just r  -> pure r
        Nothing -> pure ref

refreshBinder :: Refr -> StateT (Int, Map Int Refr) IO Refr
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

duplicateFun :: Fun Refr b -> StateT (Int, Map Int Refr) IO (Fun Refr b)
duplicateFun (FUN iline self name args body) = do
    self' <- refreshBinder self
    args' <- traverse refreshBinder args
    body' <- duplicateExp body
    pure (FUN iline self' name args' body')

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
        ELIN x     -> ELIN <$> go x
        EVAL{}     -> pure expr
        EREF{}     -> pure expr

duplicateExpTop :: Exp Refr b -> IO (Exp Refr b)
duplicateExpTop e = evalStateT (duplicateExp e) (0, mempty)


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
