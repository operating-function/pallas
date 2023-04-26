{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Types
    ( Inliner
    , Refr(..)
    , Global(..)
    , Expr
    , Func
    , showSymb            -- TODO: Move out
    , gensym              -- TODO: Move out
    , duplicateExpTop     -- TODO: Move out
    )
where

import Loot.Backend
import PlunderPrelude
import Rex
import Sire.Types

import Control.Monad.State (StateT(..), evalStateT, get)
import Loot.Syntax         (symbRex)
import Optics.Zoom         (zoom)

import qualified Fan as F


-- Utils -----------------------------------------------------------------------

showSymb :: Symb -> Text
showSymb =
    let ?rexColors = NoColors
    in rexLine . symbRex


-- Types -----------------------------------------------------------------------

type Expr = Exp Refr Global
type Func = Fun Refr Global

type Inliner = Maybe Func

data Global = G
    { val     :: Fan
    , inliner :: Inliner
    }
  deriving (Generic, Eq)

instance Show Global where
  show (G (F.NAT n) _) = "(AT " <> show n <> ")"
  show (G pln _)       = "(G " <> show (F.valName pln) <> ")"

data Refr = REFR
    { name :: !Symb
    , key  :: !Int
    }

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
        ELIN xs    -> ELIN <$> traverse go xs
        EVAL{}     -> pure expr
        EREF{}     -> pure expr

duplicateExpTop :: Exp Refr b -> IO (Exp Refr b)
duplicateExpTop e = evalStateT (duplicateExp e) (0, mempty)
