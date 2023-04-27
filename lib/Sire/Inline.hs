{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Inline (inlineGlobals) where

import PlunderPrelude
import Sire.Types

import Sire.Compile.Types (Expr, Global(..))


-- Inlining --------------------------------------------------------------------

{-
    This can produce doubled ELIN markers, but it will be cleaned-up
    later.
-}
inlineGlobals :: Expr -> Expr
inlineGlobals = go []
  where
    apple :: Exp a b -> [Exp a b] -> Exp a b
    apple f []    = f
    apple f (b:c) = apple (EAPP f b) c

    go :: [Expr] -> Expr -> Expr
    go xs = \case
        x@EVAR{}    -> apple x xs
        x@EVAL{}    -> apple x xs
        EAPP f x    -> go (go [] x : xs) f
        ELAM p fun  -> apple (ELAM p fun{body=(go [] fun.body)}) xs
        ELET v x b  -> apple (ELET v (go [] x) (go [] b)) xs
        EREC v x b  -> apple (EREC v (go [] x) (go [] b)) xs
        ELIN f      -> apple (ELIN (go [] f)) xs
        EREF g      ->
            case g.inliner of
                Just FUN{inlinePls=True} -> apple (ELIN $ EREF g) xs
                _                        -> apple (EREF g) xs
