{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Inline (inlineGlobals, inlineExp) where

import PlunderPrelude
import Sire.Types

import Sire.Compile.Common (duplicateExpTop, numRefs)
import Sire.Compile.Types  (Expr, Func, Global(..), Refr(..))


-- Inline Application ----------------------------------------------------------

{-# INLINE inlineExp #-}
inlineExp :: (Int -> Maybe Func) -> [Expr] -> Expr -> IO (Either Text (Expr, [Expr]))
inlineExp varFunc xs = \case
    ELAM _ l -> fn l
    EREF g   -> maybe who fn g.inliner
    EVAR v   -> maybe who fn (varFunc v.key)
    _        -> who
  where
    who = pure $ Left "Not a known function"

    fn func =
        if (numParams < arity || isRecursive)
        then
            pure $ Left "too few params or is recursive"
        else
            fmap (Right . (,extraParams))
                $ duplicateExpTop
                $ foldr (uncurry ELET) func.body
                $ zip func.args usedParams
      where
        usedParams  = take arity xs
        extraParams = drop arity xs
        isRecursive = numRefs func.self.key func.body > 0
        arity       = length func.args
        numParams   = length xs


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
