{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Inline (inlineGlobals) where

import PlunderPrelude
import Sire.Types

import Sire.Compile.Types (Expr, Global(..))


-- Inlining --------------------------------------------------------------------

inlineGlobals :: Expr -> Expr
inlineGlobals = go []
  where
    apple :: Exp a b -> [Exp a b] -> Exp a b
    apple f []    = f
    apple f (b:c) = apple (EAPP f b) c

    go :: [Expr] -> Expr -> Expr
    go xs = \case
        x@EVAR{}       -> apple x xs
        x@EVAL{}       -> apple x xs
        EAPP f xRaw    -> go (go [] xRaw : xs) f
        EREF g         -> attempt g xs
        ELAM p fun     -> apple (ELAM p fun{body=(go [] fun.body)}) xs
        ELET v x b     -> apple (ELET v (go [] x) (go [] b)) xs
        EREC v x b     -> apple (EREC v (go [] x) (go [] b)) xs
        ELIN (f :| ys) -> apple (ELIN (go [] f :| fmap (go []) ys)) xs

    attempt :: Global -> [Expr] -> Expr
    attempt g xs =
        case g.inliner of
            Just f@FUN{inlinePls=True} -> tryIt f
            _                          -> fallback
      where
        fallback = apple (EREF g) xs

        tryIt f =
            case compare numGiven numArgs of
                LT -> fallback
                GT -> apple (attempt g inlineParams) extraParams
                EQ -> ELIN (EREF g :| xs)
          where
            numArgs      = length f.args
            numGiven     = length xs
            inlineParams = take numArgs xs
            extraParams  = drop numArgs xs
