{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Inline
    ( markThingsToBeInlined
    , inlineExp
    , inlineAll
    )
where

import PlunderPrelude
import Sire.Types

import Sire.Compile.Common (duplicateExpTop, numRefs, apple)
import Sire.Compile.Types  (Expr, Func, Global(..), Refr(..))

import Control.Monad.State


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
    Things that should be auto-inlined.

    1)  Reference to marked global bindings.

        | *else 3

    2)  Direct calls to marked functions

        | (**_ x & x) 3

    3)  Referenced to locally-bound marked functions

        @ I (**I x ? x)
        | I 3
-}
markThingsToBeInlined :: Expr -> Expr
markThingsToBeInlined = go mempty []
  where
    mark :: Expr -> Expr
    mark (ELIN x) = mark x
    mark x        = ELIN x

    go :: IntSet -> [Expr] -> Expr -> Expr
    go m xs = \case
        x@EVAL{}   -> apple x xs
        EAPP f x   -> go m (go m [] x : xs) f
        EREC v x b -> apple (EREC v (go m [] x) (go m [] b)) xs
        ELET v x b -> apple (goLet m v x b) xs
        EVAR r     -> apple (goVar m r) xs
        ELIN f     -> apple (mark $ go m [] f) xs
        EREF g     -> apple (goRef g) xs
        ELAM p fun -> apple (goLam m p fun) xs

    funIsMarked (FUN{inlinePls=True}) = True
    funIsMarked _                     = False

    globalIsMarked = maybe False funIsMarked . (.inliner)

    goVar marks r | member r.key marks = ELIN (EVAR r)
    goVar _     r | otherwise          = EVAR r

    goRef g | globalIsMarked g = ELIN (EREF g)
    goRef g | otherwise        = EREF g

    goLam m p fun =
        if funIsMarked fun then ELIN e else e
      where
        e = ELAM p fun{ body = go m [] fun.body }

    -- First we process the binding normally.  If the binding is something
    -- that should be inlined, then references to the bindings are
    -- something that should be inlined.
    goLet m r v b =
        ELET r v' (go m' [] b)
      where v' = go m [] v
            m' = case v' of { ELIN{} -> insertSet r.key m; _ -> m }


-- Inline All ------------------------------------------------------------------

{-
    Inlines every (**f x1 .. xn) where arity(f)=n and f is one of these:

    -   A function literal.
    -   A reference to a global function.
    -   A reference to a local function.
-}
inlineAll :: Expr -> IO (Expr, Int)
inlineAll topExpr =
     runStateT (go mempty [] topExpr) 0
  where
    go :: IntMap Expr -> [Expr] -> Expr -> StateT Int IO Expr
    go tab xs = \case
        x@EVAL{}    -> pure $ apple x xs
        x@EREF{}    -> pure $ apple x xs
        x@EVAR{}    -> pure $ apple x xs
        EAPP f x    -> do xv <- go tab [] x; go tab (xv : xs) f
        ELET v x b  -> doLet tab ELET v x b <&> (`apple` xs)
        EREC v x b  -> doLet tab EREC v x b <&> (`apple` xs)
        ELAM p f    -> do b <- go tab [] f.body; pure (apple (ELAM p f{body=b}) xs)
        ELIN ELIN{} -> error "Internal Error:  doubled inline marker"
        ELIN xRaw   -> do
             eRes <- liftIO (inlineExp (findInliner tab) xs xRaw)
             case eRes of
                 Right (e,extra) -> do
                     modify' succ
                     go tab extra e
                 Left _reason    -> do
                     x <- go tab [] xRaw
                     pure (apple (ELIN x) xs)

    doLet tabRaw con v valRaw bodyRaw = do
        val <- go tabRaw [] valRaw
        tab <- pure (insertMap v.key val tabRaw)
        bod <- go tab [] bodyRaw
        pure (con v  val bod)

    findInliner :: IntMap Expr -> Int -> Maybe Func
    findInliner tab key = do
        lookup key tab >>= loop
      where
        loop = \case
            ELIN x   -> loop x
            ELAM _ f -> pure f
            EVAR v   -> findInliner tab v.key
            EREF v   -> v.inliner
            _        -> Nothing
