-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{- |

    This is a single sire-to-sire transformation whose output has these
    properties:

    -   No function has free variables, including nested lambdas.

            x&(y&x)  ==>  x & ((x y & x) x)

    -   All let bindings are referenced at least twice.

            (x@(add 2 3))(inc x)  ==> (inc (add 2 3))

    -   There are no let bindings whose value expressions are constants.

            (x@3)[x x]  ==>  [3 3]

    -   All requested inline-applications have been performed.

            (**I (inc 3))                 ==>  (inc 3)

            (else 3)                      ==>  (inc 3)

            (I @ (**I x ? x))(I (inc 3))  ==>  (inc 3)

    -   There are no trivial rebindings:

            (i@inc)(x @ i 3)(y@x)(add y y)

                ==>

            (x @ inc 3)(add x x)

-}

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Transform (freeVars, transformSire) where

import PlunderPrelude
import Sire.Compile.Common
import Sire.Compile.Inline
import Sire.Types

import Control.Monad.Except (ExceptT(..))


-- Lambda Lifting --------------------------------------------------------------

{-
    To lambda lift a function

    1)  Walk the body and lambda lift each lambda found there.

    2)  Calculate the set of free variables

    ... TODO
-}
lambdaLift :: Expr -> Expr
lambdaLift = go
  where
    go :: Expr -> Expr
    go = \case
        ELAM p f   -> liftFunc p (f { body = go f.body })
        EAPP f x   -> EAPP (go f) (go x)
        ELIN x     -> ELIN (go x)
        ELET r v b -> ELET r (go v) (go b)
        EREC r v b -> EREC r (go v) (go b)
        x          -> x

    liftFunc :: Bool -> Func -> Expr
    liftFunc pinned fun =
        if null free
        then ELAM pinned fun
        else apple dupFunc newPara
      where
        dupFunc = ELAM pinned $ unsafePerformIO (duplicateFunShallow newFunc)
        newSelf = unsafePerformIO (gensym fun.self.name)
        selfRef = EVAR newSelf
        newPara = EVAR <$> free
        newBody = ELET fun.self (apple selfRef newPara) fun.body
        newArgs = free <> fun.args
        newFunc = fun { self=newSelf, args=newArgs, body=newBody }

        free :: [Refr]
        free = freeVars fun

freeVars :: Func -> [Refr]
freeVars =
    ordNub . goFun mempty
 where
    goFun :: IntSet -> Fun Refr b -> [Refr]
    goFun e f = go (e <> new) f.body
      where new = setFromList (f.self.key : toList ((.key) <$> f.args))

    go :: IntSet -> Exp Refr b -> [Refr]
    go e = \case
        EVAL{}     -> []
        EREF{}     -> []
        EVAR r     -> if (r.key `elem` e) then [] else [r]
        EAPP f x   -> go e f <> go e x
        ELIN f     -> go e f
        EREC n v b -> let e' = insertSet n.key e in go e' v <> go e' b
        ELET n v b -> let e' = insertSet n.key e in go e' v <> go e' b
        ELAM _ f   -> goFun e f


-- Basic Usage Analysis --------------------------------------------------------

{-
    This ignores self-references to cyclic values.
-}
allRefs :: Expr -> [Refr]
allRefs = go
  where
    go :: Expr -> [Refr]
    go = \case
        EVAR r     -> [r]
        EVAL{}     -> []
        EREF{}     -> []
        EAPP x y   -> go x <> go y
        ELIN x     -> go x
        ELAM _ fun -> go fun.body
        ELET _ v b -> go v <> go b
        EREC r v b -> (filter (/= r) $ go v) <> go b

{-
    Reference-counts within the current function body, but not including
    references that come from nested functions.

    This result is compared to the globalRefCounts result to determine
    if a binding is referenced from within a nested lambda.  We don't
    want to inline single-use let bindings because that can cause an
    expression to evaluated more times.
-}
localRefs :: Expr -> [Refr]
localRefs = go
  where
    go :: Expr -> [Refr]
    go = \case
        EVAR r     -> [r]
        EVAL{}     -> []
        EREF{}     -> []
        EAPP x y   -> go x <> go y
        ELIN x     -> go x
        ELAM _ _   -> []
        ELET _ v b -> go v <> go b
        EREC r v b -> filter (/= r) (go v) <> go b


globalRefCounts :: Expr -> IntMap Int
globalRefCounts e = foldr f mempty ((.key) <$> allRefs e)
  where f = alterMap (Just . maybe 1 succ)

localRefCounts :: Expr -> IntMap Int
localRefCounts e = foldr f mempty ((.key) <$> localRefs e)
  where f = alterMap (Just . maybe 1 succ)


-- Eliminate Simple Rebinds ----------------------------------------------------

eliminateRedundantBindings :: Expr -> Expr
eliminateRedundantBindings = go mempty
  where
    go :: IntMap Refr -> Expr -> Expr
    go e = \case
        x@EVAL{}   -> x
        x@EREF{}   -> x
        EAPP f x   -> EAPP (go e f) (go e x)
        ELAM p f   -> ELAM p f{body = go e f.body}
        ELIN x     -> ELIN (go e x)
        EVAR r     -> EVAR (fromMaybe r (lookup r.key e))
        EREC v x b -> doLet e EREC v x b
        ELET v x b -> doLet e ELET v x b

    doLet e con v letVal b =
        case getRebind e letVal of
            Nothing -> con v (go e letVal) (go e b)
            Just x  -> go (insertMap v.key x e) b

    getRebind :: IntMap Refr -> Expr -> Maybe Refr
    getRebind e = \case
        EVAR r -> pure $ fromMaybe r $ lookup r.key e
        _      -> Nothing


-- Eliminate Binds to constant values ------------------------------------------

eliminateConstantBindings :: Expr -> Expr
eliminateConstantBindings = go mempty
  where
    go :: IntMap Expr -> Expr -> Expr
    go e = \case
        x@EVAL{}   -> x
        x@EREF{}   -> x
        EAPP f x   -> EAPP (go e f) (go e x)
        ELAM p f   -> ELAM p f{body = go e f.body}
        ELIN x     -> ELIN (go e x)
        x@(EVAR r) -> maybe x id (lookup r.key e)
        EREC v x b -> doLet e EREC v x b
        ELET v x b -> doLet e ELET v x b

    doLet e con v letVal b =
        case getConstant e letVal of
            Nothing -> con v (go e letVal) (go e b)
            Just x  -> go (insertMap v.key x e) b

    getConstant :: IntMap Expr -> Expr -> Maybe Expr
    getConstant e = \case
        x@EVAL{}   -> pure x
        x@EREF{}   -> pure x
        EAPP{}     -> Nothing
        ELET{}     -> Nothing
        EREC{}     -> Nothing
        ELAM{}     -> Nothing
        ELIN x     -> getConstant e x
        x@(EVAR r) -> Just $ fromMaybe x $ lookup r.key e


-- Eliminate bindings that are never referenced. -------------------------------

eliminateUnusedBindings :: Expr -> Expr
eliminateUnusedBindings topExpr =
    go topExpr
  where
    counts = globalRefCounts topExpr

    isUnused r = not (member r.key counts)

    go = \case
        ELET r v b -> if isUnused r then go b else ELET r (go v) (go b)
        EREC r v b -> if isUnused r then go b else EREC r (go v) (go b)
        x@EVAL{}   -> x
        x@EVAR{}   -> x
        x@EREF{}   -> x
        EAPP x y   -> EAPP (go x) (go y)
        ELAM p f   -> ELAM p f{body = go f.body}
        ELIN x     -> ELIN (go x)

-- Eliminate bindings that are used exactly once (except nested functions) -----

{-
    When is a binding single-use?

    When it is referenced exactly once.

    Can we always eliminate these?

    No.  Only if the reference is used from the same function it was
    defined within.

    I suppose we can calculate a local refcount and a global refcount,
    the optimization applies only if they are both the same.
-}

{-
    This eliminates let-bindings that are used exactly once (and not
    referenced from a nested function).
-}
eliminateSingleUseBindings :: Expr -> Expr
eliminateSingleUseBindings topExpr =
    goBody topExpr
  where
    global = globalRefCounts topExpr

    goBody :: Expr -> Expr
    goBody body =
        go mempty body
      where
        local :: IntMap Int
        local = localRefCounts body

        isSingleUse :: Int -> Bool
        isSingleUse k =
            (Just 1 == lookup k global) && (Just 1 == lookup k local)

        go :: IntMap Expr -> Expr -> Expr
        go e = \case
          EREC r v b -> EREC r (go e v) (go e b)
          x@EVAL{}   -> x
          x@EREF{}   -> x
          x@(EVAR r) -> fromMaybe x (lookup r.key e)
          EAPP x y   -> EAPP (go e x) (go e y)
          ELIN x     -> ELIN (go e x)
          ELAM p f   -> ELAM p f{body = goBody f.body}
          ELET r v b ->
              if not (isSingleUse r.key)
              then ELET r (go e v) (go e b)
              else go e' b
                where v' = go e v
                      e' = insertMap r.key v' e


-- Sire Transformation ---------------------------------------------------------

{-
    TODO: Is this loop needed?  If not, can we prove that?
-}
optimize :: Expr -> IO Expr
optimize = go 0
  where
    go :: Int -> Expr -> IO Expr
    go n e0 = do
        e1       <- pure (markThingsToBeInlined e0)
        (e2, il) <- inlineAll e1
        e3       <- pure (eliminateConstantBindings e2)
        e4       <- pure (eliminateRedundantBindings e3)
        e5       <- pure (eliminateUnusedBindings e4)
        e6       <- pure (eliminateSingleUseBindings e5)

        if il > 0
        then go (n+1) e6
        else pure e6

transformSire :: Expr -> ExceptT Text IO Expr
transformSire e0 = do
    e1 <- liftIO $ optimize e0
    e2 <- pure   $ lambdaLift e1
    e3 <- liftIO $ optimize e2
    pure e3
