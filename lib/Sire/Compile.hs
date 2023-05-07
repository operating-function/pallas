-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile (compileSire, module X) where

import Loot.Backend
import PlunderPrelude
import Sire.Compile.Common
import Sire.Types

import Control.Monad.Except   (ExceptT(..))
import Loot.Types             (Bod(..), LawName(LN), Rul(..), Val(..))
import Sire.Compile.Resolve   (resolveExp)
import Sire.Compile.Transform (freeVars, transformSire)

import qualified Fan                as F
import qualified Sire.Compile.Types as X


-- Internal Types --------------------------------------------------------------

{-
    The compilation result:

    -   [args]: If the expression is a constant, this is the number
        of extra arguments that can be applied before the expression is
        no longer constant.  If the expression is not constant, this is
        set to 0.

    -   [code]: The actual PLAN law-code that this compiles into.
-}
data CRes = CR
    { arity  :: Nat
    , code   :: Bod Global
    }
  deriving (Eq, Show)


-- Sire Expression to Law Body -------------------------------------------------

{-
    This compiles a nested function directly into a PLAN value.

    We assume that the input contains no free variables, a property
    that is guarenteed by `transformSire`.
-}
compileFun :: Bool -> Func -> ExceptT Text IO Fan
compileFun pinned f = do
    unless (null $ freeVars f) do
        throwError "Internal Error: optimization did not lift all lambdas"
    x <- expBod (nextVar, environ) f.body
    let rul = RUL f.lawTag (fromIntegral numArgs) x.code
    let fan' = ruleFanOpt $ fmap (.val) rul
    pure (if pinned then F.mkPin fan' else fan')
  where
    numArgs = length f.args
    nextVar = numArgs+1
    argKeys = (.key) <$> (f.self : toList f.args)
    argRefs = (\k -> CR 0 k) . BVAR <$> [0..]
    environ = mapFromList (zip argKeys argRefs)

{-
    This compiles a Sire expression into PLAN legal code.  If the result
    is a constant value, we track the arity, otherwise we set arity=0.

    We use the arity information to optimize code size.  For example,
    the input expression `(0 0)` could compile to this:

        (0 (2 0) (2 0))

    Or it could compile to this:

        (2 (0 0))

    The latter is preferable, but this merger is only possible because
    the head (0) has arity>1, otherwise we would be moving evaluation
    from runtime to compile time.

    TODO: This would actually be a lot simpler if we could cut Loot.Val
    out of the loop and just use Fan values.  In particular, there expBod
    would no longer need to track arities, since we can just use
    `F.trueArity` directly.
-}
expBod :: (Int, IntMap CRes) -> Expr -> ExceptT Text IO CRes
expBod = go
  where
    bapp :: CRes -> CRes -> CRes
    bapp f x =
        case (f.code, x.code) of
            (BCNS a, BCNS b) | f.arity>1 -> cnsApp (f.arity - 1) a b
            _                            -> dynApp (f.arity - 1)
      where
        dynApp arity     = CR arity (BAPP f.code x.code)
        cnsApp arity a b = CR arity (BCNS $ APP a b)

    go :: (Int, IntMap CRes) -> Expr -> ExceptT Text IO CRes
    go s@(_nex, tab) = \case
        EVAL b ->
            pure CR{arity,code}
          where
            arity = fromIntegral (F.trueArity b)
            code  = BCNS $ REF $ G b Nothing

        ELAM p f -> do
            fan <- compileFun p f
            go s (EVAL fan)

        EVAR r -> do
            let err = throwError ("Internal Error: broken ref: " <> tshow r)
            maybe err pure (lookup r.key tab)

        EREF (G t i) ->
            pure CR{arity,code}
          where arity  = fromIntegral (F.trueArity t)
                code   = BCNS $ REF $ G t i

        EAPP f x   -> bapp <$> go s f <*> go s x
        ELIN f     -> go s f
        EREC n v b -> goRec s n v b
        ELET n v b -> goLet s n v b

    goLet :: (Int, IntMap CRes) -> Refr -> Expr -> Expr -> ExceptT Text IO CRes
    goLet (nex, tab) bind expr body = do
        v <- expBod (nex+1, tab) expr
        let val = CR { arity = v.arity, code = BVAR (fromIntegral nex) }
        let env = (nex+1, insertMap bind.key val tab)
        b <- expBod env body
        pure CR { arity = b.arity, code = BLET v.code b.code }

    goRec :: (Int, IntMap CRes) -> Refr -> Expr -> Expr -> ExceptT Text IO CRes
    goRec (nex, tab) bind expr body = do
        let val = CR{ arity=0, code=BVAR (fromIntegral nex) }
        let env = (nex+1, insertMap bind.key val tab)
        v <- expBod env expr
        b <- expBod env body
        pure CR { arity = b.arity, code = BLET v.code b.code }


-- The Sire Compiler -----------------------------------------------------------

{-
    We start with @expBod (1, mempty)@ because 1 is the next variable
    number to use when generating PLAN legal code.

    We are compiling the expression as a zero-argument function, so var
    #0 is a self-reference, and var #1 is the code for the first binding.
-}

compileSire :: Map Symb Global -> XExp -> ExceptT Text IO Global
compileSire scope ast = do
    body <- resolveExp mempty ast
    eRaw <- traverse (getRef scope) body
    expr <- transformSire eRaw
    cRes <- expBod (1, mempty) expr
    pure $ G { val     = ruleFanOpt $ RUL (LN 0) 0 ((.val) <$> cRes.code)
             , inliner = getInliner expr
             }
  where
    getRef :: Map Symb Global -> Symb -> ExceptT Text IO Global
    getRef env nam = do
        maybe (unresolved nam) pure (lookup nam env)

    unresolved :: Symb -> ExceptT Text IO a
    unresolved nam =
        throwError ("Unresolved Reference: " <> showSymb nam)

    getInliner (ELIN x)   = getInliner x
    getInliner (ELAM _ f) = Just f
    getInliner (EREF g)   = g.inliner
    getInliner _          = Nothing
