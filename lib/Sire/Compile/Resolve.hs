-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Resolve (resolveExp) where

import PlunderPrelude
import Sire.Types

import Sire.Compile.Common (Refr(..), gensym, numRefs)

import qualified Data.Map as M


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
    EREC n v b -> goRec n v b
    ELET n v b -> goLet n v b
  where
    go = resolveExp

    goRec n v b = do
        r <- gensym n
        let e2 = insertMap n r e
        v2 <- go e2 v
        if numRefs r.key v2 > 0
        then EREC r v2 <$> go e2 b
        else ELET r v2 <$> go e2 b

    goLet n v b = do
        r <- gensym n
        let e2 = insertMap n r e
        ELET r <$> go e v <*> go e2 b
