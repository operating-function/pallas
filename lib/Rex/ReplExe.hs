-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.ReplExe (main) where

import PlunderPrelude
import Rex
import Rex.Policy (Block(..))

--------------------------------------------------------------------------------

main :: IO ()
main = colorsOnlyInTerminal do
    f <- getArgs <&> \case
        "--open":_ -> forceOpen
        "--nest":_ -> forceNest
        _          -> id

    replStdin \(blk :: Block) ->
        if null blk.errors then
            putStrLn (rexFile $ f blk.rex)
        else
            for_ blk.errors \e -> do
                putStrLn (dent "!!" $ e)

forceOpen :: Rex -> Rex
forceOpen = go
  where
    go (N _ r cs k) = N OPEN r (go <$> cs) (go <$> k)
    go (T th t k)   = T th t (go <$> k)
    go (C c)        = absurd c

forceNest :: Rex -> Rex
forceNest = go
  where
    go (N _ r cs k) = N NEST r (go <$> cs) (go <$> k)
    go (T th t k)   = T th t (go <$> k)
    go (C c)        = absurd c

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln
