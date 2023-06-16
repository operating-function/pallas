-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.ReplExe (main) where

import PlunderPrelude
import Rex

--------------------------------------------------------------------------------

main :: IO ()
main = colorsOnlyInTerminal do
    f <- getArgs <&> \case
        "--open":_ -> forceOpen
        "--nest":_ -> forceNest
        _          -> id

    replStdin \case
        BLK _ _ (Left err) -> putStrLn (dent "!!" $ err)
        BLK _ _ (Right rx) -> putStrLn (rexFile $ f rx)

forceOpen :: Rex -> Rex
forceOpen = go
  where
    go (N _ r cs k) = N OPEN r (go <$> cs) (go <$> k)
    go (T th t k)   = T th t (go <$> k)
    go (C c)        = absurd c

forceNest :: Rex -> Rex
forceNest = go
  where
    go (N _ r cs k) = N NEST_PREFIX r (go <$> cs) (go <$> k)
    go (T th t k)   = T th t (go <$> k)
    go (C c)        = absurd c

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln
