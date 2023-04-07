{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex
    ( module X
    , RexColor
    , RexColorScheme(..)
    , RuneShape(..)
    , TextShape(..)
    , Leaf
    , GRex(..)
    , Rex
    , rexFile
    , rexFileBuilder
    , rexLine
    , rexLineBuilder
    , colorsOnlyInTerminal
    )
where

import ClassyPrelude

import Rex.Block as X
import Rex.Print
import Rex.ReadT as X
import Rex.Types

import qualified System.Console.Terminal.Size as TSize

--------------------------------------------------------------------------------

isConsole :: IO Bool
isConsole =
    TSize.size @Int >>= \case
        Nothing -> pure False
        Just _  -> pure True

colorsOnlyInTerminal :: (RexColor => IO a) -> IO a
colorsOnlyInTerminal act = do
    colors <- isConsole <&> bool NoColors BoldColors
    (let ?rexColors = colors in act)
