-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.TestUtils
    ( GoldPaths(..)
    , findGoldenTests
    , langTest
    , dent
    , runTest
    )
where

import PlunderPrelude
import Test.Tasty

import Rex               (Block, replFile)
import System.Directory  (listDirectory)
import Test.Tasty.Golden (goldenVsFile)

import qualified Data.Text as T

--------------------------------------------------------------------------------

data GoldPaths = GOLD_PATHS
    { gpFileNm :: FilePath
    , gpSource :: FilePath
    , gpOutput :: FilePath
    , gpGolden :: FilePath
    }

findGoldenTests :: Text -> IO [GoldPaths]
findGoldenTests end = do
    mapMaybe getBase . sort <$> listDirectory "tests/"
  where
    endStr = unpack end

    getBase :: FilePath -> Maybe GoldPaths
    getBase (pack -> fn) =
        if end `T.isSuffixOf` fn
        then Just $ mkGoldPaths $ take (T.length fn - T.length end) fn
        else Nothing

    mkGoldPaths :: Text -> GoldPaths
    mkGoldPaths (unpack -> base) = GOLD_PATHS{..}
      where
        gpFileNm = base <> endStr
        gpSource = "tests/" <> base <> endStr
        gpOutput = "tests/" <> base <> endStr <> ".out"
        gpGolden = "tests/" <> base <> endStr <> ".golden"

langTest :: Text -> (GoldPaths -> TestTree) -> IO ()
langTest end exe = do
    files <- findGoldenTests end
    defaultMain $ testGroup "Golden Tests"
                $ exe <$> files

dent :: Text -> Text -> Text
dent pre =
    unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln

runTest
    :: GoldPaths
    -> (FilePath -> Handle -> Bool -> Block -> IO ())
    -> (Handle -> IO ())
    -> TestTree
runTest GOLD_PATHS{..} act end =
    goldenVsFile gpFileNm gpGolden gpOutput $ do
        vFirst <- newIORef True
        withFile gpOutput WriteMode \h -> do
            replFile gpSource \b -> do
                firstLn <- readIORef vFirst
                act gpFileNm h firstLn b
                writeIORef vFirst False
            end h
