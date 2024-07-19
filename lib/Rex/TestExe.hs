-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.TestExe (main) where

import PlunderPrelude
import Rex
import Rex.TestUtils
import Rex.Policy
import Rex.Mechanism
import Test.Tasty

import Data.Text.IO (hPutStrLn)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "TODO: update rex tests."
    when False $
        let ?rexColors = NoColors
        in langTest ".rex" goldenRex

goldenRex :: RexColor => GoldPaths -> TestTree
goldenRex pax = do
    runTest pax doBlk (const $ pure ())
  where
    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk fil h firstLn blk = do
        unless firstLn (hPutStrLn h "")
        putStr case blk.errors of
                 []  -> verifyBlock fil (unlines $ decodeUtf8 . (.byt) <$> blk.lines) blk.rex
                 e:_ -> dent "!!" e

verifyBlock :: RexColor => FilePath -> Text -> Rex -> Text
verifyBlock fil inp rex =
  let out = rexFile rex
  in case parseBlocks fil out of
       Right [r] | r==rex -> out
       Right rs           -> dent "!!" $ noRound inp rs
       Left err           -> dent "!!" $ roundErr inp err

noRound :: RexColor => Text -> [Rex] -> Text
noRound inp [bar] =
  unlines [ "This block:\n"
          , inp
          , "\nBecomes this when pretty-printed and re-parsed\n"
          , rexFile bar
          ]
noRound inp [] =
  unlines [ "This block:\n"
          , inp
          , "\nBecomes nothing when pretty-printed and re-parsed."
          ]
noRound inp bars =
  unlines [ "This block:\n"
          , inp
          , "\nBecomes these blocks when pretty-printed and re-parsed\n"
          , intercalate "\n\n" (rexFile <$> bars)
          ]

roundErr :: RexColor => Text -> Text -> Text
roundErr inp err =
  unlines [ "This input:\n"
          , inp
          , "\nGets this error when pretty-printed and re-parsed\n"
          , err
          ]
