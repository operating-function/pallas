{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.TestExe (main) where

import PlunderPrelude
import Rex
import Rex.TestUtils
import Test.Tasty

import Data.Text.IO (hPutStr, hPutStrLn)

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
    doBlk fil h firstLn (BLK _pos tex blk) = do
        unless firstLn (hPutStrLn h "")
        hPutStr h $ case blk of Left err -> dent "!!" err
                                Right vl -> verifyBlock fil tex vl

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
