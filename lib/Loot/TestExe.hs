{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.TestExe (main, validateLoot) where

import Fan
import PlunderPrelude
import Rex
import Rex.TestUtils
import Test.Tasty

import Fan.Convert
import Fan.Save         (getPinHash)
import GHC.IO.Handle    (hFlushAll)
import Loot.Types       (Symb)
import System.Directory (removeFile)

import qualified Loot.ReplExe as Repl

--------------------------------------------------------------------------------

devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)

main :: IO ()
main = do
    putStrLn "TODO: update Loot tests."
    when False $
        let ?rexColors = NoColors
        in do vEnv <- liftIO $ newIORef mempty
              langTest ".loot" (goldenLoot vEnv)

cab,cabHash :: Nat
cab = utf8Nat "_"
cabHash = utf8Nat "_hash"

goldenLoot :: RexColor => IORef (Map Symb Fan) -> GoldPaths -> TestTree
goldenLoot vEnv pax = do
    runTest pax doBlk end
  where
    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk _fil _h _firstLn blk = Repl.runBlock devNull False vEnv blk

    GOLD_PATHS{gpSource,gpOutput} = pax

    end :: Handle -> IO ()
    end h = do
        env <- readIORef vEnv
        pln <- pure $ fromMaybe (NAT 0) $ lookup cab env
        has <- mkPin' pln >>= getPinHash
        Repl.printValue h False (Just cab) pln
        Repl.printValue h True (Just cabHash) (toNoun has)
        hFlushAll h >> hClose h
        validateLoot gpOutput (gpSource <> ".tmp")

validateLoot :: RexColor => FilePath -> FilePath -> IO ()
validateLoot lootFile tempFile = do
    vEnv <- liftIO (newIORef mempty)
    Repl.replFile lootFile (Repl.runBlock devNull False vEnv)
    h <- openFile tempFile WriteMode
    env <- readIORef vEnv
    pln <- pure $ fromMaybe (NAT 0) $ lookup cab env
    has <- mkPin' pln >>= getPinHash
    Repl.printValue h False (Just cab) pln
    Repl.printValue h True (Just cabHash) (toNoun has)
    hFlushAll h >> hClose h
    srcText <- readFileUtf8 lootFile
    tmpText <- readFileUtf8 tempFile
    unless (srcText == tmpText) do
        putStrLn ("Re-Processing output produces different result")
        putStrLn "To show the difference, run:\n"
        putStrLn $ concat
            [ "    diff --color '"
            , pack lootFile
            , "' '"
            , pack tempFile
            , "'"
            ]
        error "Roundtrip Failed"
    removeFile tempFile
