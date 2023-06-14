-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.TestExe (main) where

import Fan
import Fan.Save
import Jelly.Types    (hashToByteString)
import PlunderPrelude
import Rex
import Rex.TestUtils
import Sire.Types
import Test.Tasty

import qualified Fan          as F
import qualified Loot.ReplExe as Loot
import qualified Loot.TestExe as Loot
import qualified Sire.ReplExe as Sire

import GHC.IO.Handle (hFlushAll)


-- Globals ---------------------------------------------------------------------

vMac :: IORef (Map Text Fan)
vMac = unsafePerformIO (newIORef mempty)

devNull :: Handle
devNull = unsafePerformIO (openFile "/dev/null" WriteMode)


--------------------------------------------------------------------------------

main :: IO ()
main = colorsOnlyInTerminal do
  putStrLn "TODO: update Sire tests."
  when False do
    writeIORef F.vShowFan Sire.showFan
    writeIORef F.vTrkFan  Sire.trkFan
    writeIORef F.vJetMatch (F.jetMatch)

    -- TODO Need to reset this after each test.
    vSrc <- liftIO $ newIORef mempty
    vPrp <- liftIO $ newIORef (F.TAb mempty)
    vEnv <- liftIO $ newIORef mempty
    vGen <- liftIO $ newIORef 1
    langTest ".sire" (let ?rexColors = NoColors in goldenSire vSrc vPrp vGen vEnv)

cab :: Symb
cab = utf8Nat "_"

goldenSire
    :: RexColor
    => IORef (Map Nat Nat)
    -> IORef Fan
    -> IORef Nat
    -> IORef (Map Symb Sire.Global)
    -> GoldPaths
    -> TestTree
goldenSire vSrc vPrp vGen vEnv pax = do
    runTest pax doBlk end
  where
    GOLD_PATHS{gpSource,gpOutput} = pax

    doBlk :: FilePath -> Handle -> Bool -> Block -> IO ()
    doBlk _fil _h _firstLn blk = do
        Sire.runBlockFan devNull False vSrc vPrp vGen vEnv vMac blk

    end :: Handle -> IO ()
    end h = do
           env <- readIORef vEnv
           pln <- pure $ fromMaybe (NAT 0) ((.val) <$> lookup cab env)
           has <- mkPin' pln >>= getPinHash

           Loot.printValue h False (Just $ utf8Nat "_") pln
           Loot.printValue h True (Just $ utf8Nat "_hash")
                           (BAR $ hashToByteString has)
           hFlushAll h >> hClose h
           Loot.validateLoot gpOutput (gpSource <> ".tmp")
