-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE RecursiveDo #-}

module Server.Hardware.Rand where

import PlunderPrelude

import Data.Acquire
import Fan.Eval
import Fan.Prof
import Server.Debug
import Server.Hardware.Types

import System.Entropy (getEntropy)

--------------------------------------------------------------------------------

data HWState = HW_STATE
    { queue      :: TQueue SysCall
    , randWorker :: Async Void
    }


--------------------------------------------------------------------------------

runRandWorker :: HWState -> IO Void
runRandWorker st =
    withProcessName "Rand" $ withThreadName "Bytes" $ forever step
  where
    act syscall ["byte", NAT count] = do
        ent <- withSimpleTracingEvent "getEntropy" "rand" do
                   getEntropy (fromIntegral count)
        atomically (writeResponse syscall ent)

    act syscall _ = do
        atomically (fillInvalidSyscall syscall)

    step = do
        syscall <- atomically (readTQueue st.queue)
        withTracingResultsFlow "step" "rand" do
            responseFlow <- act syscall (toList syscall.args)
            let endFlows = [syscall.cause]
            let startFlows = [responseFlow]
            pure (mempty, startFlows, [], endFlows, ())

categoryCall :: Vector Fan -> Text
categoryCall args = "%rand " <> case toList args of
  ["byte", NAT _] -> "%byte"
  _               -> "UNKNOWN"

describeCall :: Vector Fan -> Text
describeCall args = "%rand " <> case toList args of
  ["byte", NAT _] -> "%byte"
  _               -> "UNKNOWN"

createHardwareRand :: Debug => Acquire Device
createHardwareRand = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { spin = \_ -> pass -- Don't care which cog makes the calls.
        , stop = \_ -> pass
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
  where
    runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
    runSysCall st syscall = do
        writeTQueue st.queue syscall
        pure (CANCEL (pure ()), [])

    shutdown = cancel . (.randWorker)

    startup :: IO HWState
    startup = mdo
        queue      <- newTQueueIO
        randWorker <- async (runRandWorker st)
        let st = HW_STATE{queue,randWorker}
        pure st
