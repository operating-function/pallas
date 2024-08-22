-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-
    TODO: Add profile tracing the the worker threads.

    This is the timer hardware.  The interface is simple:

        when :: IO Time       -- Get the current time
        wait :: Time -> IO () -- Block until the given time as passed.

    Implementation
    ==============

    WHEN Requests
    -------------

    We maintain a pool of all active WHEN requests (with ids).

    New WHEN requests are simply just added to the pool, on cancel we
    do nothing.

    There is a worker thread that processes WHEN requests.  If there
    are no WHEN, requests we block.  Otherwise, we fill all WHEN requests
    with the current time.

    WAIT Requests
    -------------

    We maintain a pool of all active WAIT requests (with ids).

    We also maintain an heap mapping timestamps to their request ids.

    New WAIT requests are added to the pool, and to the heap.  Canceling
    them just removes them from the pool.  We don't bother to remove
    them from the heap.

    There is a worker thread that processes WAIT requests.  It runs
    another (trivial) "alarm" thread and just sleeps until a certain
    time and then sets a flag.

    The WAIT worker looks at:

    -   What is the WHEN request with the earliest time?
    -   Is there an active alarm?

        -   If so, what time is it set for?
        -   If so, has it woken up yet?

    And applies the following logic:

    -   If there are no alarms and no WHEN requests, retry

    -   If the most recent WHEN matches the timer, but the timer hasn't
        fired, retry.

    -   When there is a timer but no WHEN:

        -   Cancel the timer

    -   If there is a WHEN but no timer:

      -   Launch a new timer

    -   When the most recent WHEN does not match the alarm:

        -   Cancel the timer
        -   Launch a new timer
        -   Update the timer state
-}

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE Strict #-}

module Server.Hardware.Time (createHardwareTime) where

import Control.Concurrent
import Data.Acquire
import Data.UnixTime
import Fan.Eval
import Fan.Prof
import Foreign.C.Types
import PlunderPrelude
import Server.Common
import Server.Debug
import Server.Hardware.Types

import Data.Heap (MinHeap)

import qualified Data.Heap as Heap
import qualified Data.Map  as M

--------------------------------------------------------------------------------

data HWState = HW_STATE
    { waitPool   :: TVar (Pool SysCall)
    , whens      :: TVar [SysCall]
    , waits      :: TVar (MinHeap (Int64, Int))
    , alarm      :: TVar (Maybe (Int64, TVar Bool, ThreadId))
    , waitWorker :: ThreadId
    , whenWorker :: ThreadId
    }

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st kal = do
    case toList kal.args of
        ["when"] -> do
            modifyTVar' st.whens (kal :)
            pure (CANCEL pass, [])
        ["wait", NAT wakeTime] -> do
            let wen = fromIntegral wakeTime
            if wakeTime /= fromIntegral wen then
                -- If the wakeTime overflows an Int64, we never return.
                pure (CANCEL pass, [])
            else do
                key <- poolRegister st.waitPool kal
                modifyTVar' st.waits (Heap.insert (wen, key))
                pure (CANCEL (poolUnregister st.waitPool key), [])
        _ -> do
            --
            fillInvalidSyscall kal
            pure (CANCEL pass, [])

categoryCall :: Vector Fan -> Text
categoryCall args = "%time " <> case toList args of
  ["when"]        -> "%when"
  ["wait", NAT _] -> "%wait"
  _               -> "UNKNOWN"

describeCall :: Vector Fan -> Text
describeCall args = "%time " <> case toList args of
  ["when"]        -> "%when"
  ["wait", NAT w] -> "%wait " <> tshow w
  _               -> "UNKNOWN"

createHardwareTime :: Debug => Acquire Device
createHardwareTime = do
    st <- mkAcquire mk release
    pure DEVICE
        { stop = pass
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
  where
    mk :: IO HWState
    mk = mdo
        waitPool   <- newTVarIO emptyPool
        whens      <- newTVarIO []
        waits      <- newTVarIO mempty
        alarm      <- newTVarIO Nothing
        whenWorker <- forkIO (void $ runWhenWorker st)
        waitWorker <- forkIO (void $ runWaitWorker st)
        let ~st = HW_STATE{..}
        pure st

    release :: HWState -> IO ()
    release st = do
        killThread st.waitWorker
        killThread st.whenWorker
        atomically (readTVar st.alarm) >>= \case
            Nothing                  -> pass
            Just (_, _, alarmThread) -> killThread alarmThread

runWhenWorker :: HWState -> IO Void
runWhenWorker st = do
    withProcessName "Time" $ do
      withThreadName "When" $ do
        forever step
  where
    step :: IO ()
    step = do
        whens <- atomically do
            whens <- readTVar st.whens
            when (null whens) retry
            writeTVar st.whens []
            pure whens

        let endFlows = map (.cause) whens

        withTracingFlow "When" "time" mempty endFlows $ do
          unixNow <- getUnixTime
          let CTime cNow = unixNow.utSeconds
          let fan        = NAT (fromIntegral cNow)
          startFlows <- atomically $ for whens $ \kal -> writeResponse kal fan
          pure (startFlows, [], ())

-- TODO: Profiler Tracing
runWaitWorker :: HWState -> IO Void
runWaitWorker st = do
    withProcessName "Time" $ do
      withThreadName "Wait" $ do
        forever step
  where
    step :: IO ()
    step = withTracingResultsFlow "alarm" "time" $ do
        (description, action, endFlows, startFlows) <- atomically do
            waits <- readTVar st.waits
            alarm <- readTVar st.alarm -- Maybe (Nat, TVar Bool, ThreadId)

            case (alarm, Heap.view waits) of

                (Nothing, Nothing) ->
                    retry

                -- If there is an alarm but no wait syscall, unset
                -- the alarm.
                (Just (_,_,tid), Nothing) -> do
                    writeTVar st.alarm Nothing
                    pure ("alarm but no wait", (killThread tid), [], [])

                -- If there is a wait call, but no alarm, set an alarm.
                (Nothing, Just ((wakeTime,_key),_)) -> do
                    let action = do
                          vFired <- newTVarIO False

                          tid <- forkIO do
                              nowUnix <- getUnixTime
                              let CTime nowSecs = nowUnix.utSeconds
                              when (nowSecs < wakeTime) do
                                  let diffsecs =
                                        fromIntegral (wakeTime - nowSecs)
                                  threadDelay (1_000_000 * diffsecs)
                              atomically (writeTVar vFired True)

                          let newAlarm = (wakeTime, vFired, tid)
                          atomically (writeTVar st.alarm (Just newAlarm))
                    pure ("wait but no alarm", action, [], [])

                -- If the alarm doesn't match the earliest wait call, just
                -- delete the alarm.  The next call to `step` will
                -- create a new one.
                (Just (n,_,tid), Just ((wen,_),_)) | n /= wen -> do
                    writeTVar st.alarm Nothing
                    pure ("alarm doesn't match", (killThread tid), [], [])

                -- Alarm matches the earliest wait call.  Block until
                -- the timer has been fired, cause the wait syscall to
                -- return, and then remove the alarm and the wait syscall.
                (Just (_,f,tid), Just ((_,key),more)) -> do
                    fired <- readTVar f
                    unless fired retry
                    pool <- readTVar st.waitPool
                    mybFlows <- case lookup key pool.tab of
                        Nothing -> pure Nothing
                        Just kl -> do
                          response <- writeResponse kl ()
                          pure $ Just (kl.cause, response)
                    poolUnregister st.waitPool key
                    writeTVar st.alarm Nothing
                    writeTVar st.waits more

                    let (endFlows, startFlows) = case mybFlows of
                          Nothing                  -> ([], [])
                          Just (request, response) -> ([request], [response])
                    pure ("alarm fire", (killThread tid), endFlows, startFlows)

        action
        let traceArgs = M.singleton "wakeup reason" (Right description)
        pure (traceArgs, startFlows, [], endFlows, ())
