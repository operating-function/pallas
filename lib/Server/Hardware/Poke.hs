-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Server.Hardware.Poke
  ( SubmitPoke
  , createHardwarePoke) where

import Fan.Convert
import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Common
import Server.Debug
import Server.Hardware.Types
import Server.LmdbStore

import Control.Concurrent.STM.TVar (stateTVar)
import Data.Acquire                (Acquire, mkAcquire)
import Data.Heap                   (MinHeap)
import Server.Types.Logging        (CogId(..))

import qualified Data.Heap   as Heap
import qualified Data.Map    as M
import qualified Data.Vector as V
import qualified Fan         as F

-- Callback function which pokes a cog and blocks until the cog has processed
-- the poke. This should block if the cog does not currently have an open poke
-- request.
type SubmitPoke = CogId -> Vector Text -> F.Fan -> IO ()

type RequestStorage = Map (CogId, Vector Text) (MinHeap Int)

data HWState = HW_STATE
    { requestPool :: TVar (Pool SysCall)
    , requests    :: TVar RequestStorage
    }

data Req = POKE (Vector Text)

categoryCall :: Vector Fan -> Text
categoryCall args = "%poke " <> case toList args of
  ["poke", _] -> "%poke"
  _           -> "UNKNOWN"

describeCall :: Vector Fan -> Text
describeCall args = "%poke " <> case toList args of
  ["poke", txt] -> "%poke " <> tshow (fromNoun @(Vector Text) txt)
  _             -> "UNKNOWN"

addRequest :: CogId -> Vector Text -> Int -> RequestStorage
           -> RequestStorage
addRequest cog path key rsm = M.alter update (cog, path) rsm
  where
    update :: Maybe (MinHeap Int) -> Maybe (MinHeap Int)
    update Nothing     = Just $ Heap.insert key Heap.empty
    update (Just heap) = Just $ Heap.insert key heap

rmRequest :: CogId -> Vector Text -> Int -> RequestStorage
          -> RequestStorage
rmRequest cog path key rsm = M.alter update (cog, path) rsm
  where
    update :: Maybe (MinHeap Int) -> Maybe (MinHeap Int)
    update Nothing     = Nothing
    update (Just heap) = let after = Heap.filter (/= key) heap
                         in if Heap.null after then Nothing else Just after

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall =
  case toList syscall.args of
    ["poke", ROW nats] -> do
      let path = map (decodeUtf8 . natBytes . toNat) (V.fromArray nats)
      key <- poolRegister st.requestPool syscall
      modifyTVar' st.requests (addRequest syscall.cog path key)

      let docancel = do
            poolUnregister st.requestPool key
            modifyTVar' st.requests
                        (rmRequest syscall.cog path key)

      pure (CANCEL docancel, [])
    _ -> do
      fillInvalidSyscall syscall
      pure (CANCEL (pure ()), [])

-- Function given to ConsoleExe to submit pokes.
submitPoke :: HWState -> SubmitPoke
submitPoke st cogId path val = do
  -- We don't want to have the tracing system associate servant asyncs with us,
  -- so build our own new process/thread async every time we want to do
  -- something and just wait on the result.
  w <- newProcessAsync "Poke" "Poke" $
    withTracingResultsFlow "submitPoke" "poke" $ atomically do
      tv <- readTVar st.requests
      let myb = lookup (cogId, path) tv
      h <- maybe retry pure myb
      (key,rest) <- maybe (error "empty heap not deleted in Poke") pure
                          (Heap.view h)
      pool <- readTVar st.requestPool
      mybFlows <- case lookup key pool.tab of
        Nothing -> retry
        Just kl -> do
          modifyTVar' st.requests (rmRequest kl.cog path key)
          poolUnregister st.requestPool key
          response <- writeResponse kl val
          pure $ Just (kl.cause, response)

      let (endFlows, startFlows) = case mybFlows of
            Nothing                  -> ([], [])
            Just (request, response) -> ([request], [response])
      pure (mempty, startFlows, [], endFlows, ())

  wait w

createHardwarePoke
    :: Debug
    => Acquire (Device, SubmitPoke)
createHardwarePoke = do
  st <- mkAcquire startup shutdown

  let dev = DEVICE
        { spin = \_ -> pass -- Don't care which cog makes the calls.
        , stop = \_ -> pass
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }

  pure (dev, submitPoke st)
  where
    startup :: IO HWState
    startup = do
      requestPool <- newTVarIO emptyPool
      requests    <- newTVarIO mempty
      pure HW_STATE{requestPool,requests}

    shutdown :: HWState -> IO ()
    shutdown _ = pure ()
