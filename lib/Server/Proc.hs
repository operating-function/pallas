-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-
    TOPLEVEL TODOS:

    - TODO: `shutdownMachine` works for now, but what we really want is a way
            to hit ctrl-c once to begin shutdown and then have a second ctrl-c
            which aborts snapshotting.
-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Wall   #-}
-- {-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server.Proc
    ( Machine(..)
    , spawnProc
    , shutdownMachine
    )
where

import PlunderPrelude

import Control.Monad.State   (StateT, execStateT)
import Fan                   (Fan(..), PrimopCrash(..), (%%))
-- import Optics                (set)
import Server.Convert        ()
import System.Random.Shuffle (shuffleM)

import Fan.Convert
import Fan.Prof
import Server.Debug
import Server.Evaluator
import Server.Hardware.Types
import Server.Time
import Server.Types.Logging

import qualified Data.IntMap as IM
-- import qualified Data.Map    as M
import qualified Data.Vector as V

--------------------------------------------------------------------------------

newtype ProcId = PROC_ID Word64
  deriving newtype (Eq, Show, Ord)

-- | Toplevel handle for a Machine, a group of individual Proc fan evaluation
-- that makes requests and receives responses and which share an event log.
--
-- Internally, machine execution has three long lived asyncs: a `Runner` async,
-- a `Logger` async, and a `Snapshotter` async. These communicate and are
-- controlled via the following STM variables:
data Machine = MACHINE
  { -- ctx            :: MachineContext
    procAsyncs     :: Map ProcId (Async ())
  }

thirtySecondsInMicroseconds :: Nat
thirtySecondsInMicroseconds = 30 * 10 ^ (6::Int)

data Response
  = RespRead Fan
  | RespWrite Fan -- TODO do we need this?
  deriving (Show)

instance ToNoun Response where
  toNoun = \case
    RespRead result -> result
    RespWrite _     -> NAT 0

data ReadRequest
  = CogRead { query :: Fan, state :: CallStateVar }

data WriteRequest
  = CogWrite { cmd :: Fan, state :: CallStateVar }

data Request
  = DB_READ Fan
  | DB_WRITE Fan
  deriving (Show)

instance FromNoun Request where
  fromNoun fan = do
      row <- getRowVec fan
      tag <- fromNoun @Natural (row V.! 0)
      case tag of
  --    0 -> Just $ DB_WAIT
        1 -> Just $ DB_READ (row V.! 1)
        2 -> Just $ DB_WRITE (row V.! 1)
        _ -> Nothing

getCurrentReqNoun :: Fan -> Vector Fan
getCurrentReqNoun s = do
    case s of
        KLO _ xs -> do
            let len = sizeofSmallArray xs
            case (xs .! (len-1)) of
                ROW x -> V.fromArray x
                _     -> mempty
        _ -> mempty

-- A request noun is empty if it is a row with a nonzero value.
hasNonzeroReqs :: Fan -> Bool
hasNonzeroReqs = any (/= NAT 0) . getCurrentReqNoun

data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

-- | A list of parsed out valid requests from `noun`. For every proc, for every
-- index in that proc's requests table, there is a raw fan value and a
-- `LiveRequest` which contains STM variables to listen
type ProcSysCalls = IntMap (Fan, LiveRequest)

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { hw    :: DeviceTable
    , vProc :: TVar Fan          -- ^ Current value
    , vReqs :: TVar ProcSysCalls -- ^ Current requests table
    }

-- -----------------------------------------------------------------------

data ResponseTuple = RTUP
    { key  :: RequestIdx
    , resp :: Response
    }
  deriving (Show)

getReadResult :: ReadRequest -> STM Fan
getReadResult CogRead{..} = readTVar state.var >>= \case
  DONE result _ -> pure result <* writeTVar state.var DEAD -- TODO abstract out the result handling using a typeclass or a function in the record
  _             -> retry

getWriteResult :: WriteRequest -> STM Fan
getWriteResult CogWrite{..} = readTVar state.var >>= \case
  DONE _ _ -> pure (toNoun ()) <* writeTVar state.var DEAD -- TODO maybe return something? if the Proc should be able to react to error msgs.
  _        -> retry

-- | A `LiveRequest` is a handle that could produce a Response to an open
-- Request.
data LiveRequest
  = LiveDbRead
    { lrIdx :: RequestIdx
    , lrCall :: ReadRequest
    , lrCancel :: Cancel
    }
  | LiveDbWrite
    { lwIdx :: RequestIdx
    , lwCall :: WriteRequest
    , lwCancel :: Cancel
    }

instance Show LiveRequest where
    show = \case
        LiveDbRead{}  -> "READ"
        LiveDbWrite{} -> "WRITE"

data ParseRequestsState = PRS
    { syscalls  :: ProcSysCalls
--  , flows     :: [Flow]
--  , onPersist :: [STM OnCommitFlow]
    }

makeFieldLabelsNoPrefix ''Runner
makeFieldLabelsNoPrefix ''ParseRequestsState

-- -----------------------------------------------------------------------
-- No template haskell beyond this point because optics.
-- -----------------------------------------------------------------------

spawnProc :: Debug => Fan -> DeviceTable -> IO (Async ())
spawnProc proc hw =
  asyncOnCurProcess $ withThreadName "Foo"
  $ handle (onErr "runner foo")
  $ runnerFun hw (error "procname") =<< atomically do
      vProc <- newTVar proc
      vReqs <- newTVar mempty
      pure RUNNER{..}
  where
    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- | Synchronously shuts down a machine and wait for it to exit.
shutdownMachine :: Machine -> IO ()
shutdownMachine MACHINE{procAsyncs} = traverse_ cancel procAsyncs

callCog :: SysCall -> STM Cancel
callCog = error "callCog"

-- | Given a Request parsed from the proc, turn it into a LiveRequest that can
-- produce a value and that we can listen to.
buildLiveRequest :: Debug
                 => DeviceTable -> RequestIdx -> Request -> STM LiveRequest
buildLiveRequest _ reqIdx = \case
  DB_READ query -> do
    callSt <- STVAR <$> newTVar LIVE
    let lrCall = CogRead query callSt
    lrCancel <- callCog $ error "lrCall"
    pure LiveDbRead{lrIdx=reqIdx, lrCall, lrCancel}
  DB_WRITE cmd -> do
    callSt <- STVAR <$> newTVar LIVE
    let lwCall = CogWrite cmd callSt
    lwCancel <- callCog (error "lwCall")
    pure LiveDbWrite{lwIdx=reqIdx, lwCall, lwCancel}

receiveResponse :: (Fan, LiveRequest) -> STM ResponseTuple
receiveResponse = \case
    (_, LiveDbRead{..}) -> do -- TODO this should probably be a typeclass?
      outcome <- getReadResult lrCall
      pure RTUP{key=lrIdx, resp=RespRead outcome}

    (_, LiveDbWrite{..}) -> do
      outcome <- getWriteResult lwCall
      pure RTUP{key=lwIdx, resp=RespWrite outcome}

-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Proc Runner --------------------------------------------------------------

runnerFun :: Debug => DeviceTable -> ByteString -> Runner -> IO ()
runnerFun _ processName st =
    bracket_ registerProcsWithHardware stopProcsWithHardware $ do
        -- Process the initial syscall vector
        atomically $ parseRequests st

        -- Run the event loop until we're forced to stop.
        procTick
  where
    registerProcsWithHardware :: IO ()
    registerProcsWithHardware = pure () -- for_ ctx.hw.table $ \d -> d.spin procId

    stopProcsWithHardware :: IO ()
    stopProcsWithHardware = pure () -- for_ ctx.hw.table $ \d -> d.stop procId

    -- Collects all responses that are ready for one proc.
    takeReturns :: IO (Maybe ResponseTuple)
    takeReturns = do
        withAlwaysTrace "WaitForResponse" "proc" do
            procSysCalls <- IM.elems <$> readTVarIO st.vReqs
            case procHasLiveRequests procSysCalls of
                False -> pure Nothing
                True -> Just <$> do
                  reordered <- shuffleM procSysCalls -- TODO shuffling is ugly! maybe use a finger tree instead?
                  -- debugText $ "takeReturns: " <> tshow reordered
                  atomically . asum $ receiveResponse <$> reordered

    procTick :: IO ()
    procTick = takeReturns >>= \case
      Nothing -> pure ()
      Just response -> do
        withProcessName processName $
          withThreadName ("Proc: ") $
            runResponse st response
        procTick

    procHasLiveRequests :: [(a, LiveRequest)] -> Bool
    procHasLiveRequests = not . null -- any \(_, req) -> validLiveRequest req

{-
    Given a set of responses to syscalls in a procs SysCall table, create
    a new event value and pass that into the proc, to get the new proc state.

    Side Effects:

    -   The PLAN value for the proc is replaced.

    -   The proc's requests row is updated to reflect the new set of
        requests.

    -   If we received a PROC_TELL message, the corresponding PROC_ASK request
        is also processed.

    -   If we spawned a proc or stopped a proc, we inform every hardware
        device that this happened.

    Results:

    -   The set of profiling events triggered by this change.

    -   A list of event-log receipts (and the corresponding actions to
        be triggered when those receipts have been committed to disk.
        (This is for disk-synchronized syscalls).
-}
runResponse :: Debug => Runner -> ResponseTuple -> IO ()
runResponse st resp = do
  proc <- atomically (readTVar st.vProc)
  (_, result) <- do
      withAlwaysTrace "Eval" "proc" do
          let preEvaluate = ensureResponseEvaluated resp.resp
          evalWithTimeout thirtySecondsInMicroseconds [preEvaluate] (proc %% toNoun resp.key) (toNoun resp.resp)

  let newState = case result of
        OKAY _ resultFan -> case hasNonzeroReqs resultFan of
                                True  -> resultFan
                                False -> error "TODO: restart"
        CRASH{}          -> error "TODO: restart"
        TIMEOUT          -> error "TODO: restart, maybe?"

  withAlwaysTrace "Tick" "proc" do
      let responses = resp.resp

      -- 1) Notify hardware about spins which started a proc. We have to do this
      -- before we record the proc state or do parseRequests because the initial
      -- state of a newly created proc might try to communicate with the
      -- hardware.
      performAlertHardwareOnSpin responses

      -- 2) Perform parseRequest and handle all changes that have to be handled
      -- attomically. This has to happen after we notify the hardware about
      -- procs being spun.
      atomically $ performStateUpdate newState result

      -- 3) If this proc shut down for any reason, we have to alert the hardware
      -- that it has shut down. This has to be done after parsing requests
      -- because parsing requests will cancel ongoing hardware events.
--    case newState of
--      CG_SPINNING _ -> pure ()
--      _             -> for_ st.ctx.hw.table $ \d -> d.stop procNum

      pure ()

  where
    ensureResponseEvaluated :: Response -> IO ()
    {-
    ensureResponseEvaluated (RespTell TELL_PAYLOAD{..}) = do
      -- We must force evaluate the response thunk so that we make sure it
      -- doesn't have a crash, but must do this while crediting the runtime
      -- (and possible crash) to the %tell since the %tell provides the
      -- function.
      evaluate $ force ret
      pure ()
    -}
    ensureResponseEvaluated _ = pure ()

    performAlertHardwareOnSpin :: Response -> IO ()
    performAlertHardwareOnSpin _ = pure ()

    performStateUpdate newState result = do
        writeTVar st.vProc newState

        -- TODO do we need the below?
        _ <- case result of
            OKAY{} -> do
              -- Delete the consumed `LiveRequest`s without formally
              -- canceling it because it completed.
              modifyTVar' st.vReqs (deleteMap resp.key.int)
            _ -> do
              -- If we crashed, do nothing. We'll formally cancel all
              -- requests when we reparse in the next step, and we have no
              -- valid responses to process side effects from.
              pure mempty

        _ <- parseRequests st

        pure ()

-- | Given a noun in a runner, update the `requests`
parseRequests :: Debug => Runner -> STM ()
parseRequests runner = do
    procState <- readTVar runner.vProc
    syscalls <- readTVar runner.vReqs

    let init = PRS{syscalls}

    st <- flip execStateT init do
              let actual = getCurrentReqNoun procState
              mapM_ createReq (toList actual)

    writeTVar runner.vReqs st.syscalls
  where
    createReq :: Fan -> StateT ParseRequestsState STM ()
    createReq v = maybe (pure ()) build do
        ROW reqFan <- pure v
        let fan = V.fromArray reqFan
        i <- fromNoun $ fan V.! 0
        request <- fromNoun $ fan V.! 1
        pure (RequestIdx i, request)
      where
        build :: (RequestIdx, Request) -> StateT ParseRequestsState STM ()
        build (rIdx, request) = do
            live <- lift $
              buildLiveRequest (error "TODO hw table") rIdx request

            modifying' #syscalls (insertMap rIdx.int (v, live))

evalWithTimeout
    :: Debug
    => Nat
    -> [IO ()]
    -> Fan
    -> Fan
    -> IO (NanoTime, EvalOutcome)
evalWithTimeout msTimeout preActions fun arg = do
  (runtime, raw) <- withCalcRuntime $ timeout (fromIntegral msTimeout) $ do
    try doAllEvals >>= \case
      Left (PRIMOP_CRASH op val) -> do debug ("crash"::Text, NAT op, val)
                                       pure (Left (op,val))
      Right f                    -> pure (Right f)
  case raw of
    Nothing          -> pure (runtime, TIMEOUT)
    Just (Left(o,e)) -> pure (runtime, CRASH o e)
    Just (Right v)   -> pure (runtime, OKAY runtime v)
  where
    doAllEvals = do
      sequence_ preActions
      evaluate $ force (fun %% arg)
