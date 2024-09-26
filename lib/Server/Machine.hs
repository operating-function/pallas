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
-- {-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -freverse-errors #-}

module Server.Machine
    ( Machine(..)
    , MachineContext(..)
    , Moment(..)
    , performReplay
    , replayAndCrankMachine
    , shutdownMachine
    )
where

import PlunderPrelude

import Control.Monad.State   (StateT, execStateT, modify', runStateT)
import Control.Monad.State.Class (MonadState)
import Fan                   (Fan(..), PrimopCrash(..), (%%), fanIdx)
import GHC.Prim              (reallyUnsafePtrEquality#)
-- import Optics                (set)
import Server.Convert        ()
-- import System.Random.Shuffle (shuffleM)

import qualified GHC.Natural as GHC

import Data.Sorted
import Fan.Convert
import Fan.Prof
import Server.Common
import Server.Debug
import Server.Evaluator
import Server.Hardware.Types
import Server.LmdbStore
import Server.Proc
import Server.Time
import Server.Types.Logging

-- import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Vector as V

--------------------------------------------------------------------------------

receiptQueueMax :: GHC.Natural
receiptQueueMax = 65536

--------------------------------------------------------------------------------

data Moment = MOMENT {
  val  :: CogState,
  work :: NanoTime
  }

data MachineContext = MACHINE_CONTEXT
    { lmdb        :: LmdbStore
    , hw          :: DeviceTable
    , eval        :: Evaluator
    , enableSnaps :: Bool
    }

-- | Flow report for an onCommit call.
data OnCommitFlow = OnCommitFlow
    { step   :: Flow     -- ^ This flow caused this commit.
    , starts :: [Flow]   -- ^ This commit started these flows.
    }

unzipOnCommitFlows :: [OnCommitFlow] -> ([Flow], [Flow])
unzipOnCommitFlows onCommits = ( map (.step) onCommits
                               , join $ map (.starts) onCommits)

-- | Toplevel handle for a Machine, a group of individual Cog fan evaluation
-- that makes requests and receives responses and which share an event log.
--
-- Internally, machine execution has three long lived asyncs: a `Runner` async,
-- a `Logger` async, and a `Snapshotter` async. These communicate and are
-- controlled via the following STM variables:
data Machine = MACHINE {
  ctx              :: MachineContext,

  -- The three phases:
  runnerAsync      :: Async (),
  loggerAsync      :: Async (),
  snapshotAsync    :: Async (),
  workersAsyncs    :: TVar CogWorkers,

  -- Signal to shut down all the asyncs. After setting this, you should wait on
  -- all three in order.
  shutdownLogger   :: TVar Bool,
  shutdownSnapshot :: TVar Bool,

  -- Starts empty, but is filled once the machine has shutdown and the
  shutdownComplete :: TMVar (),

  -- Noun state and time of the last commit at each of the three phases.
  liveVar          :: TVar Moment,
  writVar          :: TVar (BatchNum, Moment),

  -- | Command to log immediately (because of a pending add)
  logImmediately   :: TVar Bool,
  logReceiptQueue  :: TBQueue (Receipt, [STM OnCommitFlow])
  }

oneSecondInNs :: NanoTime
oneSecondInNs = NanoTime $ 10 ^ (9::Int)

-- Configuration for
logbatchWorkIntervalInNs, snapshotWorkIntervalInNs :: NanoTime
logbatchWorkIntervalInNs = oneSecondInNs
snapshotWorkIntervalInNs = oneSecondInNs * 45

-- twoSecondsInMicroseconds :: Nat
-- twoSecondsInMicroseconds = 2 * 10 ^ (6::Int)

thirtySecondsInMicroseconds :: Nat
thirtySecondsInMicroseconds = 30 * 10 ^ (6::Int)

data WriteRequest = COG_WRITE
  { cmd :: Fan
  , cause :: Flow
  }

-- This used to use Maybe but was generalized to f because sometimes we need
-- STM instead. f should probably always have an Alternative instance though.
class Effect f c where
    makeCall :: Eff -> f c
--  writeResponse :: c ->

instance Effect STM WriteRequest where
  makeCall eff@EFF{..} = kill eff *> liftMaybe do
    [NAT 0, NAT "write", cmd] <- sequence [fan V.!? 0, fan V.!? 1, fan V.!? 2]
    pure COG_WRITE{..}

data ReadRequest = COG_READ
  { reqIdx :: RequestIdx
  , query  :: Fan
  , state  :: CallStateVar
--, cause  :: Flow
  }

instance Alternative f => Effect f ReadRequest where
  makeCall EFF{..} = liftMaybe do
    [NAT 0, NAT "read", query] <- sequence [fan V.!? 0, fan V.!? 1, fan V.!? 2]
    pure COG_READ{..}

instance ToNoun WriteRequest where
  toNoun write = write.cmd

instance ToNoun ReadRequest where
  toNoun read = read.query

data TellOutcome
    = OutcomeOK Fan TellId
    | OutcomeCrash
  deriving (Show)

data Response
    = RespEval EvalOutcome
    | RespCall Fan SysCall
    | RespAsk TellOutcome
  deriving (Show)

responseToVal :: Response -> Fan
responseToVal (RespCall f _) = f
responseToVal (RespAsk (OutcomeOK val _)) = (NAT 0) %% val
responseToVal (RespAsk OutcomeCrash) = (NAT 0)
responseToVal (RespEval e)   =
    ROW case e of
        TIMEOUT   -> mempty                      -- []
        OKAY _ r  -> rowSingleton r              -- [f]
        CRASH n e -> arrayFromListN 2 [NAT n, e] -- [n e]

-- TODO separate eval and write
responseToReceiptItem :: (ProcId, WriteRequest) -> (Int, ReceiptItem)
responseToReceiptItem (PROC_ID i, COG_WRITE{cmd})
  = (fromIntegral i, ReceiptVal cmd)

makeOKReceipt :: [(ProcId, WriteRequest)] -> Receipt
makeOKReceipt =
    RECEIPT_OK . mapFromList . fmap responseToReceiptItem

data Request
    = ReqEval EvalRequest
    | ReqTell Word64 Fan
  deriving (Show)

getCurrentWorkerNoun :: Fan -> Vector Fan
getCurrentWorkerNoun fan = case fromEndOfClosure 0 fan of
    Just (ROW xs) -> V.fromArray xs
    _             -> mempty

getCurrentDbNoun :: Fan -> Fan
getCurrentDbNoun = fromMaybe 0 . fromEndOfClosure 2

getCurrentReadsNoun :: Fan -> Fan
getCurrentReadsNoun = fromMaybe 42 . fromEndOfClosure 1 -- TODO we should have something other than 42 but this is nice for now because it crashes and is recognizable

fromEndOfClosure :: Nat -> Fan -> Maybe Fan
fromEndOfClosure idx = \case
  v@KLO{} -> Just $ fanIdx idx v
  _       -> Nothing

-- A request noun is empty if it is a row with a nonzero value.
hasNonzeroReqs :: Fan -> Bool
hasNonzeroReqs = any (/= NAT 0) . getCurrentWorkerNoun

data EvalCancelledError = LIVE_EVAL_CANCELLED
  deriving (Exception, Show)

data Worker
  = EXEC { pump :: Fan }
  | EVAL { fun :: Fan, args :: Vector Fan }

stageWorker :: Debug => DeviceTable -> Worker -> STM (IO LiveWorker)
stageWorker hw EXEC{pump} = do
    writes <- newTQueue
    let write = writeTQueue writes
    reads <- newTQueue
    let read = writeTQueue reads

    let call pc = asum [ makeCall pc >>= read  >> pure (CANCEL pass, [])
                       , makeCall pc >>= write >> pure (CANCEL pass, [])
                       , makeCall pc >>= callHardware hw
                       ] `orElse` (kill pc $> (CANCEL pass, []))
    pure do
      (thread, inbox) <- spawnProc pump call
      pure LIVE_EXEC{..}

instance Alternative f => Effect f SysCall where
  makeCall EFF{..} = liftMaybe $ V.uncons fan >>= \case
     (NAT i, args) | i /= 0 -> Just SYSCALL{dev=DEV_NAME i, ..}
     _                      -> Nothing

instance FromNoun Worker where
  fromNoun v = fromNoun v >>= \case
    [0,f,as] -> EVAL f <$> fromNoun as
    [1,pump] -> Just $ EXEC pump
    _        -> Nothing

data LiveWorker
  = LIVE_EXEC { pump :: Fan, reads :: TQueue ReadRequest, writes :: TQueue WriteRequest, inbox :: ResponseTuple -> STM (), thread :: Async () }
  | LIVE_EVAL { fun :: Fan, args :: Vector Fan, eval :: Evaluation, thread :: Async () }

instance Show LiveWorker where
  show = \case
    LIVE_EXEC{pump} -> "LIVE_EXEC{" <> show pump <> "}"
    LIVE_EVAL{fun,args} -> "LIVE_EVAL{fun=" <> show fun <> ", args=" <> show args <> "}"

-- | A list of parsed out valid requests from `noun`. For every cog, for every
-- index in that cog's requests table, there is a raw fan value and a
-- `LiveWorker` which contains STM variables to listen to as well as a thread
-- handle to kill the worker.
--
-- TODO this should be an array/vector instead.
type CogWorkers = IntMap (Fan, LiveWorker)

-- | All the information needed for both
data TellPayload = TELL_PAYLOAD
    { reqIdx :: RequestIdx

    -- The locally unique tellId for this apply. Written to the event log.
    , tellId :: TellId

    -- The value produced by running the tell function against its input. It is
    -- deliberately left lazy since they have to be created inside an STM
    -- action in `receiveResponse`, but must be evaluate/forced and credited
    -- against the tell's timeout. (We previously also separated the value here
    -- into its head and tail with `fanIdx` instead of just storing the full
    -- result, but that causes some partial evaluation and can crash the
    -- interpreter if the first statement is a `trk`.)
    , ret    :: ~Fan
    }
  deriving (Show)

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { ctx      :: MachineContext
    , vMoment  :: TVar Moment          -- ^ Current value + cumulative CPU time
    , vWorkers :: TVar CogWorkers      -- ^ Current active workers
    }

-- -----------------------------------------------------------------------

-- An effect that happens atomically after a Response is processed by a cog and
-- didn't crash.
data CogReplayEffect
    = CTell
      { tellId :: TellId
      , tell   :: Fan
      }
    deriving (Show)
{-
data ResponseTuple = RTUP
    { key  :: RequestIdx
    , resp :: Response
    , work :: NanoTime
    , flow :: Flow
    }
  deriving (Show)
-}

data ParseRequestsState = PRS
    { workers   :: IntMap (Fan, LiveWorker)
    , newWorkers :: IntMap (Fan, IO LiveWorker)
    , flows     :: [Flow]
    , onPersist :: [STM OnCommitFlow]
    , cancels   :: IO ()
    }

makeFieldLabelsNoPrefix ''MachineContext
makeFieldLabelsNoPrefix ''Moment
makeFieldLabelsNoPrefix ''Runner
makeFieldLabelsNoPrefix ''ParseRequestsState

-- -----------------------------------------------------------------------
-- No template haskell beyond this point because optics.
-- -----------------------------------------------------------------------

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (a, b, _) = (a, b)

third :: (a, b, c) -> c
third (_, _, c) = c

type ReconstructedEvals = [(Fan, Fan, Maybe CogReplayEffect)]

type Tells = Map TellId Fan

recomputeEvals
    :: Moment
    -> Tells
    -> IntMap ReceiptItem
    -> StateT NanoTime IO ReconstructedEvals
recomputeEvals m tells tab =
    for (mapToList tab) \(idx, rVal) -> do
        let k = toNoun (fromIntegral idx :: Word)
        case rVal of
            ReceiptVal val -> pure (k, val, Nothing)
            ReceiptEvalOK  -> do
                -- We performed an eval which succeeded the
                -- first time.
                case getEvalFunAt m (PROC_ID $ fromIntegral idx) of
                    Nothing ->
                        throwIO INVALID_OK_RECEIPT_IN_LOGBATCH
                    Just (fun, args)  -> do
                        (runtime, res) <- withCalcRuntime do
                            evaluate $ force (foldl' (%%) fun args)
                        modify' (+ runtime)
                        pure (k, ROW (rowSingleton res), Nothing)
  where
    getEvalFunAt :: Moment -> ProcId -> Maybe (Fan, Vector Fan)
    getEvalFunAt m (PROC_ID procId) = do
        workers <- getCurrentWorkerNoun <$> cogSpinningFun m.val
        worker <- workers V.!? fromIntegral procId
        fromNoun worker >>= \case
            EVAL fun args -> Just (fun, args)
            _             -> Nothing

-- | Loads and the last snapshot from disk, and loops over the event loop
-- (starting from that event) and processes each Receipt from each LogBatch.
--
-- At the end, we return the current batch number and the computed Moment
-- (the current state of a running cog).
performReplay
    :: Debug
    => Cushion
    -> MachineContext
    -> ReplayFrom
    -> IO (BatchNum, Moment)
performReplay cache ctx replayFrom = do
    let mkInitial (a, b) = (b, (MOMENT a 0, mempty))
    (bn, (m, _)) <- loadLogBatches replayFrom mkInitial doBatch ctx.lmdb cache
    pure (bn, m)
  where
    doBatch :: (BatchNum, (Moment, Tells)) -> LogBatch
            -> IO (BatchNum, (Moment, Tells))
    doBatch (_, momentTells) LogBatch{batchNum,executed} =
        loop batchNum momentTells executed

    runEffect :: (Moment, Tells) -> Maybe CogReplayEffect -> (Moment, Tells)
    runEffect (m, t) = \case
        Just CTell{..} -> (m, M.insert tellId tell t)
        _              -> (m, t)

    loop :: BatchNum -> (Moment, Tells) -> [Receipt]
         -> IO (BatchNum, (Moment, Tells))
    loop bn mt []     = pure (bn, mt)
    loop bn (m, t) (x:xs) =
        let mybCogFun = cogSpinningFun m.val
        in case x of
            RECEIPT_TIME_OUT{..} -> do
                -- Evaluating this bundle of responses timed out the first time
                -- we ran it, so just don't run it and just recreate the
                -- timed-out state from the recorded timeout amount.
                case mybCogFun of
                    Nothing -> throwIO INVALID_TIMEOUT_IN_LOGBATCH
                    Just fun -> do
                        let cog = CG_TIMEOUT timeoutAmount fun
                        pure (bn, (m { val = cog }, t))

            RECEIPT_CRASHED{..} -> do
                -- While it's deterministic whether a plunder computation
                -- crashes or not, its crash value is not deterministic so we
                -- must reconstitute the crash from the recorded result.
                case mybCogFun of
                    Nothing -> throwIO INVALID_CRASHED_IN_LOGBATCH
                    Just final -> do
                        let cog = CG_CRASHED{op,arg,final}
                        pure (bn, (m { val = cog }, t))

            RECEIPT_OK{..} -> do
                -- The original run succeeded, rebuild the value from the
                -- receipt items.
                (eRes, eWork) <- runStateT (recomputeEvals m t inputs) 0

                let arg = TAb $ mapFromList $ map tripleToPair eRes
                let (m, t) = foldl' runEffect (m, t) (map third eRes)

                fun <- case mybCogFun of
                    Nothing  -> throwIO INVALID_COGID_IN_LOGBATCH
                    Just fun -> pure fun

                (iWork, outcome) <- evalCheckingCrash fun arg

                newVal <- case outcome of
                    TIMEOUT ->
                        error "performReplay: impossible timeout on replay"
                    CRASH o e -> pure $ CG_CRASHED o e fun
                    OKAY _ result -> pure $ CG_SPINNING result

                let updateRunner = MOMENT
                        { work = m.work + eWork + iWork
                        , val = newVal
                        }

                loop bn (updateRunner, t) xs

    
-- | Main entry point for starting running a Cog.
replayAndCrankMachine
    :: Debug
    => Cushion
    -> MachineContext
    -> ReplayFrom
    -> IO Machine
replayAndCrankMachine cache ctx replayFrom = do
  let threadName  = encodeUtf8 "Main"

  withProcessName processName (wrapReplay threadName)
  where
    processName = "Machine"

    wrapReplay :: ByteString -> IO Machine
    wrapReplay threadName = do
      withThreadName threadName $ do
        setThreadSortIndex (-3)
        withTracingFlow "Replay" "machine" mempty [] $ do
          (batchNum, moment) <- performReplay cache ctx replayFrom

          debugText $ "REPLAY TIME: " <> tshow moment.work <> " ns"

          (machine, flows) <- buildMachine threadName batchNum moment
          pure (flows, [], machine)

    buildMachine
        :: ByteString
        -> BatchNum
        -> Moment
        -> IO (Machine, [Flow])
    buildMachine threadName lastBatch moment = do
      shutdownLogger   <- newTVarIO False
      shutdownSnapshot <- newTVarIO False
      shutdownComplete <- newEmptyTMVarIO
      liveVar          <- newTVarIO moment
      writVar          <- newTVarIO (lastBatch, moment)
      logImmediately   <- newTVarIO False
      logReceiptQueue  <- newTBQueueIO receiptQueueMax

      runner <- atomically do
          vMoment  <- newTVar moment
          vWorkers <- newTVar mempty
          reads    <- newTBQueue receiptQueueMax
          shutdown <- newTVar False

          pure RUNNER{..}

      -- TODO: Hack because I don't understand how this is supposed
      -- to work.
      initialFlows <- newEmptyMVar

      machine <- mdo
        snapshotAsync <- asyncOnCurProcess $ withThreadName "Snapshot" $ do
          setThreadSortIndex (-1)
          handle (onErr "snapshot") $ snapshotFun machine

        loggerAsync <- asyncOnCurProcess $ withThreadName "Log" $ do
          setThreadSortIndex (-2)
          handle (onErr "log") $ logFun machine

        runnerAsync <- asyncOnCurProcess $ withThreadName threadName $ do
          handle (onErr "runner") $
            runnerFun initialFlows machine processName runner

        let workersAsyncs = runner.vWorkers

        let machine = MACHINE{..}
        pure machine

      flows <- takeMVar initialFlows
      pure (machine, flows)

    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- | Synchronously shuts down a machine and wait for it to exit.
shutdownMachine :: Machine -> IO ()
shutdownMachine machine@MACHINE{..} = do
  readTVarIO workersAsyncs >>= traverse_ (\(_,worker) -> cancel worker.thread)
  -- Kill the runner to immediately cancel any computation being done.
  cancel runnerAsync
  waitForLogAsyncShutdown machine

waitForLogAsyncShutdown :: Machine -> IO ()
waitForLogAsyncShutdown MACHINE{..} = do
  -- Wait for the logging system to finish any logging task it's already
  -- started on.
  atomically $ writeTVar shutdownLogger True
  wait loggerAsync
  wait snapshotAsync
  atomically $ putTMVar shutdownComplete ()

{-
    An eval that was timed out or crashed requires zero work to replay,
    since we persist the result instead of re-computing it during replay.
-}
{-
workToReplayEval :: EvalOutcome -> NanoTime
workToReplayEval = \case
    OKAY w _ -> w
    CRASH{}  -> 0
    TIMEOUT  -> 0
-}

-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Cog Runner --------------------------------------------------------------

runnerFun :: Debug => MVar [Flow] -> Machine -> ByteString -> Runner -> IO ()
runnerFun initialFlows machine processName st = do
    -- Process the initial syscall vector
    (flows, onCommit) <- updateRunner st Nothing

    -- We got here via replay, so any synchronous requests are
    -- immediately safe to execute.
    for_ onCommit atomically
        -- TODO: do this all in one atomically?

    -- The code that calls us wants these.
    putMVar initialFlows flows

    readers <- asyncOnCurProcess $ withThreadName "readHandler" $ do
          setThreadSortIndex (-1)
          handle (onErr "readHandler") readHandler

    -- Run the event loop until we're forced to stop.
    machineTick

    cancel readers

    -- Force a sync to shutdown.
    waitForLogAsyncShutdown machine
  where
    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

    -- We've completed a unit of work. Time to tell the logger about
    -- the new state of the world.
    exportState :: (Receipt, [STM OnCommitFlow]) -> STM ()
    exportState (receipt, onCommit) = do
        when (length onCommit > 0) do
            writeTVar machine.logImmediately True
        writeTBQueue machine.logReceiptQueue (receipt, onCommit)

    machineTick :: IO ()
    machineTick = do
      writeRequests <- atomically do
        workers <- readTVar st.vWorkers
        allWrites <- concat <$> for (mapToList workers) \(i,(_,worker)) -> do
          pendingWrites <- flushTQueue worker.writes
          pure $ zip (repeat $ PROC_ID $ fromIntegral i) pendingWrites
        guarded (not . null) allWrites
      results <- cogTick writeRequests
      atomically do
          mapM_ exportState results
          readTVar st.vMoment >>= writeTVar machine.liveVar
      machineTick

    cogTick :: [(ProcId, WriteRequest)] -> IO [(Receipt, [STM OnCommitFlow])]
    cogTick writeReqs =
      withProcessName processName $
        withThreadName "CogId" $ do
          let endFlows = [] -- TODO [writeReqs.flow]
          let traceArg = M.singleton "Request indices"
                       $ Right
                       $ "writeReqs.idx" -- TODO parse this out

          withTracingFlow "WriteRequest" "cog" traceArg endFlows $ do
            (flows, receipts) <- runWrites st writeReqs
            pure (flows, [], receipts)

    readHandler :: IO ()
    readHandler = -- unlessM (readTVarIO machine.shutdownWorkers)
         do
      readRequests <- atomically do
        workers <- readTVar st.vWorkers
        concat <$> for workers \(_,worker) -> case worker of
          LIVE_EVAL{}      -> pure []
          LIVE_EXEC{reads} -> flushTQueue reads
      unless (null readRequests) do
        moment <- readTVarIO st.vMoment
        let cogFun = fromMaybe (error "trying to read from a non-spinning cog")
                   $ cogSpinningFun moment.val
        runReads cogFun readRequests
      readHandler

runReads :: Debug => Fan -> [ReadRequest] -> IO ()
runReads cogFun readReqs = do
    let fun = getCurrentReadsNoun cogFun
    for_ readReqs \(COG_READ idx query (STVAR return)) -> do
        (_, result) <- evalWithTimeout thirtySecondsInMicroseconds [] fun
                        [ getCurrentDbNoun cogFun
                        , NAT $ fromIntegral idx.int
                        , query ]
--      traceM $ unlines ["reading from cog: " <> show cogFun
--                       ,"dbstate: " <> show (getCurrentDbNoun cogFun)
--                       ,"reads function: " <> show fun
--                       ,"workers: " <> show (getCurrentWorkerNoun cogFun)
--                       ,"query: " <> show query
--                       ,"result: " <> show result]
        atomically . writeTVar return $ case result of
            OKAY _ resultFan -> DONE resultFan FlowDisabled -- TODO check old var, probably not use CallStateVar?
            _                -> DONE (NAT 0) FlowDisabled -- TODO real flow? do we need flow here?

runWrites :: Debug
          => Runner -> [(ProcId, WriteRequest)]
          -> IO ([Flow], [(Receipt, [STM OnCommitFlow])])
runWrites st writeReqs = do
    moment <- readTVarIO st.vMoment
    let fun = fromMaybe (error "Trying to run a stopped cog")
            $ cogSpinningFun moment.val
    (runtimeUs, result) <-
        withAlwaysTrace "Eval" "cog" $ do
            res <- evalWithTimeout thirtySecondsInMicroseconds [] fun [toNoun writeReqs]
            pure res

    let resultReceipt = case result of
          TIMEOUT      -> RECEIPT_TIME_OUT{timeoutAmount=runtimeUs}
          CRASH op arg -> RECEIPT_CRASHED{..}
          OKAY{}       -> makeOKReceipt writeReqs

    let (newCog, output) = case result of
          OKAY _ v ->
            let newCog = fanIdx 0 v
                output = fanIdx 1 v
             in if hasNonzeroReqs newCog
                 then (CG_SPINNING newCog, fromMaybe mempty $ fromNoun output)
                 else (CG_FINISHED newCog, mempty)
          CRASH op arg     -> (CG_CRASHED op arg fun, mempty)
          TIMEOUT          -> (CG_TIMEOUT runtimeUs fun, mempty)

    withAlwaysTrace "Tick" "cog" do
        (startedFlows, onPersists) <- updateRunner st $ Just (newCog,runtimeUs,output)
        let receiptPairs = [(resultReceipt, onPersists)]
        pure (startedFlows, receiptPairs)
{-
    Hack to avoid comparing workers.

    It's not expensive to replace a worker, but can be expensive
    to check them for equality.

    This causes workers to just always be considered non-matching,
    unless they happen to be pointer-equals.

    TODO: Find a better solution, since this could needlessly kill workers.
-}
keep :: Fan -> Fan -> Bool
keep x y =
    case reallyUnsafePtrEquality# x y of
        1# -> True
        _  -> False

concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip a = let (f, s) = unzip a
                in (concat f, concat s)

updateRunner :: Debug
             => Runner -> Maybe (CogState, NanoTime, Array (ProcId, Int, Fan))
             -> IO ([Flow], [STM OnCommitFlow])
updateRunner runner update = do
    (sideEffects, flows, onCommit) <- atomically do
      whenJust update \(newCog, timeDelta, _) ->
        modifyTVar' runner.vMoment \m ->
          m { val=newCog, work=(m.work + timeDelta) }
      toOutside <- parseWorkers
      whenJust update \(_, _, outputs) -> forM_ outputs \(PROC_ID worker, reqIdx, msg) ->
        lookup (fromIntegral worker) <$> readTVar runner.vWorkers >>= \case
          Just (_, LIVE_EXEC{inbox}) -> inbox $ RTUP (RequestIdx reqIdx) (Just msg)
          _                          -> pass
      pure toOutside
    sideEffects $> (flows, onCommit)
  where

    -- | Given a noun in a runner, update the `workers`
    parseWorkers :: Debug => STM (IO (), [Flow], [STM OnCommitFlow])
    parseWorkers = do
        cogState <- (.val) <$> readTVar runner.vMoment
        oldWorkers <- readTVar runner.vWorkers

        let init = PRS{workers=oldWorkers, newWorkers=mempty, flows=[], onPersist=[], cancels=pass}
        let newWorkers = case cogState of
              CG_CRASHED{}       -> mempty
              CG_TIMEOUT{}       -> mempty
              CG_FINISHED{}      -> mempty
              CG_SPINNING cogFun -> getCurrentWorkerNoun cogFun

        st <- flip execStateT init do

            for_ (mapToList oldWorkers) \(i,(v,_)) ->
                case newWorkers V.!? i of
                    Nothing           -> deleteWorker i
                    Just w | keep v w -> pass
                    Just w            -> deleteWorker i >> addWorker i w

            for_ (zip [0..] $ toList newWorkers) \(i,v) ->
                unless (member i oldWorkers) $ addWorker i v

        writeTVar runner.vWorkers st.workers

        -- Once updateRunner has completed all its atomic actions, we need to
        -- cancel old workers and spawn new ones.
        --
        -- Canceling old workers after updating the worker map is correct even
        -- though old worker threads might linger, because we always pull every
        -- WriteRequest and ReadRequest we process from the map. A thread might
        -- continue to queue up stale requests for a while, but we have already
        -- deleted our reference to its queue(s) and will never see them.
        --
        -- Spawning new worker threads and putting their handles back in the map
        -- can't be done atomically since IO can't be lifted into STM, but this
        -- is still safe since we're the only thread that writes to the map, and
        -- any readers don't care about the handles, which is the only field in
        -- `Worker` that `startWorkerThread` changes.

        let sideEffects = do
              st.cancels
              new <- traverse sequence st.newWorkers
              atomically $ modifyTVar' runner.vWorkers (union new)

        pure (sideEffects, st.flows, st.onPersist)

      where
        deleteWorker :: MonadState ParseRequestsState m => Int -> m ()
        deleteWorker key = do
            use (#workers % at key) >>= \case
                Nothing -> error "deleteWorker: impossible"
                Just (_, worker) -> do
                    modifying' #workers $ deleteMap key
                    modifying' #cancels (>> cancel worker.thread)

        addWorker :: Int -> Fan -> StateT ParseRequestsState STM ()
        addWorker i fan = do
          whenJust (fromNoun fan) \w -> do
            staged <- lift $ stageWorker runner.ctx.hw w
            modifying' #newWorkers $ insertMap i (fan, staged)


evalWithTimeout
    :: Debug
    => Nat
    -> [IO ()]
    -> Fan
    -> [Fan]
    -> IO (NanoTime, EvalOutcome)
evalWithTimeout msTimeout preActions fun args = do
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
      evaluate $ force (foldl' (%%) fun args)

evalCheckingCrash
    :: Debug
    => Fan
    -> Fan
    -> IO (NanoTime, EvalOutcome)
evalCheckingCrash fun arg = do
  (runtime, raw) <- withCalcRuntime $ do
    try (evaluate $ force (fun %% arg)) >>= \case
      Left (PRIMOP_CRASH op val) -> do debug ("crash"::Text, NAT op, val)
                                       pure (Left (op,val))
      Right f                    -> pure (Right f)
  case raw of
    Left (o,e) -> pure (runtime, CRASH o e)
    Right v    -> pure (runtime, OKAY runtime v)

-- The EventLog Routine --------------------------------------------------------

data LogNext = LOG_NEXT
    { shutdown :: Bool
    , batchNum :: BatchNum
    , receipts :: [(Receipt, [STM OnCommitFlow])]
    , moment   :: Moment
    }

{-
    This is the per-cog event-log persistence logic that runs in
    `loggerAsync`.

    Event logging is almost entirely asynchronous w.r.t ship execution,
    the only exception is that some syscalls are "synchronous" and they
    need to block until the state that triggered them has been committed
    to disk.

    Like snapshotting, the decision of whether or not to write a log
    batch depends on the amount of cpu-work has been performed since
    the last batch.

    We also write a final log batch right before shutdown, if the daemon
    receives a SIGKILL.  This isn't synchronized with snapshot routine,
    so the final disk state will often be a snapshot with a single log
    batch that needs to be replayed, but this should be cheap in practice.

    There is also the `logImmediately` flag, which will cause the log
    to be written right away.  This is important for "synchronous" events,
    since we want to cause those to block for as little time as possible.
-}
logFun :: Debug => Machine -> IO ()
logFun machine =
    loop
  where
    loop :: IO ()
    loop = do
        next <- withAlwaysTrace "Block" "log" (atomically getNextBatch)

        -- It's possible for next.receipts to be empty if a shutdown
        -- is requested and no events have happened since the last batch
        -- was written.
        withTracingFlow "Log" "log" mempty [] $ do
            if null next.receipts then do
                pure ([], [], ())
            else do
                (startFlows, stepFlows) <- doBatch next
                pure (startFlows, stepFlows, ())

        case next.shutdown of
          True  -> atomically $ writeTVar machine.shutdownSnapshot True
          False -> loop

    -- Either reads a shutdown command or the next batch of work to log.
    getNextBatch :: STM LogNext
    getNextBatch = do
        shutdown             <- readTVar machine.shutdownLogger
        live                 <- readTVar machine.liveVar
        (lastBatchNum, writ) <- readTVar machine.writVar
        forcedLog            <- readTVar machine.logImmediately
        fullQ                <- isFullTBQueue machine.logReceiptQueue

        -- TODO: IIUC "workNs" is currently actually just the runtime.
        -- The idea was that we would use "total execution time" to
        -- trigger snapshots, in order to bound the amount of time needed
        -- to replay.  This makes no sense for log batches, and the
        -- total-runtime makes no sense for snapshots.  Shit is working,
        -- however, so there's no need to changes this atm.

        let timedLog  = live.work > (writ.work + logbatchWorkIntervalInNs)
        let shouldLog = shutdown || forcedLog || fullQ || timedLog

        unless shouldLog retry

        when forcedLog (writeTVar machine.logImmediately False)

        receipts <- flushTBQueue machine.logReceiptQueue
        moment   <- readTVar machine.liveVar

        let batchNum = lastBatchNum + 1
        pure LOG_NEXT{shutdown, receipts, moment, batchNum}

    doBatch :: LogNext -> IO ([Flow], [Flow])
    doBatch next = do
        let (receipts, onCommit) = unzip next.receipts

        now <- getNanoTime

        writeLogBatch machine.ctx.lmdb $
            LogBatch
                { batchNum  = next.batchNum
                , writeTime = now
                , executed  = receipts
                }

        atomically $ writeTVar machine.writVar (next.batchNum, next.moment)

        -- Perform the associated STM actions that were supposed to be
        -- run once committed.  This is used for synchronous syscalls
        -- which need to block until we have committed to the state that
        -- triggered them.
        unzipOnCommitFlows <$> traverse atomically (join onCommit)


-- The Snapshotting Routine ----------------------------------------------------

{-
    The is the per-machine snapshotting logic that runs in `snapshotAsync`.

    Snapshotting is fully asynchronous.  It doesn't block normal
    execution, so we allow it to lag behind the current state.

    When we replay, we load the latest snapshot, and then we find all
    of the log batches that come after that point.

    Here, we track the mount of CPU-work that will be required to replay those
    batches, and we wait until that reaches a certain threshold before
    taking a snapshot.

    We also take a snapshot right before shutdown, when the daemon receives a
    SIGKILL.
-}
snapshotFun :: Debug => Machine -> IO ()
snapshotFun machine =
    when machine.ctx.enableSnaps do
        loop 0
  where
    loop lastSnapWork = do
        let nextSnapWork = lastSnapWork + snapshotWorkIntervalInNs

        (shutdown, bn, moment) <-
            withAlwaysTrace "Block" "log" $
            atomically do
                shutdown           <- readTVar machine.shutdownSnapshot
                (logBN, logMoment) <- readTVar machine.writVar

                let shouldSnap = shutdown || logMoment.work > nextSnapWork

                unless shouldSnap retry

                pure (shutdown, logBN, logMoment)

        withAlwaysTrace "Snapshot" "log" do
            writeMachineSnapshot machine.ctx.lmdb bn moment.val

        unless shutdown do
            loop moment.work
