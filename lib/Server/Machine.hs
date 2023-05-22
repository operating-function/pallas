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
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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

import Control.Concurrent.STM.TQueue (flushTQueue)
import Control.Monad.State           (StateT, execStateT, modify', runStateT)
import Data.Vector                   ((!))
import GHC.Conc                      (unsafeIOToSTM)
import GHC.Prim                      (reallyUnsafePtrEquality#)
-- ort Text.Show.Pretty              (ppShow)
import System.Random (randomIO)

import Fan (Fan(..), PrimopCrash(..), getRow, (%%))

import qualified Fan

import Fan.Convert
import Fan.Prof
import Server.Common         ()
import Server.Convert        ()
import Server.Debug
import Server.Evaluator
import Server.Hardware.Types
import Server.LmdbStore
import Server.Time
import Server.Types.Logging

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data Moment = MOMENT {
  val  :: IntMap Fan,
  work :: NanoTime
  }

data MachineContext = MACHINE_CONTEXT
    { machineName :: MachineName
    , lmdb        :: LmdbStore
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

  -- Signal to shut down all the asyncs. After setting this, you should wait on
  -- all three in order.
  shutdownLogger   :: TVar Bool,
  shutdownSnapshot :: TVar Bool,

  -- Noun state and time of the last commit at each of the three phases.
  liveVar          :: TVar Moment,
  writVar          :: TVar (BatchNum, Moment),

  -- | Command to log immediately (because of a pending add)
  logImmediately   :: TVar Bool,
  logReceiptQueue  :: TQueue (Receipt, [STM OnCommitFlow])
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

data Response
    = RespEval EvalOutcome
    | RespCall Fan SysCall
    | RespSpin CogId
  deriving (Show)

responseToVal :: Response -> Fan
responseToVal (RespCall f _) = f
responseToVal (RespSpin (COG_ID id)) = fromIntegral id
responseToVal (RespEval e)   =
    ROW case e of
        TIMEOUT   -> mempty              -- []
        OKAY _ r  -> singleton r         -- [f]
        CRASH n e -> fromList [NAT n, e] -- [n e]

responseToReceipt :: CogId -> Bool -> [(Int, ResponseTuple)] -> Receipt
responseToReceipt cogId didCrash =
    RECEIPT cogId didCrash . mapFromList . fmap f
  where
    f :: (Int, ResponseTuple) -> (Int, ReceiptItem)
    f (idx, tup) = case tup.resp of
        RespEval OKAY{} -> (idx, ReceiptEvalOK)
        resp            -> (idx, ReceiptVal (responseToVal resp))

data CallRequest = CR
    { durable :: Bool
    , device  :: DeviceName
    , params  :: Vector Fan
    }
  deriving (Show)

data SpinRequest = SR
    { cogFun  :: Fan
    }
  deriving (Show)

data Request
    = ReqEval EvalRequest
    | ReqCall CallRequest
    -- TODO: We'll need to intercept specific %cog hardware requests here and I
    -- need to think hard about implementing their semantics. send/recv is a
    -- case where two different formal requests have to be triggered in the
    -- same logbatch, and be written so the recv happens before the send
    -- (because of replay).
    --
    -- | ReqSend
    -- | ReqRecv
    --
    -- Maybe more easier is just:
    | ReqSpin SpinRequest
    | UNKNOWN Fan
  deriving (Show)

{-
  [%eval timeout/@ fun/fan arg/Fan ...]
  [%cog %spin fun/Fan]  -- COG_ID 0 only
  [%cog %send dst/Nat param/Fan]
  [%cog %recv]
  [$call synced/? param/Fan ...]
-}
valToRequest :: MachineName -> CogId -> Fan -> Request
valToRequest machine cogId top = fromMaybe (UNKNOWN top) do
    row <- getRow top
    _   <- guard (length row >= 3)
    tag <- fromNoun @DeviceName (row!0)
    nat <- fromNoun @Nat (row!1)

    if tag == "eval" then do
        let timeoutSecs = nat
        let timeoutMs   = timeoutSecs * 1000
        guard (length row >= 4)
        let func = row!2
        let args = V.drop 3 row
        let flow = FlowDisabled -- TODO: What?
        let er = EVAL_REQUEST{func,args,machine,cogId,flow,timeoutMs}
        pure (ReqEval er)
        -- e (ReqEval $ error "_" timeoutSecs (row!2) (V.drop 3 row))
    else if tag == "cog" then do
        case nat of
            "spin" | cogId == crankCogId -> do
                pure (ReqSpin $ SR $ row!2)
            _ -> Nothing
    else
        case nat of
            0 -> pure (ReqCall $ CR False tag $ V.drop 2 row)
            1 -> pure (ReqCall $ CR True  tag $ V.drop 2 row)
            _ -> Nothing

getCurrentReqNoun :: Fan -> Vector Fan
getCurrentReqNoun s = do
    case s of
        KLO _ xs -> do
            let len = sizeofSmallArray xs
            case (xs Fan.^ (len-1)) of
                ROW x -> x
                _     -> mempty
        _ -> mempty

data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

-- | A list of parsed out valid requests from `noun`. For every cog, for every
-- index in that cog's requests table, there is a raw fan value and a
-- `LiveRequest` which contains STM variables to listen
type CogSysCalls = IntMap (Fan, LiveRequest)
type MachineSysCalls = IntMap CogSysCalls

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { ctx       :: MachineContext
    , vMoment   :: TVar Moment    -- ^ Current value + cummulative CPU time
    , vRequests :: TVar MachineSysCalls  -- ^ Current requests table
    }


-- -----------------------------------------------------------------------

-- An effect that happens atomically after a Response is processed by a cog and
-- didn't crash.
data CogEffect
    = CSpin
      { reCogId :: CogId
      , reFun   :: Fan
      }
    deriving (Show)

data ResponseTuple = RTUP
    { key    :: RequestIdx
    , resp   :: Response
    , work   :: NanoTime
    , flow   :: Flow
    , effect :: Maybe CogEffect
    }
  deriving (Show)

-- | A `LiveRequest` is a handle that could produce a Response to an open
-- Request.
data LiveRequest
  = LiveEval {
    leIdx    :: RequestIdx,
    leRecord :: Evaluation
    }
  | LiveCall {
    lcIdx     :: RequestIdx,
    lcCancel  :: Cancel,
    lcSysCall :: SysCall
    }
  | LiveSpin {
    lsIdx :: RequestIdx,
    lsFun :: Fan
    }
  | LiveUnknown

instance Show LiveRequest where
    show = \case
        LiveEval{}  -> "EVAL"
        LiveCall{}  -> "CALL"
        LiveSpin{}  -> "SPIN"
        LiveUnknown -> "UNKNOWN"

data ParseRequestsState = PRS
    { syscalls  :: CogSysCalls
    , flows     :: [Flow]
    , onPersist :: [STM OnCommitFlow]
    }

makeFieldLabelsNoPrefix ''MachineContext
makeFieldLabelsNoPrefix ''Moment
makeFieldLabelsNoPrefix ''Runner
makeFieldLabelsNoPrefix ''ParseRequestsState

-- -----------------------------------------------------------------------
-- No template haskell beyond this point because optics.
-- -----------------------------------------------------------------------

-- | Loads and the state on disk from a given snapshot and the log batches
-- written on top of it.
performReplay
    :: Debug
    => Cushion
    -> MachineContext
    -> ReplayFrom
    -> IO (BatchNum, Moment)
performReplay cache ctx replayFrom = do
  let mkInitial (a, b) = (b, MOMENT a 0)
  loadLogBatches ctx.machineName replayFrom mkInitial replayBatch ctx.lmdb cache
  where
    replayBatch :: (BatchNum, Moment) -> LogBatch -> IO (BatchNum, Moment)
    replayBatch (_, moment) LogBatch{batchNum,executed} =
        batchLoop batchNum moment executed

    recomputeEvals
        :: Moment -> Receipt -> StateT NanoTime IO (Bool, [(Fan,Fan)])
    recomputeEvals m (RECEIPT cogId didCrash tab) =
        fmap (didCrash,) $
        for (mapToList tab) \(idx, rVal) -> do
            let k = toNoun (fromIntegral idx :: Word)
            case rVal of
                ReceiptVal val -> pure (k, val)
                ReceiptEvalOK  -> do
                    -- We performed an eval which succeeded the
                    -- first time.
                    case lookup cogId.int m.val of
                        Nothing -> throwIO INVALID_COGID_IN_LOGBATCH
                        Just cog ->
                            case getEvalFunAt cogId cog (RequestIdx idx) of
                                Nothing ->
                                    throwIO INVALID_OK_RECEIPT_IN_LOGBATCH
                                Just (fun, args)  -> do
                                    (runtime, res) <- withCalcRuntime do
                                        evaluate $ force (foldl' (%%) fun args)
                                    modify' (+ runtime)
                                    pure (k, ROW (singleton res))
                ReceiptRecv _ _ -> error "IMPLEMENT RECV REFERENCES"
                ReceiptSpun _ -> error "IMPLEMENT RESPIN IN REPLAY"

    batchLoop :: BatchNum -> Moment -> [Receipt] -> IO (BatchNum, Moment)
    batchLoop bn m []     = pure (bn, m)
    batchLoop bn m (x:xs) = do
        ((didCrash, eRes), eWork) <- flip runStateT 0 (recomputeEvals m x)
        let inp = TAB (mapFromList eRes)
        let arg = if didCrash then 0 %% inp else inp
        case lookup x.cogNum.int m.val of
          Nothing -> throwIO INVALID_COGID_IN_LOGBATCH
          Just fun -> do
            (iWork, new) <- withCalcRuntime $ evaluate $ force (fun %% arg)
            let newWork = (m.work + eWork + iWork)
            batchLoop bn MOMENT{work=newWork,
                                val=(insertMap x.cogNum.int new m.val)} xs

    getEvalFunAt :: CogId -> Fan -> RequestIdx -> Maybe (Fan, Vector Fan)
    getEvalFunAt cogId noun (RequestIdx idx) = do
      let row = getCurrentReqNoun noun
      case row V.!? idx of
        Nothing -> Nothing
        Just val -> do
          case valToRequest ctx.machineName cogId val of
            ReqEval er -> Just (er.func, er.args)
            _          -> Nothing

-- | Main entry point for starting running a Cog.
replayAndCrankMachine :: Debug => Cushion -> MachineContext -> ReplayFrom
                      -> IO Machine
replayAndCrankMachine cache ctx replayFrom = do
  let threadName  = encodeUtf8 "Main"

  withProcessName processName (wrapReplay threadName)
  where
    processName = "Machine: " <> encodeUtf8 (txt ctx.machineName)

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
      liveVar          <- newTVarIO moment
      writVar          <- newTVarIO (lastBatch, moment)
      logImmediately   <- newTVarIO False
      logReceiptQueue  <- newTQueueIO

      runner <- atomically do
          vMoment   <- newTVar moment
          vRequests <- newTVar mempty
          pure RUNNER{vMoment, ctx, vRequests}

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

        let machine = MACHINE{..}
        pure machine

      flows <- takeMVar initialFlows
      pure (machine, flows)

    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- | Synchronously shuts down a machine and wait for it to exit.
shutdownMachine :: Machine -> IO ()
shutdownMachine MACHINE{..} = do
  -- Kill the runner to immediately cancel any computation being done.
  cancel runnerAsync

  -- Wait for the logging system to finish any logging task it's already
  -- started on.
  atomically $ writeTVar shutdownLogger True
  wait loggerAsync
  wait snapshotAsync

-- | Given a Request parsed from the cog, turn it into a LiveRequest that can
-- produce a value and that we can listen to.
buildLiveRequest
    :: Debug
    => Flow
    -> MachineContext
    -> CogId
    -> RequestIdx
    -> Request
    -> STM ([Flow], LiveRequest, Maybe (STM OnCommitFlow))
buildLiveRequest causeFlow ctx cogId reqIdx = \case
    ReqEval EVAL_REQUEST{..} -> do -- timeoutMs func args -> do
        let req = EVAL_REQUEST{flow=causeFlow, timeoutMs, func, args,
                               machine=ctx.machineName, cogId}
        leRecord <- pleaseEvaluate ctx.eval req
        pure ([], LiveEval{leIdx=reqIdx, leRecord}, Nothing)

    ReqCall cr -> do
        callSt <- STVAR <$> newTVar LIVE

        let lcSysCall = SYSCALL ctx.machineName cogId cr.device cr.params
                                callSt causeFlow
        -- If the call requires durability, pass the STM action to start
        -- it back to the caller. Otherwise, just run the action and
        -- pass back nothing.
        (startedFlows, logCallback, lcCancel) <-
            if cr.durable then do
                canceled <- newTVar False
                vCancel  <- newTVar (CANCEL $ writeTVar canceled True)

                -- If the cancel happens before the disk sync, then we
                -- never submit the request to hardware.  If the cancel
                -- happens after `onCommit`, then we run the actual cancel
                -- action.

                let onCancel = do
                        cancel <- readTVar vCancel
                        cancel.action

                let onCommit = do
                        isCanceled <- readTVar canceled
                        startedFlows <- case isCanceled of
                          True -> pure []
                          False -> do
                            (cancel, startedFlows) <-
                              callHardware ctx.hw cr.device lcSysCall
                            writeTVar vCancel cancel
                            pure startedFlows
                        pure $ OnCommitFlow causeFlow startedFlows

                pure ([], Just onCommit, CANCEL onCancel)
            else do
                (cancel, flows) <- callHardware ctx.hw cr.device lcSysCall
                pure (flows, Nothing, cancel)

        pure (startedFlows, LiveCall{lcIdx=reqIdx, lcSysCall, lcCancel},
              logCallback)

    ReqSpin (SR func) -> do
      pure ([], LiveSpin{lsIdx=reqIdx, lsFun=func}, Nothing)

    UNKNOWN _ -> do
        pure ([], LiveUnknown, Nothing)

cancelRequest :: LiveRequest -> STM ()
cancelRequest LiveEval{..} = leRecord.cancel.action
cancelRequest LiveCall{..} = lcCancel.action
cancelRequest LiveSpin{}   = pure () -- Not asynchronous
cancelRequest LiveUnknown  = pure ()

{-
    An eval that was timed out or crashed requires zero work to replay,
    since we persist the result instead of re-computing it during replay.
-}
workToReplayEval :: EvalOutcome -> NanoTime
workToReplayEval = \case
    OKAY w _ -> w
    CRASH{}  -> 0
    TIMEOUT  -> 0

receiveResponse
  :: TVar Moment
  -> (Fan, LiveRequest)
  -> STM (Maybe ResponseTuple)
receiveResponse momentVar = \case
    (_, LiveEval{..}) -> do
        getEvalOutcome leRecord >>= \case
            Nothing -> pure Nothing
            Just (outcome, flow) -> do
                let work = workToReplayEval outcome
                pure (Just RTUP{key=leIdx, resp=RespEval outcome, work, flow,
                                effect=Nothing})

    (_, LiveCall{..}) -> do
        getCallResponse lcSysCall >>= \case
            Nothing -> pure Nothing
            Just (fan, flow) -> do
                let resp = RespCall fan lcSysCall
                -- TODO: Handle runtimeNs here, if we spent seconds on
                -- an http transfer, we want to commit that as work so
                -- we wouldn't fetch the same file again when restarting.
                --
                -- TODO: Reconsider the above TODO?  Replay does not
                -- run effects again.
                pure (Just RTUP{key=lcIdx, resp, work=0, flow, effect=Nothing})

    (_, LiveSpin{..}) ->
      do
        cogid <- getRandomNonConflictingId =<< readTVar momentVar
        -- TODO: How should the flows be represented when spinning a new cog!?
        pure (Just RTUP{key=lsIdx, resp=RespSpin cogid, work=0,
                        flow=FlowDisabled, effect=Just (CSpin{reCogId=cogid
                                                             ,reFun=lsFun})})
      where
        getRandomNonConflictingId :: Moment -> STM CogId
        getRandomNonConflictingId moment = do
          -- Word8 is a very temporary hack: this should not be a negative
          -- number, but IntMap only is bound to +/- 2^29-1.
          r :: Word8 <- unsafeIOToSTM $ randomIO
          case lookup (fromIntegral r) moment.val of
            Nothing -> pure $ COG_ID $ fromIntegral r
            Just _  -> getRandomNonConflictingId moment

    (_, LiveUnknown) -> do
        pure Nothing

-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Cog Runner --------------------------------------------------------------

runnerFun :: Debug => MVar [Flow] -> Machine -> ByteString -> Runner -> IO ()
runnerFun initialFlows machine processName st =
    withMachineHardwareInterface machine.ctx.machineName machine.ctx.hw do

        -- Process the initial syscall vector
        (flows, onCommit) <- atomically (parseAllRequests st)

        -- We got here via replay, so any synchronous requests are
        -- immediately safe to execute.
        for_ onCommit atomically
            -- TODO: do this all in one atomically?

        -- The code that calls us wants these.
        putMVar initialFlows flows

        -- Run the event loop
        forever machineTick

  where
    -- We've completed a unit of work. Time to tell the logger about
    -- the new state of the world.
    exportState :: Receipt -> [STM OnCommitFlow] -> STM ()
    exportState receipt onCommit = do
        when (length onCommit > 0) do
            writeTVar machine.logImmediately True
        writeTQueue machine.logReceiptQueue (receipt, onCommit)

    -- TODO: Optimize
    collectResponses :: IntMap (Maybe a) -> [(Int, a)]
    collectResponses imap =
        go [] (mapToList imap)
      where
        go !acc []                  = acc
        go !acc ((k,Just v) : kvs)  = go ((k,v):acc) kvs
        go !acc ((_,Nothing) : kvs) = go acc         kvs

    mkCogResponses :: MachineSysCalls -> [STM (CogId, [(Int, ResponseTuple)])]
    mkCogResponses msc = map build $ mapToList msc
      where
        build :: (Int, CogSysCalls) -> STM (CogId, [(Int, ResponseTuple)])
        build (k, sysCalls) = do
           returns <- fmap collectResponses
                    $ traverse (receiveResponse st.vMoment)
                    $ sysCalls

           when (null returns) retry

           pure (COG_ID k, returns)

    -- Collects all responses that are ready for one cog.
    takeReturns :: IO (CogId, [(Int, ResponseTuple)])
    takeReturns = do
        withAlwaysTrace "WaitForResponse" "cog" do
            -- This is in a separate atomically block because it doesn't
            -- change and we want to minimize contention.
            machineSysCalls <- atomically (readTVar st.vRequests)
            atomically $ asum $ mkCogResponses machineSysCalls

    -- Every machineTick, we try to pick off as much work as possible for cogs
    -- to do.
    --
    -- TODO: This is the simple way of doing things, there's a ton of runtime
    -- gains that could be had at the cost of complexity by having a persistent
    -- async for each Cog, but that adds some sync subtlety while this means
    -- the machine is as fast as only the slowest cog, but is obviously correct.
    machineTick :: IO ()
    machineTick = do
        (cogId, valTuples) <- takeReturns
        result <- cogTick (cogId, valTuples)
        atomically $ do
            case result of
              Just (onC,r) -> exportState r onC
              _            -> pure ()

            moment <- readTVar st.vMoment
            writeTVar machine.liveVar moment

    --
    cogTick
      :: (CogId, [(Int, ResponseTuple)])
      -> IO (Maybe ([STM OnCommitFlow], Receipt))
    cogTick (cogId, valTuples) =
      withProcessName processName $
        withThreadName ("Cog: " <> (encodeUtf8 $ tshow cogId)) $ do
          let endFlows = (.flow) . snd <$> valTuples
          let traceArg = M.singleton "syscall indicies"
                       $ Right
                       $ tshow
                       $ fmap fst valTuples

          withTracingFlow "Response" "cog" traceArg endFlows $
            runResponse st cogId valTuples False >>= \case
              Nothing -> pure ([], [], Nothing)
              Just (flows, onCommit, receipt) ->
                pure (flows, [], Just (onCommit, receipt))

runResponse
    :: Debug
    => Runner
    -> CogId
    -> [(Int, ResponseTuple)]
    -> Bool
    -> IO (Maybe ([Flow], [STM OnCommitFlow], Receipt))
runResponse st cogid rets didCrash = do
  let arg = TAB
          $ mapFromList
          $ flip fmap rets
          $ \(k,v) -> (NAT (fromIntegral k), responseToVal v.resp)
  moment <- atomically (readTVar st.vMoment)
  let fun = fromMaybe (error "Invalid cogid") $ lookup cogid.int moment.val
  (runtimeUs, result) <- do
      withAlwaysTrace "Eval" "cog" do
          evalWithTimeout thirtySecondsInMicroseconds fun
              (if didCrash then (0 %% arg) else arg)
  case result of
    TIMEOUT -> recordInstantEvent "Main Timeout"   "cog" mempty >> onCrash
    CRASH{} -> recordInstantEvent "Main Exception" "cog" mempty >> onCrash
    OKAY _ result -> do
      withAlwaysTrace "Tick" "cog" do
        (sideEffectFlows, sideEffectPersists) <- concatUnzip <$> atomically do
            -- Update the runner state, making sure to delete the consumed
            -- LiveRequest without formally canceling it because it already
            -- completed.
            modifyTVar' st.vMoment   \m ->
                MOMENT (insertMap cogid.int result m.val) (m.work + runtimeUs)
            modifyTVar' st.vRequests $
                adjustMap (\kals -> foldl' delReq kals (fst<$>rets)) cogid.int

            -- If this was a successful run of the main event, do whatever side
            -- effect running this event was supposed to have.
            case didCrash of
                True -> pure mempty
                False -> forM rets $ \(_,RTUP{effect}) -> case effect of
                    Nothing -> pure (mempty, mempty)
                    Just CSpin{..} -> do
                        modifyTVar' st.vMoment \m ->
                            m { val=insertMap reCogId.int reFun m.val }
                        parseRequests st reCogId

        (startedFlows, onPersists) <- atomically (parseRequests st cogid)

        pure $ Just ( startedFlows ++ sideEffectFlows
                    , onPersists ++ sideEffectPersists
                    , responseToReceipt cogid didCrash rets
                    )
 where
   onCrash =
       if didCrash
       then throwIO COG_DOUBLE_CRASH
       else runResponse st cogid rets True

   delReq :: CogSysCalls -> Int -> CogSysCalls
   delReq acc k = IM.delete k acc

{-
    Hack to avoid comparing SERV requests.

    It's not expensive to replace the SERV request, but it is expensive
    to check them for equality.

    This causes SERV requests to just always be considered non-matching,
    unless the two requests happen to be pointer-equals.

    TODO: Find a better solution
-}
keep :: Fan -> Fan -> Bool
keep x y =
    case reallyUnsafePtrEquality# x y of
        1# -> True
        _  ->
           case x of
               ROW rs ->
                   if length rs == 4 && rs!0 == "http" && rs!2 == "serv"
                   then False
                   else x==y
               _ -> x==y


concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip a = let (f, s) = unzip a
                in (concat f, concat s)

parseAllRequests :: Debug => Runner -> STM ([Flow], [STM OnCommitFlow])
parseAllRequests runner = do
  cogs <- (keys . (.val)) <$> readTVar runner.vMoment
  actions <- forM cogs $ \i -> parseRequests runner (COG_ID i)
  pure $ concatUnzip actions

-- | Given a noun in a runner, update the `requests`
parseRequests :: Debug => Runner -> CogId -> STM ([Flow], [STM OnCommitFlow])
parseRequests runner cogId = do
    moment   <- (fromMaybe (error "no cogid m") . lookup cogId.int . (.val)) <$>
                readTVar runner.vMoment
    syscalls <- (fromMaybe mempty . lookup cogId.int) <$>
                readTVar runner.vRequests

    let init = PRS{syscalls, flows=[], onPersist=[]}

    st <- flip execStateT init do
              let expected = syscalls
              let actual   = getCurrentReqNoun moment

              for_ (mapToList expected) \(i,(v,_)) ->
                  case actual V.!? i of
                      Nothing           -> cancelReq i
                      Just w | keep v w -> pure ()
                      Just w            -> cancelReq i >> createReq i w

              for_ (zip [0..] $ toList actual) \(i,v) ->
                  unless (member i expected) do
                      createReq i v

    modifyTVar' runner.vRequests $ insertMap cogId.int st.syscalls

    pure (st.flows, st.onPersist)

  where

    cancelReq :: Int -> StateT ParseRequestsState STM ()
    cancelReq key = do
        -- traceM ("cancelReq:" <> show key)
        slot <- use (#syscalls % at key)
        case slot of
            Nothing -> error "impossible"
            Just (_, liveReq) -> do
                modifying' #syscalls (deleteMap key)
                lift (cancelRequest liveReq)

    reqDebugSummary :: Request -> ByteString
    reqDebugSummary = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          describeSyscall runner.ctx.hw rc.device rc.params)
        ReqSpin{}  -> "%spin"
        UNKNOWN{}  -> "UNKNOWN"

    flowCategory :: Request -> ByteString
    flowCategory = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          syscallCategory runner.ctx.hw rc.device rc.params)
        ReqSpin{}  -> "%spin"
        UNKNOWN{}  -> "UNKNOWN"

    createReq :: Int -> Fan -> StateT ParseRequestsState STM ()
    createReq i v = do
        let request = valToRequest runner.ctx.machineName cogId v
            reqData = reqDebugSummary request
            reqName = "Cog " <> (encodeUtf8 $ tshow cogId.int)
                   <> " idx "
                   <> encodeUtf8 (tshow i) <> ": " <> reqData
            catData = flowCategory request
        f <- lift $ allocateRequestFlow reqName (decodeUtf8 catData)

        let rIdx = RequestIdx i

        (startedFlows, live, onPersist) <- lift $
          buildLiveRequest f runner.ctx cogId rIdx request

        case onPersist of
            Nothing -> pure ()
            Just cb -> modifying' #onPersist (cb:)

        modifying' #syscalls (insertMap i (v, live))
        modifying' #flows  (++ (f:startedFlows))

evalWithTimeout
    :: Debug
    => Nat
    -> Fan
    -> Fan
    -> IO (NanoTime, EvalOutcome)
evalWithTimeout msTimeout fun arg = do
  (runtime, raw) <- withCalcRuntime $ timeout (fromIntegral msTimeout) $ do
    try (evaluate $ force (fun %% arg)) >>= \case
      Left (PRIMOP_CRASH op val) -> do debug ("crash"::Text, NAT op, val)
                                       pure (Left (op,val))
      Right f                    -> pure (Right f)
  case raw of
    Nothing          -> pure (runtime, TIMEOUT)
    Just (Left(o,e)) -> pure (runtime, CRASH o e)
    Just (Right v)   -> pure (runtime, OKAY runtime v)


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

        let nextLogWork = writ.work + logbatchWorkIntervalInNs
        let shouldLog   = shutdown || forcedLog || (live.work > nextLogWork)

        unless shouldLog retry

        when forcedLog (writeTVar machine.logImmediately False)

        receipts <- flushTQueue machine.logReceiptQueue
        moment   <- readTVar machine.liveVar

        pure LOG_NEXT{batchNum=(lastBatchNum + 1), ..}

    doBatch :: LogNext -> IO ([Flow], [Flow])
    doBatch next = do
        let (receipts, onCommit) = unzip next.receipts

        now <- getNanoTime

        writeLogBatch machine.ctx.lmdb machine.ctx.machineName $
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
        (starts, steps) <- unzipOnCommitFlows <$>
            traverse atomically (join onCommit)
        pure (starts, steps)


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
            writeMachineSnapshot machine.ctx.lmdb machine.ctx.machineName bn
                                 moment.val

        unless shutdown do
            loop moment.work
