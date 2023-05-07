-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-
    TOPLEVEL TODOS:

    - TODO: `shutdownCog` works for now, but what we really want is a way to hit
            ctrl-c once to begin shutdown and then have a second ctrl-c which
            aborts snapshotting.
-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server.Cog
    ( Cog(..)
    , CogContext(..)
    , Moment(..)
    , performReplay
    , replayAndSpinCog
    , shutdownCog
    )
where

import PlunderPrelude

import Control.Concurrent.STM.TQueue (flushTQueue)
import Control.Monad.State           (StateT, execStateT, modify', runStateT)
import Data.Vector                   ((!))
import GHC.Prim                      (reallyUnsafePtrEquality#)
-- ort Text.Show.Pretty              (ppShow)

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
  val  :: Fan,
  work :: NanoTime
  }

data CogContext = COG_CONTEXT
    { cogName     :: CogName
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

-- | Toplevel handle for a Cog, a fan evaluation that makes requests and
-- receives responses.
--
-- Internally, cog execution has three long lived asyncs: a `Runner` async, a
-- `Logger` async, and a `Snapshotter` async. These communicate and are
-- controlled via the following STM variables:
data Cog = COG {
  ctx              :: CogContext,

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
  deriving (Show)

responseToVal :: Response -> Fan
responseToVal (RespCall f _) = f
responseToVal (RespEval e)   =
    ROW case e of
        TIMEOUT   -> mempty              -- []
        OKAY _ r  -> singleton r         -- [f]
        CRASH n e -> fromList [NAT n, e] -- [n e]

responseToReceipt :: Bool -> [(Int, ResponseTuple)] -> Receipt
responseToReceipt didCrash =
    RECEIPT didCrash . mapFromList . fmap f
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

data Request
    = ReqEval EvalRequest
    | ReqCall CallRequest
    | UNKNOWN Fan
  deriving (Show)

{-
  [%eval timeout/@ fun/fan arg/Fan ...]
  [$call synced/? param/Fan ...]
-}
valToRequest :: CogName -> Fan -> Request
valToRequest cog top = fromMaybe (UNKNOWN top) do
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
        let er = EVAL_REQUEST{func,args,cog,flow,timeoutMs}
        pure (ReqEval er)
        -- e (ReqEval $ error "_" timeoutSecs (row!2) (V.drop 3 row))
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

-- | A list of parsed out valid requests from `noun`. For every index
-- in the requests table, there is a raw fan value and a `LiveRequest`
-- which contains STM variables to listen
type SysCalls = IntMap (Fan, LiveRequest)

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { ctx       :: CogContext
    , vMoment   :: TVar Moment    -- ^ Current value + cummulative CPU time
    , vRequests :: TVar SysCalls  -- ^ Current requests table
    }


-- -----------------------------------------------------------------------

data ResponseTuple = RTUP
    { key  :: RequestIdx
    , resp :: Response
    , work :: NanoTime
    , flow :: Flow
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
  | LiveUnknown

instance Show LiveRequest where
    show = \case
        LiveEval{}  -> "EVAL"
        LiveCall{}  -> "CALL"
        LiveUnknown -> "UNKNOWN"


data ParseRequestsState = PRS
    { syscalls  :: SysCalls
    , flows     :: [Flow]
    , onPersist :: [STM OnCommitFlow]
    }

makeFieldLabelsNoPrefix ''CogContext
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
    -> CogContext
    -> ReplayFrom
    -> IO (BatchNum, Moment)
performReplay cache ctx replayFrom = do
  let mkInitial (a, b) = (b, MOMENT a 0)
  loadLogBatches ctx.cogName replayFrom mkInitial replayBatch ctx.lmdb cache
  where
    replayBatch :: (BatchNum, Moment) -> LogBatch -> IO (BatchNum, Moment)
    replayBatch (_, moment) LogBatch{batchNum,executed} =
        batchLoop batchNum moment executed

    recomputeEvals
        :: Moment -> Receipt -> StateT NanoTime IO (Bool, [(Fan,Fan)])
    recomputeEvals m (RECEIPT didCrash tab) =
        fmap (didCrash,) $
        for (mapToList tab) \(idx, rVal) -> do
            let k = toNoun (fromIntegral idx :: Word)
            case rVal of
                ReceiptVal val -> pure (k, val)
                ReceiptEvalOK  -> do
                    -- We performed an eval which succeeded the
                    -- first time.
                    case getEvalFunAt m.val (RequestIdx idx) of
                        Nothing -> throwIO INVALID_OK_RECEIPT_IN_LOGBATCH
                        Just (fun, args)  -> do
                            (runtime, res) <- withCalcRuntime do
                                evaluate $ force (foldl' (%%) fun args)
                            modify' (+ runtime)
                            pure (k, ROW (singleton res))

    batchLoop :: BatchNum -> Moment -> [Receipt] -> IO (BatchNum, Moment)
    batchLoop bn m []     = pure (bn, m)
    batchLoop bn m (x:xs) = do
        ((didCrash, eRes), eWork) <- flip runStateT 0 (recomputeEvals m x)
        let inp = TAB (mapFromList eRes)
        let arg = if didCrash then 0 %% inp else inp
        (iWork, new) <- withCalcRuntime $ evaluate $ force (m.val %% arg)
        let newWork = (m.work + eWork + iWork)
        batchLoop bn MOMENT{work=newWork, val=new} xs

    getEvalFunAt :: Fan -> RequestIdx -> Maybe (Fan, Vector Fan)
    getEvalFunAt noun (RequestIdx idx) = do
      let row = getCurrentReqNoun noun
      case row V.!? idx of
        Nothing -> Nothing
        Just val -> do
          case valToRequest ctx.cogName val of
            ReqEval er -> Just (er.func, er.args)
            _          -> Nothing

-- | Main entry point for starting running a Cog.
replayAndSpinCog :: Debug => Cushion -> CogContext -> ReplayFrom -> IO Cog
replayAndSpinCog cache ctx replayFrom = do
  let processName = "Cog: " <> encodeUtf8 (txt ctx.cogName)
      threadName  = encodeUtf8 "Main"

  withProcessName processName (wrapReplay threadName)
  where
    wrapReplay :: ByteString -> IO Cog
    wrapReplay threadName = do
      withThreadName threadName $ do
        setThreadSortIndex (-3)
        withTracingFlow "Replay" "cog" mempty [] $ do
          (batchNum, moment) <- performReplay cache ctx replayFrom

          debugText $ "REPLAY TIME: " <> tshow moment.work <> " ns"

          (cog, flows) <- buildCog threadName batchNum moment
          pure (flows, [], cog)

    buildCog
        :: ByteString
        -> BatchNum
        -> Moment
        -> IO (Cog, [Flow])
    buildCog threadName lastBatch moment = do
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

      cog <- mdo
        snapshotAsync <- asyncOnCurProcess $ withThreadName "Snapshot" $ do
          setThreadSortIndex (-1)
          handle (onErr "snapshot") $ snapshotFun cog

        loggerAsync <- asyncOnCurProcess $ withThreadName "Log" $ do
          setThreadSortIndex (-2)
          handle (onErr "log") $ logFun cog

        runnerAsync <- asyncOnCurProcess $ withThreadName threadName $ do
          handle (onErr "runner") $ runnerFun initialFlows cog runner

        let cog = COG{..}
        pure cog

      flows <- takeMVar initialFlows
      pure (cog, flows)

    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- | Synchronously shuts down a cog and
shutdownCog :: Cog -> IO ()
shutdownCog COG{..} = do
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
    -> CogContext
    -> RequestIdx
    -> Request
    -> STM ([Flow], LiveRequest, Maybe (STM OnCommitFlow))
buildLiveRequest causeFlow ctx idx = \case
    ReqEval EVAL_REQUEST{..} -> do -- timeoutMs func args -> do
        let req = EVAL_REQUEST{flow=causeFlow, timeoutMs, func, args,
                               cog=ctx.cogName}
        leRecord <- pleaseEvaluate ctx.eval req
        pure ([], LiveEval{leIdx=idx, leRecord}, Nothing)

    ReqCall cr -> do
        callSt <- STVAR <$> newTVar LIVE

        let lcSysCall = SYSCALL ctx.cogName cr.device cr.params callSt causeFlow
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

        -- TODO: What were you doing before you went to lunch? You were trying
        -- to thread through created startFlows in addition to the stepFlow.

        pure (startedFlows, LiveCall{lcIdx=idx, lcSysCall, lcCancel},
              logCallback)

    UNKNOWN _ -> do
        pure ([], LiveUnknown, Nothing)

cancelRequest :: LiveRequest -> STM ()
cancelRequest LiveEval{..} = leRecord.cancel.action
cancelRequest LiveCall{..} = lcCancel.action
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

receiveResponse :: (Fan, LiveRequest) -> STM (Maybe ResponseTuple)
receiveResponse = \case
    (_, LiveEval{..}) -> do
        getEvalOutcome leRecord >>= \case
            Nothing -> pure Nothing
            Just (outcome, flow) -> do
                let work = workToReplayEval outcome
                pure (Just RTUP{key=leIdx, resp=RespEval outcome, work, flow})

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
                pure (Just RTUP{key=lcIdx, resp, work=0, flow})

    (_, LiveUnknown) -> do
        pure Nothing

-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Cog Runner --------------------------------------------------------------

runnerFun :: Debug => MVar [Flow] -> Cog -> Runner -> IO ()
runnerFun initialFlows cog st =
    withCogHardwareInterface cog.ctx.cogName cog.ctx.hw do

        -- Process the initial syscall vector
        (flows, onCommit) <- atomically (parseRequests st)

        -- We got here via replay, so any synchronous requests are
        -- immediately safe to execute.
        for_ onCommit atomically
            -- TODO: do this all in one atomically?

        -- The code that calls us wants these.
        putMVar initialFlows flows

        -- Run the event loop
        forever cogTick

  where
    -- We've completed a unit of work. Time to tell the logger about
    -- the new state of the world.
    exportState :: Receipt -> [STM OnCommitFlow] -> STM ()
    exportState receipt onCommit = do
        moment <- readTVar st.vMoment
        when (length onCommit > 0) do
            writeTVar cog.logImmediately True
        writeTQueue cog.logReceiptQueue (receipt, onCommit)
        writeTVar cog.liveVar moment

    -- TODO: Optimize
    collectResponses :: IntMap (Maybe a) -> [(Int, a)]
    collectResponses imap =
        go [] (mapToList imap)
      where
        go !acc []                  = acc
        go !acc ((k,Just v) : kvs)  = go ((k,v):acc) kvs
        go !acc ((_,Nothing) : kvs) = go acc         kvs

    -- Collects all responses that are ready.
    takeReturns :: IO [(Int, ResponseTuple)]
    takeReturns = do
        withAlwaysTrace "WaitForResponse" "cog" do
            -- This is in a separate atomically block because it doesn't
            -- change and we want to minimize contention.
            sysCalls <- atomically (readTVar st.vRequests)

            atomically do
                returns <- fmap collectResponses
                         $ traverse receiveResponse
                         $ sysCalls

                when (null returns) retry

                pure returns

    cogTick :: IO ()
    cogTick = do
        -- traceM "TICK"
        --  <- atomically (readTVar st.vRequests)
        -- traceM (ppShow vec)
        valTuples <- takeReturns

        let endFlows  = (.flow) . snd <$> valTuples

        let traceArg = M.singleton "syscall indicies"
                     $ Right
                     $ tshow
                     $ fmap fst valTuples

        withTracingFlow "Response" "cog" traceArg endFlows do

            runResponse st valTuples False >>= \case
                Nothing -> do
                    pure ([], [], ())

                Just (flows, onCommit, receipt) -> do
                    atomically (exportState receipt onCommit)
                    pure (flows, [], ())

runResponse
    :: Debug
    => Runner
    -> [(Int, ResponseTuple)]
    -> Bool
    -> IO (Maybe ([Flow], [STM OnCommitFlow], Receipt))
runResponse st rets didCrash = do
  let arg = TAB
          $ mapFromList
          $ flip fmap rets
          $ \(k,v) -> (NAT (fromIntegral k), responseToVal v.resp)

  moment <- atomically (readTVar st.vMoment)
  (runtimeUs, result) <- do
      withAlwaysTrace "Eval" "cog" do
          evalWithTimeout thirtySecondsInMicroseconds moment.val
              (if didCrash then (0 %% arg) else arg)
  case result of
    TIMEOUT -> recordInstantEvent "Main Timeout"   "cog" mempty >> onCrash
    CRASH{} -> recordInstantEvent "Main Exception" "cog" mempty >> onCrash
    OKAY _ result -> do
      withAlwaysTrace "Tick" "cog" do
        -- Update the runner state, making sure to delete the consumed
        -- LiveRequest without formally canceling it because it already
        -- completed.
        atomically do
            modifyTVar' st.vMoment   \m -> MOMENT result (m.work + runtimeUs)
            modifyTVar' st.vRequests \kals -> foldl' delReq kals (fst<$>rets)

        (startedFlows, onPersists) <- atomically (parseRequests st)

        pure $ Just ( startedFlows
                    , onPersists
                    , responseToReceipt didCrash rets
                    )
 where
   onCrash =
       if didCrash
       then throwIO COG_DOUBLE_CRASH
       else runResponse st rets True

   delReq :: SysCalls -> Int -> SysCalls
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


-- | Given a noun in a runner, update the `requests`
parseRequests :: Debug => Runner -> STM ([Flow], [STM OnCommitFlow])
parseRequests runner = do
    moment   <- readTVar runner.vMoment
    syscalls <- readTVar runner.vRequests

    let init = PRS{syscalls, flows=[], onPersist=[]}

    st <- flip execStateT init do
              let expected = syscalls
              let actual   = getCurrentReqNoun moment.val

              for_ (mapToList expected) \(i,(v,_)) ->
                  case actual V.!? i of
                      Nothing           -> cancelReq i
                      Just w | keep v w -> pure ()
                      Just w            -> cancelReq i >> createReq i w

              for_ (zip [0..] $ toList actual) \(i,v) ->
                  unless (member i expected) do
                      createReq i v

    writeTVar runner.vRequests $! st.syscalls

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
        UNKNOWN{}  -> "UNKNOWN"

    flowCategory :: Request -> ByteString
    flowCategory = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          syscallCategory runner.ctx.hw rc.device rc.params)
        UNKNOWN{}  -> "UNKNOWN"

    createReq :: Int -> Fan -> StateT ParseRequestsState STM ()
    createReq i v = do
        -- traceM ("createReq:" <> show i <> " " <> show v)
        let request = valToRequest runner.ctx.cogName v
            reqData = reqDebugSummary request
            reqName = "Cog " <> encodeUtf8 runner.ctx.cogName.txt <> " idx "
                   <> encodeUtf8 (tshow i) <> ": " <> reqData
            catData = flowCategory request
        f <- lift $ allocateRequestFlow reqName (decodeUtf8 catData)

        let rIdx = RequestIdx i

        (startedFlows, live, onPersist) <- lift $
          buildLiveRequest f runner.ctx rIdx request

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
logFun :: Debug => Cog -> IO ()
logFun cog =
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
          True  -> atomically $ writeTVar cog.shutdownSnapshot True
          False -> loop

    -- Either reads a shutdown command or the next batch of work to log.
    getNextBatch :: STM LogNext
    getNextBatch = do
        shutdown             <- readTVar cog.shutdownLogger
        live                 <- readTVar cog.liveVar
        (lastBatchNum, writ) <- readTVar cog.writVar
        forcedLog            <- readTVar cog.logImmediately

        let nextLogWork = writ.work + logbatchWorkIntervalInNs
        let shouldLog   = shutdown || forcedLog || (live.work > nextLogWork)

        unless shouldLog retry

        when forcedLog (writeTVar cog.logImmediately False)

        receipts <- flushTQueue cog.logReceiptQueue
        moment   <- readTVar cog.liveVar

        pure LOG_NEXT{batchNum=(lastBatchNum + 1), ..}

    doBatch :: LogNext -> IO ([Flow], [Flow])
    doBatch next = do
        let (receipts, onCommit) = unzip next.receipts

        now <- getNanoTime

        writeLogBatch cog.ctx.lmdb cog.ctx.cogName $
            LogBatch
                { batchNum  = next.batchNum
                , writeTime = now
                , executed  = receipts
                }

        atomically $ writeTVar cog.writVar (next.batchNum, next.moment)

        -- Perform the associated STM actions that were supposed to be
        -- run once committed.  This is used for synchronous syscalls
        -- which need to block until we have committed to the state that
        -- triggered them.
        (starts, steps) <- unzipOnCommitFlows <$>
            traverse atomically (join onCommit)
        pure (starts, steps)


-- The Snapshotting Routine ----------------------------------------------------

{-
    The is the per-cog snapshotting logic that runs in `snapshotAsync`.

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
snapshotFun :: Debug => Cog -> IO ()
snapshotFun cog =
    when cog.ctx.enableSnaps do
        loop 0
  where
    loop lastSnapWork = do
        let nextSnapWork = lastSnapWork + snapshotWorkIntervalInNs

        (shutdown, bn, moment) <-
            withAlwaysTrace "Block" "log" $
            atomically do
                shutdown           <- readTVar cog.shutdownSnapshot
                (logBN, logMoment) <- readTVar cog.writVar

                let shouldSnap = shutdown || logMoment.work > nextSnapWork

                unless shouldSnap retry

                pure (shutdown, logBN, logMoment)

        withAlwaysTrace "Snapshot" "log" do
            writeCogSnapshot cog.ctx.lmdb cog.ctx.cogName bn moment.val

        unless shutdown do
            loop moment.work
