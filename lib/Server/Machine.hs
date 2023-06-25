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
import System.Random         (randomIO)
import System.Random.Shuffle (shuffleM)

import Fan (Fan(..), PrimopCrash(..), getRow, (%%))

import qualified Fan

import Fan.Convert
import Fan.Prof
import Optics                (set)
import Server.Common
import Server.Convert        ()
import Server.Debug
import Server.Evaluator
import Server.Hardware.Types
import Server.LmdbStore
import Server.Time
import Server.Types.Logging

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V
-- import qualified Fan.Prof    as Prof

--------------------------------------------------------------------------------

data Moment = MOMENT {
  val  :: Map CogId CogState,
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

data SendOutcome
    = SendCrash
    | SendOK
  deriving (Show)

data Response
    = RespEval EvalOutcome
    | RespCall Fan SysCall
    | RespWhat (Set Nat)
    | RespRecv PendingSendRequest
    | RespSend SendOutcome
    | RespSpin CogId Fan
    | RespReap CogId (Maybe CogState)
    | RespStop CogId (Maybe CogState)
    | RespWho CogId
  deriving (Show)

responseToVal :: Response -> Fan
responseToVal (RespCall f _) = f
responseToVal (RespWhat w) = toNoun w
responseToVal (RespRecv PENDING_SEND{..}) =
  ROW $ fromList [toNoun sender, ROW msgParams]
responseToVal (RespSend SendOK) = NAT 0
responseToVal (RespSend SendCrash) = NAT 1
responseToVal (RespSpin (COG_ID id) _) = fromIntegral id
responseToVal (RespReap _ f) = toNoun f
responseToVal (RespStop _ f) = toNoun f
responseToVal (RespWho (COG_ID id)) = fromIntegral id
responseToVal (RespEval e)   =
    ROW case e of
        TIMEOUT   -> mempty              -- []
        OKAY _ r  -> singleton r         -- [f]
        CRASH n e -> fromList [NAT n, e] -- [n e]

responseToReceiptItem :: (Int, ResponseTuple) -> (Int, ReceiptItem)
responseToReceiptItem (idx, tup) = case tup.resp of
    RespEval OKAY{}           -> (idx, ReceiptEvalOK)
    RespRecv PENDING_SEND{..} -> (idx, ReceiptRecv{..})
    RespSpin cog _            -> (idx, ReceiptSpun cog)
    RespReap cog _            -> (idx, ReceiptReap cog)
    RespStop cog _            -> (idx, ReceiptStop cog)
    resp                      -> (idx, ReceiptVal (responseToVal resp))

makeOKReceipt :: CogId -> [(Int, ResponseTuple)] -> Receipt
makeOKReceipt cogId =
    RECEIPT_OK cogId . mapFromList . fmap responseToReceiptItem

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

data SendRequest = SNDR
    { cogDst  :: CogId
    , channel :: Word64
    , params  :: Vector Fan
    }
  deriving (Show)

-- TODO: We in theory have send done, but recv is the hard part since the recv
-- also must respond to the recv.

data Request
    = ReqEval EvalRequest
    | ReqCall CallRequest
    | ReqWhat (Set Nat)
    | ReqSend SendRequest
    | ReqRecv Word64
    | ReqSpin SpinRequest
    | ReqReap CogId
    | ReqStop CogId
    | ReqWho
    | UNKNOWN Fan
  deriving (Show)

{-
  [%eval timeout/@ fun/fan arg/Fan ...]
  [%cog %spin fun/Fan]
  [%cog %send dst/Nat param/Fan]
  [%cog %recv]
  [$call synced/? param/Fan ...]
-}
valToRequest :: MachineName -> CogId -> Fan -> Request
valToRequest machine cogId top = fromMaybe (UNKNOWN top) do
    row <- getRow top
    tag <- fromNoun @DeviceName (row!0)

    if tag == "eval" then do
        nat <- fromNoun @Nat (row!1)
        let timeoutSecs = nat
        let timeoutMs   = timeoutSecs * 1000
        guard (length row >= 4)
        let func = row!2
        let args = V.drop 3 row
        let flow = FlowDisabled -- TODO: What?
        let er = EVAL_REQUEST{func,args,machine,cogId,flow,timeoutMs}
        pure (ReqEval er)
        -- e (ReqEval $ error "_" timeoutSecs (row!2) (V.drop 3 row))
    else if tag == "what" then do
        what <- fromNoun @(Set Nat) (row!1)
        pure $ ReqWhat what
    else if tag == "cog" then do
        nat <- fromNoun @Nat (row!1)
        case nat of
            "spin" | length row == 3 -> pure (ReqSpin $ SR $ row!2)
            "reap" | length row == 3 -> ReqReap <$> fromNoun @CogId (row!2)
            "stop" | length row == 3 -> ReqStop <$> fromNoun @CogId (row!2)
            "send" | length row >= 4 -> do
                dst <- fromNoun @CogId (row!2)
                channel <- fromNoun (row!3)
                pure (ReqSend (SNDR dst channel (V.drop 4 row)))
            "recv" | length row == 3 -> ReqRecv <$> fromNoun (row!2)
            "who"  | length row == 2 -> pure ReqWho
            _ -> Nothing
    else do
        _   <- guard (length row >= 3)
        nat <- fromNoun @Nat (row!1)
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
type MachineSysCalls = Map CogId CogSysCalls

-- | This is a record that a cog has an active SysCall requesting an
-- IPC-send to another cog.  See `CogSendPool`
data PendingSendRequest = PENDING_SEND
    { sender    :: CogId
    , reqIdx    :: RequestIdx
    , msgParams :: Vector Fan
    }
  deriving (Show)

-- This is a collection of all {PendingSendRequest}s within all the cogs of a
-- machine on all channels.  This is part of the top-level Machine STM state,
-- because these IPC interactions need happen transactionally.
type CogSendPool = Map CogId (TVar ChannelSendPool)

type ChannelSendPool = Map Word64 (TVar (Pool PendingSendRequest))

channelPoolRegister :: Word64 -> TVar ChannelSendPool -> PendingSendRequest
                    -> STM Int
channelPoolRegister channel vChannels psr = do
    channels <- readTVar vChannels
    pool <- case M.lookup channel channels of
        Just pool -> pure pool
        Nothing -> do
            pool <- newTVar emptyPool
            modifyTVar' vChannels $ insertMap channel pool
            pure pool
    poolRegister pool psr

-- Given a channel number, takes an item from that pool, cleaning up empty
-- channel pools.
--
-- Unlike `readPool`, this never retries and returns a Maybe instead because
-- retrying during `receiveResponse` can cause more widespread blockage.
channelPoolTake :: Word64 -> TVar ChannelSendPool
                -> STM (Maybe PendingSendRequest)
channelPoolTake channel vChannels = do
    channels <- readTVar vChannels
    case lookup channel channels of
        Nothing -> pure Nothing
        Just vPool -> do
            pool <- readTVar vPool
            case IM.minView pool.tab of
                Nothing -> error "Pool didn't get cleaned when empty?"
                Just (x, xs) -> do
                    case null xs of
                        True  -> modifyTVar' vChannels $ M.delete channel
                        False -> writeTVar vPool (set #tab xs $ pool)
                    pure $ Just x

channelPoolUnregister :: Word64 -> TVar ChannelSendPool -> Int -> STM ()
channelPoolUnregister channel vChannels poolId =
  do
    channels <- readTVar vChannels
    case lookup channel channels of
        Nothing -> pure ()
        Just vPool -> do
            empty <- stateTVar vPool $ \pool ->
                let newPool = over #tab (deleteMap poolId) pool
                in (null newPool.tab, newPool)

            when empty $ do
                modifyTVar' vChannels $ deleteMap channel

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { ctx       :: MachineContext
    , vMoment   :: TVar Moment          -- ^ Current value + cumulative CPU time
    , vRequests :: TVar MachineSysCalls -- ^ Current requests table
    , vSends    :: TVar CogSendPool
    }

-- -----------------------------------------------------------------------

-- An effect that happens atomically after a Response is processed by a cog and
-- didn't crash.
data CogReplayEffect
    = CSpin
      { reCogId :: CogId
      , reFun   :: Fan
      }
    | CStop
      { reCogId :: CogId }
    deriving (Show)

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
  | LiveWhat {
    lwhIdx     :: RequestIdx,
    lwhCog     :: Set Nat,
    lwhRuntime :: Set Nat
    }
  | LiveSend {
    lsndChannel  :: Word64,
    lsndPoolId   :: Int,
    lsndChannels :: TVar ChannelSendPool
    }
  | LiveRecv {
    lrIdx      :: RequestIdx,
    lrChannel  :: Word64,
    lrChannels :: TVar ChannelSendPool
    }
  | LiveSpin {
    lsIdx :: RequestIdx,
    lsFun :: Fan
    }
  | LiveReap {
    lrIdx   :: RequestIdx,
    lrCogId :: CogId
    }
  | LiveStop {
    lstIdx   :: RequestIdx,
    lstCogId :: CogId
    }
  | LiveWho {
    lwIdx   :: RequestIdx,
    lwCogId :: CogId
    }
  | LiveUnknown

instance Show LiveRequest where
    show = \case
        LiveEval{}  -> "EVAL"
        LiveCall{}  -> "CALL"
        LiveWhat{}  -> "WHAT"
        LiveSend{}  -> "SEND"
        LiveRecv{}  -> "RECV"
        LiveSpin{}  -> "SPIN"
        LiveReap{}  -> "REAP"
        LiveStop{}  -> "STOP"
        LiveWho{}   -> "WHO"
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

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (a, b, _) = (a, b)

third :: (a, b, c) -> c
third (_, _, c) = c

type ReconstructedEvals = [(Fan, Fan, Maybe CogReplayEffect)]

recomputeEvals
    :: MachineContext
    -> Moment
    -> CogId
    -> IntMap ReceiptItem
    -> StateT NanoTime IO ReconstructedEvals
recomputeEvals ctx m cogId tab =
    for (mapToList tab) \(idx, rVal) -> do
        let k = toNoun (fromIntegral idx :: Word)
        case rVal of
            ReceiptVal val -> pure (k, val, Nothing)
            ReceiptEvalOK  -> do
                -- We performed an eval which succeeded the
                -- first time.
                case getEvalFunAt m cogId (RequestIdx idx) of
                    Nothing ->
                        throwIO INVALID_OK_RECEIPT_IN_LOGBATCH
                    Just (fun, args)  -> do
                        (runtime, res) <- withCalcRuntime do
                            evaluate $ force (foldl' (%%) fun args)
                        modify' (+ runtime)
                        pure (k, ROW (singleton res), Nothing)
            ReceiptSpun newCogId -> do
                case getSpinFunAt m cogId (RequestIdx idx) of
                    Nothing ->
                        throwIO $ INVALID_SPUN_RECEIPT_IN_LOGBATCH newCogId
                    Just fun -> do
                        let ef = CSpin newCogId fun
                        pure (k, NAT $ fromIntegral newCogId.int, Just ef)
            ReceiptRecv{..} -> do
                case getSendFunAt m sender cogId reqIdx of
                    Nothing ->
                        throwIO INVALID_RECV_RECEIPT_IN_LOGBATCH
                    Just val ->
                      let out = ROW $ fromList [toNoun sender, val]
                      in pure (k, out, Nothing)
            ReceiptReap{..} -> do
                case M.lookup cogNum m.val of
                    Nothing ->
                        throwIO INVALID_REAP_RECEIPT_IN_LOGBATCH
                    Just CG_SPINNING{} ->
                        throwIO INVALID_REAP_RECEIPT_IN_LOGBATCH
                    Just val -> do
                        let ef = CStop cogNum
                        pure (k, (NAT 0) %% toNoun val, Just ef)
            ReceiptStop{..} -> do
                case M.lookup cogNum m.val of
                    Nothing ->
                        pure (k, NAT 0, Nothing)
                    Just val -> do
                        let ef = CStop cogNum
                        pure (k, (NAT 0) %% toNoun val, Just ef)
  where
    getEvalFunAt :: Moment -> CogId -> RequestIdx -> Maybe (Fan, Vector Fan)
    getEvalFunAt m cogId idx = withRequestAt m cogId idx $ \case
        ReqEval er -> Just (er.func, er.args)
        _          -> Nothing


    getSpinFunAt :: Moment -> CogId -> RequestIdx -> Maybe Fan
    getSpinFunAt m cogId idx = withRequestAt m cogId idx $ \case
        ReqSpin (SR fun) -> Just fun
        _                -> Nothing

    getSendFunAt :: Moment -> CogId -> CogId -> RequestIdx -> Maybe Fan
    getSendFunAt m sender receiver idx = withRequestAt m sender idx $ \case
        ReqSend (SNDR cogDst _channel params)
            | cogDst == receiver     -> Just $ ROW params
            | otherwise              -> Nothing
        _                            -> Nothing

    withRequestAt :: Moment -> CogId -> RequestIdx -> (Request -> Maybe a)
                  -> Maybe a
    withRequestAt m cogId (RequestIdx idx) fun = do
        case lookup cogId m.val of
            Nothing -> Nothing
            Just CG_CRASHED{} -> Nothing
            Just CG_TIMEOUT{} -> Nothing
            Just (CG_SPINNING cog) -> do
                let row = getCurrentReqNoun cog
                case row V.!? idx of
                    Nothing  -> Nothing
                    Just val -> fun $ valToRequest ctx.machineName cogId val

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
    let mkInitial (a, b) = (b, MOMENT a 0)
    loadLogBatches ctx.machineName replayFrom mkInitial doBatch ctx.lmdb cache
  where
    doBatch :: (BatchNum, Moment) -> LogBatch -> IO (BatchNum, Moment)
    doBatch (_, moment) LogBatch{batchNum,executed} =
        loop batchNum moment executed

    loop :: BatchNum -> Moment -> [Receipt] -> IO (BatchNum, Moment)
    loop bn m []     = pure (bn, m)
    loop bn m (x:xs) =
        let mybCogFun = case M.lookup x.cogNum m.val of
                Nothing -> Nothing
                Just x  -> cogSpinningFun x
        in case x of
            RECEIPT_TIME_OUT{..} -> do
                -- Evaluating this bundle of responses timed out the first time
                -- we ran it, so just don't run it and just recreate the
                -- timed-out state from the recorded timeout amount.
                case mybCogFun of
                    Nothing -> throwIO INVALID_TIMEOUT_IN_LOGBATCH
                    Just fun -> do
                        let cog = CG_TIMEOUT timeoutAmount fun
                        pure (bn, m { val = M.insert x.cogNum cog m.val})

            RECEIPT_CRASHED{..} -> do
                -- While it's deterministic whether a plunder computation
                -- crashes or not, its crash value is not deterministic so we
                -- must reconstitute the crash from the recorded result.
                case mybCogFun of
                    Nothing -> throwIO INVALID_CRASHED_IN_LOGBATCH
                    Just final -> do
                        let cog = CG_CRASHED{op,arg,final}
                        pure (bn, m { val = M.insert x.cogNum cog m.val})

            RECEIPT_OK{..} -> do
                -- The original run succeeded, rebuild the value from the
                -- receipt items.
                (eRes, eWork) <- runStateT (recomputeEvals ctx m cogNum inputs) 0

                let arg = TAb $ mapFromList $ map tripleToPair eRes

                let runEffect m = \case
                        Just CSpin{..} -> pure $ m {
                            val = M.insert reCogId (CG_SPINNING reFun) m.val }
                        Just CStop{..} -> pure $ m {
                            val = M.delete reCogId m.val }
                        Nothing        -> pure m

                m <- foldlM runEffect m (map third eRes)

                fun <- case mybCogFun of
                    Nothing  -> throwIO INVALID_COGID_IN_LOGBATCH
                    Just fun -> pure fun

                (iWork, outcome) <- evalCheckingCrash fun arg

                newVal <- case outcome of
                    TIMEOUT ->
                        error "performReplay: impossible timeout on replay"
                    CRASH o e -> pure $ CG_CRASHED o e fun
                    OKAY _ result -> pure $ CG_SPINNING result

                let newMoment = MOMENT
                        { work = m.work + eWork + iWork
                        , val = insertMap x.cogNum newVal m.val
                        }

                loop bn newMoment xs

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

          channels <- forM (keys moment.val) $ \k -> do
              channelPools <- newTVar mempty
              pure (k, channelPools)
          vSends     <- newTVar $ mapFromList channels
          pure RUNNER{vMoment, ctx, vRequests, vSends}

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
    -> Runner
    -> MachineContext
    -> CogId
    -> RequestIdx
    -> Request
    -> STM ([Flow], LiveRequest, Maybe (STM OnCommitFlow))
buildLiveRequest causeFlow runner ctx cogId reqIdx = \case
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

    ReqWhat what -> do
        -- NOTE: If we ever make the hardware set runtime dynamic, the
        -- DeviceTable's data should be a `TVar (Map ...)` to let us detect
        -- changes to the variable.
        let runtime = S.fromList $ keys $ table ctx.hw
        pure ([], LiveWhat{lwhIdx=reqIdx,lwhCog=what,lwhRuntime=runtime},
              Nothing)

    ReqRecv channel -> do
        sends <- readTVar runner.vSends
        channels <- case M.lookup cogId sends of
            Nothing   -> error $ "Listening on a nonexistent cog " <> show cogId
            Just pool -> pure pool
        -- pool <- case M.lookup
        pure ([], LiveRecv{lrIdx=reqIdx,lrChannel=channel,lrChannels=channels},
              Nothing)

    ReqSend SNDR{cogDst,channel,params} -> do
        sends <- readTVar runner.vSends
        case M.lookup cogDst sends of
            Nothing -> error $ "Bad cogDst " <> show cogDst
            Just vChannels -> do
                poolId <-
                    channelPoolRegister channel
                                        vChannels
                                        (PENDING_SEND cogId reqIdx params)
                pure ([], LiveSend{lsndChannel=channel,
                                   lsndPoolId=poolId,
                                   lsndChannels=vChannels}, Nothing)

    ReqSpin (SR func) -> do
        pure ([], LiveSpin{lsIdx=reqIdx, lsFun=func}, Nothing)

    ReqReap cogid -> do
        pure ([], LiveReap{lrIdx=reqIdx, lrCogId=cogid}, Nothing)

    ReqStop cogid -> do
        pure ([], LiveStop{lstIdx=reqIdx, lstCogId=cogid}, Nothing)

    ReqWho -> do
        pure ([], LiveWho{lwIdx=reqIdx, lwCogId=cogId}, Nothing)

    UNKNOWN _ -> do
        pure ([], LiveUnknown, Nothing)

cancelRequest :: LiveRequest -> STM ()
cancelRequest LiveEval{..} = leRecord.cancel.action
cancelRequest LiveCall{..} = lcCancel.action
cancelRequest LiveWhat{}   = pure ()
cancelRequest LiveRecv{}   = pure ()
cancelRequest LiveSend{..} =
    channelPoolUnregister lsndChannel lsndChannels lsndPoolId
cancelRequest LiveSpin{}   = pure () -- Not asynchronous
cancelRequest LiveReap{}   = pure ()
cancelRequest LiveStop{}   = pure ()
cancelRequest LiveWho{}    = pure ()
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
  :: Runner
  -> (Fan, LiveRequest)
  -> STM (Maybe ResponseTuple)
receiveResponse st = \case
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

    (_, LiveWhat{..}) -> do
        guard (lwhCog /= lwhRuntime)
        let resp = RespWhat lwhRuntime
        pure (Just RTUP{key=lwhIdx,resp,work=0,flow=FlowDisabled})

    (_, LiveSend{}) ->
        -- Sends are never responded to normally, they're responded manually as
        -- a side effect of a recv so that we maintain atomicity of the
        -- send/recv pair in the log.
        retry

    (_, LiveRecv{..}) -> do
        channelPoolTake lrChannel lrChannels >>= \case
            Nothing -> pure Nothing
            Just send -> do
                pure (Just RTUP{key=lrIdx,
                                resp=RespRecv send,
                                work=0,
                                -- TODO: Hook up send flows here.
                                flow=FlowDisabled})

    (_, LiveSpin{..}) ->
      do
        cogid <- getRandomNonConflictingId =<< readTVar st.vMoment
        -- TODO: How should the flows be represented when spinning a new cog!?
        pure (Just RTUP{key=lsIdx, resp=RespSpin cogid lsFun, work=0,
                        flow=FlowDisabled})
      where
        getRandomNonConflictingId :: Moment -> STM CogId
        getRandomNonConflictingId moment = do
          r :: Word64 <- unsafeIOToSTM $ randomIO
          case lookup (COG_ID r) moment.val of
            Nothing -> pure $ COG_ID r
            Just _  -> getRandomNonConflictingId moment

    (_, LiveReap{..}) -> do
        do
          myb <- (lookup lrCogId . (.val)) <$> readTVar st.vMoment
          case myb of
            Nothing -> do
              pure (Just RTUP{key=lrIdx, resp=RespReap lrCogId Nothing,
                              work=0, flow=FlowDisabled})
            Just cogval -> case cogval of
              CG_SPINNING{} -> pure Nothing
              _             -> do
                performStoplike RespReap lrIdx lrCogId cogval

    (_, LiveStop{..}) -> do
        do
          myb <- (lookup lstCogId . (.val)) <$> readTVar st.vMoment
          case myb of
            Nothing ->
              pure (Just RTUP{key=lstIdx, resp=RespStop lstCogId Nothing,
                              work=0, flow=FlowDisabled})
            Just cogval -> performStoplike RespStop lstIdx lstCogId cogval

    (_, LiveWho{..}) -> do
        pure (Just RTUP{key=lwIdx, resp=RespWho lwCogId, work=0,
                        flow=FlowDisabled})

    (_, LiveUnknown) -> do
        pure Nothing

  where
    -- Common implementation of %stop and %reap, building a response while also
    -- removing the current cog's state and canceling all open requests.
    performStoplike :: (CogId -> Maybe CogState -> Response)
                    -> RequestIdx
                    -> CogId
                    -> CogState
                    -> STM (Maybe ResponseTuple)
    performStoplike mkResponse reqIdx cogId cogVal = do
        modifyTVar' st.vSends $ M.delete cogId
        modifyTVar' st.vMoment
                    \m -> m { val=deleteMap cogId m.val }

        reqs <- stateTVar st.vRequests getAndRemoveReqs
        mapM_ cancelRequest reqs

        pure (Just RTUP{key=reqIdx, resp=mkResponse cogId (Just cogVal),
                        work=0, flow=FlowDisabled})
      where
        getAndRemoveReqs s =
            ( getLiveReqs $ fromMaybe mempty $ lookup cogId s
            , deleteMap cogId s
            )
        getLiveReqs csc = map (\(_,(_,lr)) -> lr) $ mapToList csc


-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Cog Runner --------------------------------------------------------------

runnerFun :: Debug => MVar [Flow] -> Machine -> ByteString -> Runner -> IO ()
runnerFun initialFlows machine processName st =
    bracket_ registerCogsWithHardware stopCogsWithHardware $ do
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
    registerCogsWithHardware :: IO ()
    registerCogsWithHardware = do
      m <- readTVarIO st.vMoment
      let k :: [CogId] = keys m.val
      for_ k $ \cogid ->
        for_ machine.ctx.hw.table $ \d -> d.spin machine.ctx.machineName cogid

    stopCogsWithHardware :: IO ()
    stopCogsWithHardware = do
      m <- readTVarIO st.vMoment
      for_ (keys m.val) $ \cogid ->
        for_ machine.ctx.hw.table $ \d -> d.stop machine.ctx.machineName cogid

    -- We've completed a unit of work. Time to tell the logger about
    -- the new state of the world.
    exportState :: (Receipt, [STM OnCommitFlow]) -> STM ()
    exportState (receipt, onCommit) = do
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

    mkCogResponses :: [(CogId, CogSysCalls)]
                   -> [STM (CogId, [(Int, ResponseTuple)])]
    mkCogResponses = map build
      where
        build :: (CogId, CogSysCalls) -> STM (CogId, [(Int, ResponseTuple)])
        build (k, sysCalls) = do
           returns <- fmap collectResponses
                    $ traverse (receiveResponse st)
                    $ sysCalls

           when (null returns) retry

           pure (k, returns)

    -- Collects all responses that are ready for one cog.
    takeReturns :: IO (CogId, [(Int, ResponseTuple)])
    takeReturns = do
        withAlwaysTrace "WaitForResponse" "cog" do
            -- This is in a separate atomically block because it doesn't
            -- change and we want to minimize contention.
            machineSysCalls <- mapToList <$> atomically (readTVar st.vRequests)
            reordered <- shuffleM machineSysCalls
            -- debugText $ "takeReturns: " <> tshow reordered
            atomically $ asum $ mkCogResponses reordered

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
        results <- cogTick (cogId, valTuples)
        atomically $ do
            mapM_ exportState results
            readTVar st.vMoment >>= writeTVar machine.liveVar

    --
    cogTick
      :: (CogId, [(Int, ResponseTuple)])
      -> IO [(Receipt, [STM OnCommitFlow])]
    cogTick (cogId, valTuples) =
      withProcessName processName $
        withThreadName ("Cog: " <> (encodeUtf8 $ tshow cogId)) $ do
          let endFlows = (.flow) . snd <$> valTuples
          let traceArg = M.singleton "syscall indicies"
                       $ Right
                       $ tshow
                       $ fmap fst valTuples

          withTracingFlow "Response" "cog" traceArg endFlows $ do
            (flows, receipts) <- runResponse st cogId valTuples
            pure (flows, [], receipts)

{-
    Given a set of responses to syscalls in a cogs SysCall table, create
    a new event value and pass that into the cog, to get the new cog state.

    Side Effects:

    -   The PLAN value for the cog is replaced.

    -   The cog's requests row is updated to reflect the new set of
        requests.

    -   If we received an IPC message, the corresponding COG_SEND request
        is also processed.

    -   If we spawned a cog or stopped a cog, we inform every hardware
        device that this happened.

    Results:

    -   The set of profiling events triggered by this change.

    -   A list of event-log receipts (and the corresponding actions to
        be triggered when those receipts have been committed to disk.
        (This is for disk-synchronized syscalls).
-}
runResponse
    :: Debug
    => Runner
    -> CogId
    -> [(Int, ResponseTuple)]
    -> IO ([Flow], [(Receipt, [STM OnCommitFlow])])
runResponse st cogNum rets = do
  let arg = TAb
          $ mapFromList
          $ flip fmap rets
          $ \(k,v) -> (NAT (fromIntegral k), responseToVal v.resp)
  moment <- atomically (readTVar st.vMoment)
  let fun = fromMaybe (error "Trying to run a stopped cog")
          $ cogSpinningFun
          $ fromMaybe (error "Invalid cogid")
          $ lookup cogNum moment.val
  (runtimeUs, result) <- do
      withAlwaysTrace "Eval" "cog" do
          evalWithTimeout thirtySecondsInMicroseconds fun arg

  let resultReceipt = case result of
        TIMEOUT      -> RECEIPT_TIME_OUT{cogNum,timeoutAmount=runtimeUs}
        CRASH op arg -> RECEIPT_CRASHED{..}
        OKAY{}       -> makeOKReceipt cogNum rets

  let newState = case result of
        OKAY _ resultFan -> CG_SPINNING resultFan
        CRASH op arg     -> CG_CRASHED op arg fun
        TIMEOUT          -> CG_TIMEOUT runtimeUs fun

  withAlwaysTrace "Tick" "cog" do
      let responses = ((.resp) . snd) <$> rets

      (sideEffects, startedFlows, onPersists) <- atomically do
          modifyTVar' st.vMoment \m ->
              MOMENT (insertMap cogNum newState m.val) (m.work + runtimeUs)

          sideEffects <- case resultReceipt of
              RECEIPT_OK{..} -> do
                -- Delete the consumed `LiveRequest`s without formally
                -- canceling it because it completed.
                modifyTVar' st.vRequests \tab ->
                    adjustMap (\kals -> foldl' delReq kals (fst<$>rets))
                              cogNum tab
                mapM performSpinEffects responses
              _ -> do
                -- If we crashed, do nothing. We'll formally cancel all
                -- requests when we reparse in the next step, and we have no
                -- valid responses to process side effects from.
                pure mempty

          (startedFlows, onPersists) <- parseRequests st cogNum

          pure (sideEffects, startedFlows, onPersists)

      afterResults <- mapM (performAfterEffect resultReceipt) responses

      let (sideEffectFlows, sideEffectPersists) = concatUnzip sideEffects
          (afterFlows, afterReceipts) = concatUnzip afterResults
          receiptPairs = [ ( resultReceipt
                           , onPersists ++ sideEffectPersists
                           )
                         ]
                      ++ afterReceipts

      pure ( startedFlows ++ afterFlows ++ sideEffectFlows
           , receiptPairs )

  where
    delReq :: CogSysCalls -> Int -> CogSysCalls
    delReq acc k = IM.delete k acc

    performSpinEffects :: Response
                       -> STM ([Flow], [STM OnCommitFlow])
    performSpinEffects (RespSpin newCogId fun) = do
        channelPools <- newTVar mempty
        modifyTVar' st.vSends \sends ->
            M.insert newCogId channelPools sends
        modifyTVar' st.vMoment \m ->
            m { val=insertMap newCogId (CG_SPINNING fun) m.val }
        parseRequests st newCogId

    performSpinEffects _ = pure mempty

    -- Performed outside the Big Atomically Block
    performAfterEffect :: Receipt
                       -> Response
                       -> IO ([Flow], [(Receipt, [STM OnCommitFlow])])
    performAfterEffect _ (RespSpin newCogId _) = do
        -- If we just spun up another cog, tell all the hardware
        -- devices that it exists now.
        for_ st.ctx.hw.table $ \d ->
            d.spin st.ctx.machineName newCogId
        pure ([], [])

    performAfterEffect _ (RespStop dedCogId _) = do
        -- If we just stopped another cog, we need to to tell
        -- all the hardware devices that it no longer exists.
        for_ st.ctx.hw.table $ \d ->
            d.stop st.ctx.machineName dedCogId
        pure ([], [])

    performAfterEffect result (RespRecv PENDING_SEND{..}) = do
        -- When an IPC message is received, the corresponding
        -- send syscall on the sender cog must also be processed
        -- within the same transaction.
        --
        -- We do this recursively invoking runResponse for that
        -- syscall as well.
        --
        -- We are sure that the `reqIdx` still corresponds to
        -- an active COG_SEND syscall because of reasons.
        let outcome = case result of
              RECEIPT_OK{} -> SendOK
              _            -> SendCrash

        -- TODO: Enable profiling flows; how do we connect
        -- the recv completing to this?
        let rtup = RTUP { key  = reqIdx
                        , resp = RespSend outcome
                        , work = 0
                        , flow = FlowDisabled
                        }
        runResponse st sender [(reqIdx.int, rtup)]

    performAfterEffect _ _ = pure ([], [])


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
  actions <- forM cogs $ parseRequests runner
  pure $ concatUnzip actions

-- | Given a noun in a runner, update the `requests`
parseRequests :: Debug => Runner -> CogId -> STM ([Flow], [STM OnCommitFlow])
parseRequests runner cogId = do
    cogState <- (fromMaybe (error "no cogid m") . lookup cogId . (.val)) <$>
                readTVar runner.vMoment
    syscalls <- (fromMaybe mempty . lookup cogId) <$>
                readTVar runner.vRequests

    let init = PRS{syscalls, flows=[], onPersist=[]}

    st <- flip execStateT init do
              let expected = syscalls
              let actual   = case cogState of
                    CG_CRASHED{}       -> mempty
                    CG_TIMEOUT{}       -> mempty
                    CG_SPINNING cogFun -> getCurrentReqNoun cogFun

              for_ (mapToList expected) \(i,(v,_)) ->
                  case actual V.!? i of
                      Nothing           -> cancelReq i
                      Just w | keep v w -> pure ()
                      Just w            -> cancelReq i >> createReq i w

              for_ (zip [0..] $ toList actual) \(i,v) ->
                  unless (member i expected) do
                      createReq i v

    modifyTVar' runner.vRequests $ insertMap cogId st.syscalls

    pure (st.flows, st.onPersist)

  where

    cancelReq :: Int -> StateT ParseRequestsState STM ()
    cancelReq key = do
        -- traceM ("cancelReq:" <> show key)
        slot <- use (#syscalls % at key)
        case slot of
            Nothing -> error "cancelReq: impossible"
            Just (_, liveReq) -> do
                modifying' #syscalls (deleteMap key)
                lift (cancelRequest liveReq)

    reqDebugSummary :: Request -> ByteString
    reqDebugSummary = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          describeSyscall runner.ctx.hw rc.device rc.params)
        ReqWhat{}  -> "%what"
        ReqSend{}  -> "%send"
        ReqRecv{}  -> "%recv"
        ReqSpin{}  -> "%spin"
        ReqReap{}  -> "%reap"
        ReqStop{}  -> "%stop"
        ReqWho{}   -> "%who"
        UNKNOWN{}  -> "UNKNOWN"

    flowCategory :: Request -> ByteString
    flowCategory = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          syscallCategory runner.ctx.hw rc.device rc.params)
        ReqWhat{}  -> "%what"
        ReqSend{}  -> "%cog"
        ReqRecv{}  -> "%cog"
        ReqSpin{}  -> "%cog"
        ReqReap{}  -> "%reap"
        ReqStop{}  -> "%cog"
        ReqWho{}   -> "%cog"
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
          buildLiveRequest f runner runner.ctx cogId rIdx request

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
