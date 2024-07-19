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

import Control.Monad.State   (StateT, execStateT, modify', runStateT)
import Fan                   (Fan(..), PrimopCrash(..), fanIdx, (%%))
import GHC.Conc              (unsafeIOToSTM)
import GHC.Prim              (reallyUnsafePtrEquality#)
import Optics                (set)
import Server.Convert        ()
import System.Random         (randomIO)
import System.Random.Shuffle (shuffleM)

import qualified GHC.Natural as GHC

import Data.Sorted
import Fan.Convert
import Fan.Prof
import Server.Common
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

--------------------------------------------------------------------------------

receiptQueueMax :: GHC.Natural
receiptQueueMax = 65536


--------------------------------------------------------------------------------

data Moment = MOMENT {
  val  :: Map CogId CogState,
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

data TellOutcome
    = OutcomeOK Fan TellId
    | OutcomeCrash
  deriving (Show)

data Response
    = RespEval EvalOutcome
    | RespCall Fan SysCall
    | RespWhat (Set Nat)
    | RespTell TellPayload
    | RespAsk TellOutcome
    | RespSpin CogId Fan
    | RespReap CogId (Maybe CogState)
    | RespStop CogId (Maybe CogState)
    | RespWait CogId
    | RespWho CogId
  deriving (Show)

responseToVal :: Response -> Fan
responseToVal (RespCall f _) = f
responseToVal (RespWhat w) = toNoun w
responseToVal (RespTell TELL_PAYLOAD{..}) = fanIdx 1 ret
responseToVal (RespAsk (OutcomeOK val _)) = (NAT 0) %% val
responseToVal (RespAsk OutcomeCrash) = (NAT 0)
responseToVal (RespSpin (COG_ID id) _) = fromIntegral id
responseToVal (RespReap _ f) = toNoun f
responseToVal (RespStop _ f) = toNoun f
responseToVal (RespWait _) = (NAT 0)
responseToVal (RespWho (COG_ID id)) = fromIntegral id
responseToVal (RespEval e)   =
    ROW case e of
        TIMEOUT   -> mempty                      -- []
        OKAY _ r  -> rowSingleton r              -- [f]
        CRASH n e -> arrayFromListN 2 [NAT n, e] -- [n e]

responseToReceiptItem :: (Int, ResponseTuple) -> (Int, ReceiptItem)
responseToReceiptItem (idx, tup) = case tup.resp of
    RespEval OKAY{}              -> (idx, ReceiptEvalOK)
    RespSpin cog _               -> (idx, ReceiptSpun cog)
    RespReap cog _               -> (idx, ReceiptReap cog)
    RespStop cog _               -> (idx, ReceiptStop cog)
    RespTell TELL_PAYLOAD{..}    -> (idx, ReceiptTell{..})
    RespAsk (OutcomeOK _ tellid) -> (idx, ReceiptAsk tellid)
    resp                         -> (idx, ReceiptVal (responseToVal resp))

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

data AskRequest = ASKR
    { cogDst  :: CogId
    , channel :: Word64
    , msg     :: Fan
    }
  deriving (Show)

data Request
    = ReqEval EvalRequest
    | ReqCall CallRequest
    | ReqWhat (Set Nat)
    | ReqTell Word64 Fan
    | ReqAsk  AskRequest
    | ReqSpin SpinRequest
    | ReqReap CogId
    | ReqStop CogId
    | ReqWait CogId
    | ReqWho
    | UNKNOWN Fan
  deriving (Show)

{-
  [%eval timeout/@ fun/fan arg/Fan ...]
  [%cog %spin fun/Fan]
  [%cog %ask dst/Nat chan/Nat param/Fan]
  [%cog %tell chan/Nat fun/Fan]
  [%cog %wait dst/Nat]
  [$call synced/? param/Fan ...]
-}
valToRequest :: CogId -> Fan -> Request
valToRequest cogId top = fromMaybe (UNKNOWN top) do
    row <- getRowVec top
    tag <- fromNoun @DeviceName (row V.! 0)

    if tag == "eval" then do
        nat <- fromNoun @Nat (row V.! 1)
        let timeoutSecs = nat
        let timeoutMs   = timeoutSecs * 1000
        guard (length row >= 4)
        let func = row V.! 2
        let args = V.drop 3 row
        let flow = FlowDisabled -- TODO: What?
        let er = EVAL_REQUEST{func,args,cogId,flow,timeoutMs}
        pure (ReqEval er)
        -- e (ReqEval $ error "_" timeoutSecs (row V.! 2) (V.drop 3 row))
    else if tag == "what" then do
        what <- fromNoun @(Set Nat) (row V.! 1)
        pure $ ReqWhat what
    else if tag == "cog" then do
        nat <- fromNoun @Nat (row V.! 1)
        case nat of
            "spin" | length row == 3 -> pure (ReqSpin $ SR $ row V.! 2)
            "reap" | length row == 3 -> ReqReap <$> fromNoun @CogId (row V.! 2)
            "stop" | length row == 3 -> ReqStop <$> fromNoun @CogId (row V.! 2)
            "wait" | length row == 3 -> ReqWait <$> fromNoun @CogId (row V.! 2)
            "tell" | length row == 4 -> do
                channel <- fromNoun (row V.! 2)
                pure (ReqTell channel (row V.! 3))
            "ask"  | length row == 5 -> do
                dst <- fromNoun @CogId (row V.! 2)
                channel <- fromNoun (row V.! 3)
                pure (ReqAsk (ASKR dst channel (row V.! 4)))
            "who"  | length row == 2 -> pure ReqWho
            _ -> Nothing
    else do
        _   <- guard (length row >= 3)
        nat <- fromNoun @Nat (row V.! 1)
        case nat of
            0 -> pure (ReqCall $ CR False tag $ V.drop 2 row)
            1 -> pure (ReqCall $ CR True  tag $ V.drop 2 row)
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

-- | A list of parsed out valid requests from `noun`. For every cog, for every
-- index in that cog's requests table, there is a raw fan value and a
-- `LiveRequest` which contains STM variables to listen
type CogSysCalls = IntMap (Fan, LiveRequest)
type MachineSysCalls = Map CogId CogSysCalls

-- | The PendingAsk contains a pointer to an %asking request, along with its
-- value. It exists so we can quickly look up its %msg at execution time from a
-- pool of open asks.
data PendingAsk = PENDING_ASK
    { requestor :: CogId
    , reqIdx    :: RequestIdx
    , msg       :: Fan
    }
  deriving (Show)

-- | All the information needed for both
data TellPayload = TELL_PAYLOAD
    { asker  :: CogId
    , reqIdx :: RequestIdx

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

-- This is a collection of all {PendingAsk}s within all the cogs of a
-- machine on all channels.  This is part of the top-level Machine STM state,
-- because these IPC interactions need happen transactionally.
type CogChannelPool a = Map CogId (TVar (ChannelPool a))

type ChannelPool a = Map Word64 (TVar (Pool a))

channelPoolRegister :: Word64 -> TVar (ChannelPool a) -> a
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
-- Unlike `poolTakeNext`, this never retries and returns a Maybe instead
-- because retrying during `receiveResponse` can cause more widespread
-- blockage.
channelPoolTake :: Word64 -> TVar (ChannelPool a) -> STM (Maybe a)
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

channelPoolUnregister :: Word64 -> TVar (ChannelPool a) -> Int -> STM ()
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

    , vTellId   :: TVar Int

    , vAsks     :: TVar (CogChannelPool PendingAsk)
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
    | CTell
      { tellId :: TellId
      , tell   :: Fan
      }
    | CAsk
      { tellId :: TellId
      }
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
  | LiveTell {
    ltCogId    :: CogId,
    ltIdx      :: RequestIdx,
    ltChannel  :: Word64,
    ltChannels :: TVar (ChannelPool PendingAsk),
    ltFun      :: Fan
    }
  | LiveAsk {
    lcrChannel  :: Word64,
    lcrPoolId   :: Int,
    lcrChannels :: TVar (ChannelPool PendingAsk)
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
  | LiveWait {
    lwaIdx   :: RequestIdx,
    lwaCogId :: CogId
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
        LiveTell{}  -> "TELL"
        LiveAsk{}   -> "ASK"
        LiveSpin{}  -> "SPIN"
        LiveReap{}  -> "REAP"
        LiveStop{}  -> "STOP"
        LiveWait{}  -> "WAIT"
        LiveWho{}   -> "WHO"
        LiveUnknown -> "UNKNOWN"

validLiveRequest :: LiveRequest -> Bool
validLiveRequest LiveUnknown = False
validLiveRequest _           = True

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

type Tells = Map TellId Fan

recomputeEvals
    :: Moment
    -> Tells
    -> CogId
    -> IntMap ReceiptItem
    -> StateT NanoTime IO ReconstructedEvals
recomputeEvals m tells cogId tab =
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
                        pure (k, ROW (rowSingleton res), Nothing)
            ReceiptSpun newCogId -> do
                case getSpinFunAt m cogId (RequestIdx idx) of
                    Nothing ->
                        throwIO $ INVALID_SPUN_RECEIPT_IN_LOGBATCH newCogId
                    Just fun -> do
                        let ef = CSpin newCogId fun
                        pure (k, NAT $ fromIntegral newCogId.int, Just ef)
            ReceiptTell{..} -> do
                -- We have to retrieve data from both the ask and the tell
                case (getAskFunAt m asker cogId reqIdx,
                      getTellFunAt m cogId (RequestIdx idx)) of
                    (Just ask, Just tellFun) -> do
                        let ret = tellFun %% (toNoun asker) %% ask
                            askResp = fanIdx 0 ret
                            tellResp = fanIdx 1 ret
                            ef = CTell tellId askResp
                        pure (k, tellResp, Just ef)
                    _ ->
                        throwIO $ INVALID_TELL_RECEIPT_IN_LOGBATCH
            ReceiptAsk{..} -> do
                case lookup tellId tells of
                    Nothing ->
                        throwIO $ INVALID_ASK_RECEIPT_IN_LOGBATCH
                    Just askResponse ->
                        pure (k, NAT 0 %% askResponse, Just $ CAsk tellId)
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

    getAskFunAt :: Moment -> CogId -> CogId -> RequestIdx -> Maybe Fan
    getAskFunAt m sender receiver idx = withRequestAt m sender idx $ \case
        ReqAsk (ASKR cogDst _channel msg)
            | cogDst == receiver     -> Just msg
            | otherwise              -> Nothing
        _                            -> Nothing

    getTellFunAt :: Moment -> CogId -> RequestIdx -> Maybe Fan
    getTellFunAt m sender idx = withRequestAt m sender idx $ \case
        ReqTell _channel fun -> Just fun
        _                    -> Nothing

    withRequestAt :: Moment -> CogId -> RequestIdx -> (Request -> Maybe a)
                  -> Maybe a
    withRequestAt m cogId (RequestIdx idx) fun = do
        case lookup cogId m.val of
            Nothing -> Nothing
            Just CG_CRASHED{} -> Nothing
            Just CG_TIMEOUT{} -> Nothing
            Just CG_FINISHED{} -> Nothing
            Just (CG_SPINNING cog) -> do
                let row = getCurrentReqNoun cog
                case row V.!? idx of
                    Nothing  -> Nothing
                    Just val -> fun $ valToRequest cogId val

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

    loop :: BatchNum -> (Moment, Tells) -> [Receipt]
         -> IO (BatchNum, (Moment, Tells))
    loop bn mt []     = pure (bn, mt)
    loop bn (m, t) (x:xs) =
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
                        pure (bn, (m { val = M.insert x.cogNum cog m.val}, t))

            RECEIPT_CRASHED{..} -> do
                -- While it's deterministic whether a plunder computation
                -- crashes or not, its crash value is not deterministic so we
                -- must reconstitute the crash from the recorded result.
                case mybCogFun of
                    Nothing -> throwIO INVALID_CRASHED_IN_LOGBATCH
                    Just final -> do
                        let cog = CG_CRASHED{op,arg,final}
                        pure (bn, (m { val = M.insert x.cogNum cog m.val}, t))

            RECEIPT_OK{..} -> do
                -- The original run succeeded, rebuild the value from the
                -- receipt items.
                (eRes, eWork) <- runStateT (recomputeEvals m t cogNum inputs) 0

                let arg = TAb $ mapFromList $ map tripleToPair eRes

                let runEffect :: (Moment, Tells) -> Maybe CogReplayEffect
                              -> IO (Moment, Tells)
                    runEffect (m, t) = \case
                        Just CSpin{..} -> pure $ (m {
                            val = M.insert reCogId (CG_SPINNING reFun) m.val },
                                                  t)
                        Just CStop{..} -> pure $ (m {
                            val = M.delete reCogId m.val }, t)
                        Just CTell{..} -> pure $ (m, M.insert tellId tell t)
                        Just CAsk{..}  -> pure $ (m, M.delete tellId t)
                        Nothing        -> pure (m, t)

                (m, t) <- foldlM runEffect (m, t) (map third eRes)

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

                loop bn (newMoment, t) xs

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
          vMoment   <- newTVar moment
          vRequests <- newTVar mempty

          reqChannels <- forM (keys moment.val) $ \k -> do
              channelPools <- newTVar mempty
              pure (k, channelPools)
          vAsks     <- newTVar $ mapFromList reqChannels

          vTellId <- newTVar 0

          pure RUNNER{vMoment, ctx, vRequests, vTellId, vAsks}

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
shutdownMachine machine@MACHINE{runnerAsync} = do
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
        let req = EVAL_REQUEST{flow=causeFlow, timeoutMs, func, args, cogId}
        leRecord <- pleaseEvaluate ctx.eval req
        pure ([], LiveEval{leIdx=reqIdx, leRecord}, Nothing)

    ReqCall cr -> do
        callSt <- STVAR <$> newTVar LIVE

        let lcSysCall = SYSCALL cogId cr.device cr.params callSt causeFlow
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

    ReqAsk ASKR{cogDst,channel,msg} -> do
        asks <- readTVar runner.vAsks
        case M.lookup cogDst asks of
            -- TODO: Pretty sure using `error` here is wrong. Figure out what
            -- the real error handling should be when a cog requests from a cog
            -- that doesn't exist.
            Nothing -> error $ "Bad cogDst " <> show cogDst
            Just vChannels -> do
                poolId <-
                    channelPoolRegister channel
                                        vChannels
                                        (PENDING_ASK cogId reqIdx msg)
                pure ([], LiveAsk{lcrChannel=channel,
                                  lcrPoolId=poolId,
                                  lcrChannels=vChannels}, Nothing)

    ReqTell channel function -> do
        asks <- readTVar runner.vAsks
        channels <- case M.lookup cogId asks of
            Nothing   -> error $ "Listening on a nonexistent cog " <> show cogId
            Just pool -> pure pool
        pure ([], LiveTell{ltCogId=cogId,
                           ltIdx=reqIdx,
                           ltChannel=channel,
                           ltChannels=channels,
                           ltFun=function}, Nothing)

    ReqSpin (SR func) -> do
        pure ([], LiveSpin{lsIdx=reqIdx, lsFun=func}, Nothing)

    ReqReap cogid -> do
        pure ([], LiveReap{lrIdx=reqIdx, lrCogId=cogid}, Nothing)

    ReqStop cogid -> do
        pure ([], LiveStop{lstIdx=reqIdx, lstCogId=cogid}, Nothing)

    ReqWait cogid -> do
        pure ([], LiveWait{lwaIdx=reqIdx, lwaCogId=cogid}, Nothing)

    ReqWho -> do
        pure ([], LiveWho{lwIdx=reqIdx, lwCogId=cogId}, Nothing)

    UNKNOWN _ -> do
        pure ([], LiveUnknown, Nothing)

cancelRequest :: LiveRequest -> STM ()
cancelRequest LiveEval{..} = leRecord.cancel.action
cancelRequest LiveCall{..} = lcCancel.action
cancelRequest LiveWhat{}   = pure ()
cancelRequest LiveTell{}   = pure ()
cancelRequest LiveAsk{..} =
    channelPoolUnregister lcrChannel lcrChannels lcrPoolId
cancelRequest LiveSpin{}   = pure () -- Not asynchronous
cancelRequest LiveReap{}   = pure ()
cancelRequest LiveStop{}   = pure ()
cancelRequest LiveWait{}   = pure ()
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

    (_, LiveAsk{}) ->
        -- Asks are never responded to normally, they're responded manually as
        -- a side effect of a tell so that we maintain atomicity of the
        -- request/serve pair in the log.
        retry

    (_, LiveTell{..}) -> do
        channelPoolTake ltChannel ltChannels >>= \case
            Nothing -> pure Nothing
            Just PENDING_ASK{..} -> do
                tellId <- stateTVar st.vTellId \s -> (TellId s, s + 1)

                let pa = TELL_PAYLOAD {
                      asker=requestor,
                      reqIdx,
                      tellId,
                      ret = ltFun %% (toNoun requestor) %% msg
                      }

                pure (Just RTUP{key=ltIdx,
                                resp=RespTell pa,
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

    (_, LiveWait{..}) -> do
        myb <- (lookup lwaCogId . (.val)) <$> readTVar st.vMoment
        case myb of
          Nothing ->
            pure (Just RTUP{key=lwaIdx, resp=RespWait lwaCogId,
                            work=0, flow=FlowDisabled})
          Just _ -> pure Nothing

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
        modifyTVar' st.vAsks $ M.delete cogId
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

        -- Run the event loop until we're forced to stop.
        machineTick

        -- Force a sync to shutdown.
        waitForLogAsyncShutdown machine
  where
    registerCogsWithHardware :: IO ()
    registerCogsWithHardware = do
      m <- readTVarIO st.vMoment
      let k :: [CogId] = keys m.val
      for_ k $ \cogid ->
        for_ machine.ctx.hw.table $ \d -> d.spin cogid

    stopCogsWithHardware :: IO ()
    stopCogsWithHardware = do
      m <- readTVarIO st.vMoment
      for_ (keys m.val) $ \cogid ->
        for_ machine.ctx.hw.table $ \d -> d.stop cogid

    -- We've completed a unit of work. Time to tell the logger about
    -- the new state of the world.
    exportState :: (Receipt, [STM OnCommitFlow]) -> STM ()
    exportState (receipt, onCommit) = do
        when (length onCommit > 0) do
            writeTVar machine.logImmediately True
        writeTBQueue machine.logReceiptQueue (receipt, onCommit)

    -- When a "tell" gets a response, it must be the only response in the
    -- response bundle, so make sure they're handled separately.
    separateTells :: [(CogId, CogSysCalls)] -> [(CogId, CogSysCalls)]
    separateTells = loop
      where
        loop [] = []
        loop ((id,calls):xs) = tellList ++ (id, rest) : loop xs
          where
            (tells, rest) = IM.partition isTell calls

            isTell (_, LiveTell{}) = True
            isTell (_, _)          = False

            tellList = map mkEachTell $ IM.toList tells
            mkEachTell (k, v) = (id, IM.singleton k v)

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
    takeReturns :: IO (Maybe (CogId, [(Int, ResponseTuple)]))
    takeReturns = do
        withAlwaysTrace "WaitForResponse" "cog" do
            -- This is in a separate atomically block because it doesn't
            -- change and we want to minimize contention.
            machineSysCalls <-
              (separateTells . mapToList) <$> atomically (readTVar st.vRequests)

            -- We now have a mapping from cog ids to the CogSysCalls map. We
            -- need to verify if waiting on this will ever complete.
            --
            -- Note: This is a separate mechanism than CG_FINISHED; the
            -- interpreter not understanding a request should trigger an
            -- interpreter shutdown but should not trigger a %reap or %stop
            -- visible finished state.
            case machineHasLiveRequests machineSysCalls of
                False -> pure Nothing
                True -> do
                  reordered <- shuffleM machineSysCalls
                  -- debugText $ "takeReturns: " <> tshow reordered
                  Just <$> (atomically $ asum $ mkCogResponses reordered)

    -- Every machineTick, we try to pick off as much work as possible for cogs
    -- to do.
    --
    -- TODO: This is the simple way of doing things, there's a ton of runtime
    -- gains that could be had at the cost of complexity by having a persistent
    -- async for each Cog, but that adds some sync subtlety while this means
    -- the machine is as fast as only the slowest cog, but is obviously correct.
    machineTick :: IO ()
    machineTick = takeReturns >>= \case
        Nothing -> do
            putStrLn "Shutting down..."
            -- TODO: Check the state of the machine and print it here. Print if
            -- there are timed out cogs. Print if there are cogs which we can't
            -- run because of UNKNOWN requests. Print if all cogs exited
            -- cleanly with no requests (or 0 requests).
            pure ()
        Just (cogId, valTuples) -> do
            results <- cogTick (cogId, valTuples)
            atomically $ do
                mapM_ exportState results
                readTVar st.vMoment >>= writeTVar machine.liveVar
            machineTick

    -- Returns true if there are any valid, open LiveRequests that aren't
    -- UNKNOWN. This is used to end the machine when we will never be able to
    -- provide it with work.
    machineHasLiveRequests :: [(CogId, CogSysCalls)] -> Bool
    machineHasLiveRequests = any \(_, csc) -> cogHasLiveRequests csc

    cogHasLiveRequests :: CogSysCalls -> Bool
    cogHasLiveRequests = any \(_, req) -> validLiveRequest req

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

    -   If we received a COG_TELL message, the corresponding COG_ASK request
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
          let preEvaluate = map (ensureResponseEvaluated . (.resp) . snd) rets
          evalWithTimeout thirtySecondsInMicroseconds preEvaluate fun arg

  let resultReceipt = case result of
        TIMEOUT      -> RECEIPT_TIME_OUT{cogNum,timeoutAmount=runtimeUs}
        CRASH op arg -> RECEIPT_CRASHED{..}
        OKAY{}       -> makeOKReceipt cogNum rets

  let newState = case result of
        OKAY _ resultFan -> case hasNonzeroReqs resultFan of
                                True  -> CG_SPINNING resultFan
                                False -> CG_FINISHED resultFan
        CRASH op arg     -> CG_CRASHED op arg fun
        TIMEOUT          -> CG_TIMEOUT runtimeUs fun

  withAlwaysTrace "Tick" "cog" do
      let responses = ((.resp) . snd) <$> rets

      -- 1) Notify hardware about spins which started a cog. We have to do this
      -- before we record the cog state or do parseRequests because the initial
      -- state of a newly created cog might try to communicate with the
      -- hardware.
      traverse_ performAlertHardwareOnSpin responses

      -- 2) Perform parseRequest and handle all changes that have to be handled
      -- attomically. This has to happen after we notify the hardware about
      -- cogs being spun.
      (sideEffects, startedFlows, onPersists) <- atomically $
          performStateUpdate newState responses runtimeUs resultReceipt

      -- 3) If this cog shut down for any reason, we have to alert the hardware
      -- that it has shut down. This has to be done after parsing requests
      -- because parsing requests will cancel ongoing hardware events.
      case newState of
        CG_SPINNING _ -> pure ()
        _             -> for_ st.ctx.hw.table $ \d -> d.stop cogNum

      -- 4) %cog %ask/%tell must execute in pairs. We finally synchronously
      -- trigger any %ask callback now that the %tell has processed.
      afterResults <- mapM (performAskAck resultReceipt) responses

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

    ensureResponseEvaluated :: Response -> IO ()
    ensureResponseEvaluated (RespTell TELL_PAYLOAD{..}) = do
      -- We must force evaluate the response thunk so that we make sure it
      -- doesn't have a crash, but must do this while crediting the runtime
      -- (and possible crash) to the %tell since the %tell provides the
      -- function.
      evaluate $ force ret
      pure ()

    ensureResponseEvaluated _ = pure ()

    performAlertHardwareOnSpin :: Response -> IO ()
    performAlertHardwareOnSpin (RespSpin newCogId _) = do
        -- If we just spun up another cog, tell all the hardware
        -- devices that it exists now.
        for_ st.ctx.hw.table $ \d -> d.spin newCogId
    performAlertHardwareOnSpin _ = pure ()

    performStateUpdate newState responses runtimeUs resultReceipt = do
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

    performSpinEffects :: Response
                       -> STM ([Flow], [STM OnCommitFlow])
    performSpinEffects (RespSpin newCogId fun) = do
        reqChannelPools <- newTVar mempty
        modifyTVar' st.vAsks \reqs ->
            M.insert newCogId reqChannelPools reqs
        modifyTVar' st.vMoment \m ->
            m { val=insertMap newCogId (CG_SPINNING fun) m.val }
        parseRequests st newCogId

    performSpinEffects _ = pure mempty

    -- Performed outside the Big Atomically Block
    performAskAck :: Receipt
                   -> Response
                   -> IO ([Flow], [(Receipt, [STM OnCommitFlow])])
    performAskAck result (RespTell TELL_PAYLOAD{..}) = do
        -- When an IPC message is received, the corresponding
        -- ask syscall on the asking cog must also be processed
        -- within the same transaction.
        --
        -- We do this recursively invoking runResponse for that
        -- syscall as well.
        --
        -- We are sure that the `reqIdx` still corresponds to
        -- an active COG_ASK syscall because of reasons.
        let outcome = case result of
              RECEIPT_OK{} -> OutcomeOK (fanIdx 0 ret) tellId
              _            -> OutcomeCrash

        -- TODO: Enable profiling flows; how do we connect
        -- the recv completing to this?
        let rtup = RTUP { key  = reqIdx
                        , resp = RespAsk outcome
                        , work = 0
                        , flow = FlowDisabled
                        }
        runResponse st asker [(reqIdx.int, rtup)]

    performAskAck _ _ = pure ([], [])


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
                    CG_FINISHED{}      -> mempty
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
        ReqTell{}  -> "%tell"
        ReqAsk{}   -> "%ask"
        ReqSpin{}  -> "%spin"
        ReqReap{}  -> "%reap"
        ReqStop{}  -> "%stop"
        ReqWait{}  -> "%wait"
        ReqWho{}   -> "%who"
        UNKNOWN{}  -> "UNKNOWN"

    flowCategory :: Request -> ByteString
    flowCategory = \case
        ReqEval{}  -> "%eval"
        ReqCall rc -> "%call " <> (encodeUtf8 $
                          syscallCategory runner.ctx.hw rc.device rc.params)
        ReqWhat{}  -> "%what"
        ReqTell{}  -> "%tell"
        ReqAsk{}   -> "%ask"
        ReqSpin{}  -> "%cog"
        ReqReap{}  -> "%reap"
        ReqStop{}  -> "%cog"
        ReqWait{}  -> "%wait"
        ReqWho{}   -> "%cog"
        UNKNOWN{}  -> "UNKNOWN"

    createReq :: Int -> Fan -> StateT ParseRequestsState STM ()
    createReq i v = do
        let request = valToRequest cogId v
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
            writeMachineSnapshot machine.ctx.lmdb bn moment.val

        unless shutdown do
            loop moment.work
