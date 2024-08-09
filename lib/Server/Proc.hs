---Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -freverse-errors #-}


module Server.Proc
    ( spawnProc
    , WriteRequest(..)
    , ReadRequest(..)
    , CogHandle(..)
    )
where

import PlunderPrelude

import Control.Monad.State   (execStateT, modify)
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

thirtySecondsInMicroseconds :: Nat
thirtySecondsInMicroseconds = 30 * 10 ^ (6::Int)

data Response
  = RespRead Fan
  | RespWrite Fan -- TODO do we need this?
  | RespSyscall Fan
  deriving (Show)

instance ToNoun Response where
  toNoun = \case
    RespRead result -> result
    RespWrite _     -> NAT 0
    RespSyscall result -> result

data ReadRequest
  = CogRead { idx :: RequestIdx, query :: Fan, state :: CallStateVar }

data WriteRequest
  = CogWrite { idx :: RequestIdx, cmd :: Fan, state :: CallStateVar }

instance Show WriteRequest where
  show CogWrite{cmd} = show cmd

instance ToNoun WriteRequest where
  toNoun write = write.cmd

instance ToNoun ReadRequest where
  toNoun read = read.query

data Request
  = DB_READ Fan
  | DB_WRITE Fan
  | TIME_WHEN
  | TIME_WAIT Nat
  deriving (Show)

instance FromNoun Request where
  fromNoun fan = do
      row <- getRowVec fan
      tag <- fromNoun @Natural (row V.! 0)
      case tag of
  --    0 -> Just $ DB_WAIT
        1 -> Just $ DB_READ (row V.! 1)
        2 -> Just $ DB_WRITE (row V.! 1)
        3 -> Just TIME_WHEN
        4 -> TIME_WAIT <$> fromNoun (row V.! 1)
        _ -> Nothing

getCurrentReqNoun :: Fan -> Vector Fan
getCurrentReqNoun s =
    case s of
        KLO _ xs ->
            let len = sizeofSmallArray xs in
            case (xs .! (len-1)) of
                ROW x -> V.fromArray x
                _     -> mempty
        _ -> mempty
  
---- A request noun is empty if it is a row with a nonzero value.
hasNonzeroReqs :: Fan -> Bool
hasNonzeroReqs = any (/= NAT 0) . getCurrentReqNoun

data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

-- | A list of parsed out valid requests from `noun`. For every proc, for every
-- index in that proc's requests table, there is a raw fan value and a
-- `LiveRequest` which contains STM variables to listen
type ProcSysCalls = IntMap (Fan, LiveRequest)

data CogHandle = COG_HANDLE
    { write :: WriteRequest -> STM ()
    , read  :: ReadRequest -> STM ()
    }

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { hw   :: DeviceTable
    , init :: Fan          -- ^ Starting value
    , proc :: Fan          -- ^ Current value
    , reqs :: ProcSysCalls -- ^ Current requests table
    , cog  :: CogHandle
    }

-- -----------------------------------------------------------------------

data ResponseTuple = RTUP
    { key  :: RequestIdx
    , resp :: Response
    }
  deriving (Show)

getReadResult :: ReadRequest -> STM Fan
getReadResult CogRead{..} = readTVar state.var >>= \case
  DONE result _ -> result <$ writeTVar state.var DEAD -- TODO abstract out the result handling using a typeclass or a function in the record
  _             -> retry -- TODO if the result is DEAD we should remove the request

getWriteResult :: WriteRequest -> STM Fan
getWriteResult CogWrite{..} = readTVar state.var >>= \case
  DONE _ _ -> toNoun () <$ writeTVar state.var DEAD -- TODO maybe return something? if the Proc should be able to react to error msgs.
  _        -> retry


-- | A `LiveRequest` is a handle that could produce a Response to an open
-- Request.
data LiveRequest
  = LiveDbRead
    { idx :: RequestIdx
    , lrCall :: ReadRequest
    }
  | LiveDbWrite
    { idx :: RequestIdx
    , lwCall :: WriteRequest
    }
  | LiveSyscall
    { idx :: RequestIdx
    , lsCall :: SysCall
    }

instance Show LiveRequest where
    show = \case
        LiveDbRead{}  -> "READ"
        LiveDbWrite{} -> "WRITE"
        LiveSyscall{lsCall} -> intercalate " " ["[SYSCALL", show lsCall.dev, show lsCall.args, "]"]

makeFieldLabelsNoPrefix ''Runner

-- -----------------------------------------------------------------------
-- No template haskell beyond this point because optics.
-- -----------------------------------------------------------------------

spawnProc :: Debug => Fan -> CogHandle -> DeviceTable -> IO (Async ())
spawnProc proc cog hw =
  let init = proc
      reqs = mempty in
  asyncOnCurProcess $ withThreadName "Foo"
  $ handle (onErr "runner foo")
  $ runnerFun "procname" RUNNER{..}
  where
    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- | Given a Request parsed from the proc, turn it into a LiveRequest that can
-- produce a value and that we can listen to.
buildLiveRequest :: Debug => DeviceTable -> CogHandle -> RequestIdx -> Request -> STM LiveRequest
buildLiveRequest hw cog reqIdx req = do
    callSt <- STVAR <$> newTVar LIVE
    case req of
      DB_READ query -> do
        let lrCall = CogRead reqIdx query callSt
        cog.read lrCall
        pure $ LiveDbRead reqIdx lrCall
        -- modifying' #reads $ flip poolRegister lrCall

      DB_WRITE cmd -> do
        let lwCall = CogWrite reqIdx cmd callSt
        cog.write lwCall
        pure $ LiveDbWrite reqIdx lwCall

      TIME_WHEN -> do
        let timeDevice = DEV_NAME "time"
        let syscall = SYSCALL timeDevice (fromList [NAT "when"]) callSt FlowDisabled -- TODO Flow
        callHardware hw timeDevice syscall
        pure $ LiveSyscall reqIdx syscall

      TIME_WAIT ns -> do
        let timeDevice = DEV_NAME "time"
        let syscall = SYSCALL timeDevice (fromList ["wait", NAT ns]) callSt FlowDisabled
        callHardware hw timeDevice syscall
        pure $ LiveSyscall reqIdx syscall

receiveResponse :: (Fan, LiveRequest) -> STM ResponseTuple
receiveResponse = \case
    (_, LiveDbRead{..}) -> do -- TODO this should probably be a typeclass?
      outcome <- getReadResult lrCall
      pure RTUP{key=idx, resp=RespRead outcome}

    (_, LiveDbWrite{..}) -> do
      outcome <- getWriteResult lwCall
      pure RTUP{key=idx, resp=RespWrite outcome}

    (_, LiveSyscall{..}) -> do
      getCallResponse lsCall >>= \case
        Nothing -> retry
        Just (f,_) -> pure RTUP{key=idx, resp=RespSyscall f}




-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Proc Runner --------------------------------------------------------------

runnerFun :: Debug => ByteString -> Runner -> IO ()
runnerFun processName st =
    bracket_ registerProcsWithHardware stopProcsWithHardware $ do
        -- Process the initial syscall vector
        newReqs <- atomically $ parseRequests st

        -- Run the event loop until we're forced to stop.
        procTick st{reqs=newReqs}
  where
    registerProcsWithHardware :: IO ()
    registerProcsWithHardware = pure () -- for_ ctx.hw.table $ \d -> d.spin procId

    stopProcsWithHardware :: IO ()
    stopProcsWithHardware = pure () -- for_ ctx.hw.table $ \d -> d.stop procId


    procTick :: Runner -> IO ()
    procTick st = do
      response <- withAlwaysTrace "WaitForResponse" "proc"
        case IM.elems st.reqs of
          []           -> pure Nothing
          procSysCalls -> Just <$> do
              reordered <- shuffleM procSysCalls -- TODO shuffling is ugly! maybe use a finger tree instead?
              -- debugText $ "takeReturns: " <> tshow reordered
              atomically . asum $ receiveResponse <$> reordered
      case response of
        Nothing -> procTick $ reset st
        Just response ->
          procTick =<< (withProcessName processName $
                         withThreadName ("Proc: ") $
                           runResponse st response)


reset :: Runner -> Runner
reset st@RUNNER{init} = st{proc=init, reqs=mempty}

{-
    Given a set of responses to syscalls in a procs SysCall table, create
    a new event value and pass that into the proc, to get the new proc state.

    Side Effects:

    -   Any new requests are launched.

    Results:

    -   The PLAN value for the proc is replaced.

    -   The proc's requests row is updated to reflect the new set of
        requests.

    Q: what happens to outstanding requests if we restart the process?
    A: we just don't listen to them.
    Q: but is this a space leak? what happens on the cog/hw end?
-}
runResponse :: Debug => Runner -> ResponseTuple -> IO Runner
runResponse st@RUNNER{..} resp = do
  (_, result) <- do
      withAlwaysTrace "Eval" "proc" do
          evalWithTimeout thirtySecondsInMicroseconds [] (proc %% toNoun resp.key) (toNoun resp.resp)

  let st' = case result of
        OKAY _ resultFan | hasNonzeroReqs resultFan
          -> st{proc=resultFan, reqs=deleteMap resp.key.int reqs}
        _ -> reset st

  withAlwaysTrace "Tick" "proc" do
      -- Perform parseRequest and handle all changes that have to be handled
      -- atomically.
      newReqs <- atomically $ parseRequests st'
      pure st'{reqs=newReqs}

-- | Given a proc in a runner, update the `requests`.
parseRequests :: Debug => Runner -> STM ProcSysCalls
parseRequests RUNNER{..} =
  flip execStateT reqs $
    for_ (getCurrentReqNoun proc) \v ->
      whenJust (reqFromVal v) \(rIdx, request) -> do
        liveReq <- lift $ buildLiveRequest hw cog rIdx request
        modify $ insertMap rIdx.int (v, liveReq)
  where
    reqFromVal :: Fan -> Maybe (RequestIdx, Request)
    reqFromVal v = do
        ROW reqFan <- Just v
        let fan = V.fromArray reqFan
        i <- fromNoun $ fan V.! 0
        request <- fromNoun $ fan V.! 1
        pure (RequestIdx i, request)

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
