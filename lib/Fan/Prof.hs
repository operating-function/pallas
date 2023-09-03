-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Fan.Prof where

import PlunderPrelude

import Control.Concurrent.STM.TMVar (writeTMVar)
import Data.Time.Clock.POSIX        (getPOSIXTime)
import GHC.Conc                     (ThreadId, myThreadId, unsafeIOToSTM)

import qualified Control.Concurrent.KazuraQueue as K
import qualified Data.ByteString                as BS
import qualified Data.Map                       as M
import qualified System.IO                      as Sys
import qualified System.Process                 as Sys

-- V2 design
--
-- The chrome tracing json is divided into processes and threads. We must fit
-- our execution model into this tracing format so we can use tools in this
-- ecosystem. So:
--
-- - A Process is a group of named threads. It doesn't correspond to a "pid" as
--   a unix operating system commonly uses the term. It is meant to group
--   several "threads" together.
--
-- - A Thread is some group of computation that forms one callstack. It doesn't
--   correspond directly to `myThreadId`. It usually is a single async, but can
--   span multiple asyncs as long as one is waiting on the other. (There are
--   utilities for spawning asyncs which maintain this context.)
--
-- In both cases, the user level primitive is in terms of names. If you spawn
-- an async on "the same process" with the same name, these two asyncs will
-- show up as the same in the profiling output, which allows you to collate
-- temporary asyncs which are semantically doing the same thing.

type ProcessName = ByteString
type ThreadName = ByteString

-- Process --------------------------------------------------------------------

-- Process all IO actions and subsequent asyncs under this process names.
withProcessName :: ProcessName -> IO a -> IO a
withProcessName name action = do
  readIORef vProfSys >>= \case
    Nothing  -> action
    Just sys -> do
      processId <- getPidForProcessName name
      withPid processId action

withPid :: Pid -> IO a -> IO a
withPid pid action = do
  readIORef vProfSys >>= \case
    Nothing  -> action
    Just sys -> do
      tid <- myThreadId
      bracket_ (setName tid pid) (clearName tid) action
  where
    setName tid pid = do
      atomicModifyIORef' vProfSys $ \(Just nps) -> (Just nps {
        threadToProcess = M.insert tid pid (threadToProcess nps)
        }, ())

    clearName tid = do
      -- Unassociate this tid from this pid, but don't remove the process id;
      -- that's persistant and has probably been written to the log.
      atomicModifyIORef' vProfSys $ \(Just nps) -> (Just nps {
        threadToProcess = M.delete tid (threadToProcess nps)
        }, ())


getCurrentPid :: IO (Maybe Pid)
getCurrentPid = do
  rawTid <- myThreadId
  readIORef vProfSys >>= \case
    Nothing                        -> pure Nothing
    Just PROF_SYS{threadToProcess} -> pure $ lookup rawTid threadToProcess

getPidForProcessName :: ByteString -> IO Pid
getPidForProcessName name = do
  (processId, allocated) <-
    atomicModifyIORef' vProfSys $ \(Just nps) ->
      case lookup name (processNames nps) of
          Nothing ->
            (Just nps { nextProcessId = 1 + nextProcessId nps,
                        processNames = M.insert name
                                                (nextProcessId nps)
                                                (processNames nps)
                      },
             (nextProcessId nps, True))
          Just pid -> (Just nps, (pid, False))

  when allocated $ do
    readIORef vProfSys >>= \case
      Nothing -> pure ()
      Just PROF_SYS{collectorQ} -> do
        K.writeQueue collectorQ [M_EV $ SetProcessName processId name]

  pure processId


-- Thread ---------------------------------------------------------------------

withThreadName :: ThreadName -> IO a -> IO a
withThreadName threadName action = do
  readIORef vProfSys >>= \case
    Nothing  -> action
    Just sys -> do
      pid <- getCurrentPid >>= \case
        Just pid -> pure pid
        Nothing  -> getPidForProcessName "UNKNOWN PROCESS"
      rawTid <- myThreadId
      bracket_ (setName pid rawTid)
               (clearName pid rawTid)
               action
  where
    setName pid rawTid = do
      tid <- getTidForThreadName pid threadName
      stack <- newIORef []
      atomicModifyIORef' vProfSys $ \(Just nps) ->
        let td = threadData nps
        in (Just nps {
               -- Allocate the thread data if this is the first time we have an
               -- async with this name.
               threadData = case M.lookup tid td of
                   Nothing -> M.insert tid (ThreadData pid threadName stack) td
                   Just _  -> td,
               asyncToTid = M.insert rawTid tid (asyncToTid nps)
        }, ())

    clearName pid rawTid = do
      -- Note: only delete the mapping between the async's raw tid and the
      -- virtual tid we've assigned.
      atomicModifyIORef' vProfSys $ \(Just nps) ->
        (Just nps {
            asyncToTid = M.delete rawTid (asyncToTid nps)
            }, ())


getCurrentTid :: IO (Maybe Tid)
getCurrentTid = do
  readIORef vProfSys >>= \case
    Nothing                   -> pure Nothing
    Just PROF_SYS{asyncToTid} -> do
      rawTid <- myThreadId
      pure $ lookup rawTid asyncToTid


getTidForThreadName :: Pid -> ThreadName -> IO Tid
getTidForThreadName pid name = do
  (tid, allocated) <-
    atomicModifyIORef' vProfSys $ \(Just nps) ->
      case lookup (pid, name) (threadNames nps) of
        Nothing ->
          (Just nps { nextTid = 1 + nextTid nps,
                      threadNames = M.insert (pid, name)
                                             (nextTid nps)
                                             (threadNames nps)
                    },
           (nextTid nps, True))
        Just tid -> (Just nps, (tid, False))

  when allocated $
    readIORef vProfSys >>= \case
      Nothing -> pure ()
      Just PROF_SYS{collectorQ} -> do
        K.writeQueue collectorQ [M_EV $ SetThreadName pid tid name]

  pure tid

-- Sometimes you need to associate an async with a different already existing
-- tid.
withCopiedTid :: Maybe Tid -> IO a -> (IO a)
withCopiedTid mybTid action = do
  sys <- readIORef vProfSys
  case (sys, mybTid) of
    (Nothing, _) -> action
    (_, Nothing) -> action
    (Just sys, Just tid) -> do
      pid <- getCurrentPid >>= \case
        Just pid -> pure pid
        Nothing  -> getPidForProcessName "UNKNOWN PROCESS"
      rawTid <- myThreadId
      bracket_ (setName pid rawTid tid)
               (clearName pid rawTid)
               action
  where
    setName pid rawTid tid = modify $ M.insert rawTid tid
    clearName pid rawTid = modify $ M.delete rawTid

    modify fun = atomicModifyIORef' vProfSys $ \(Just nps) ->
        (Just nps {
            asyncToTid = fun (asyncToTid nps)
            }, ())

-- Creates a new async on a new "Process" with a new "Thread" named something.
newProcessAsync :: ByteString -> ByteString -> IO a -> IO (Async a)
newProcessAsync processName threadName action = async $
  withProcessName processName $
    withThreadName threadName $
      action

newProcessAsyncBound :: ByteString -> ByteString -> IO a -> IO (Async a)
newProcessAsyncBound processName threadName action = asyncBound $
  withProcessName processName $
    withThreadName threadName $
      action

setThreadSortIndex :: Int -> IO ()
setThreadSortIndex idx = do
  readIORef vProfSys >>= \case
    Nothing                   -> pure ()
    Just PROF_SYS{collectorQ, threadData} -> do
      mybTid <- getCurrentTid
      case mybTid of
        Nothing -> do
          -- putStrLn "NO REGISTERED TID FOR SORT INDEX"
          pure ()
        Just tid -> do
          case lookup tid threadData of
            Nothing -> error "Unregistered thread trying to set sort order"
            Just ThreadData{..} -> do
              K.writeQueue collectorQ [M_EV $ SetThreadSortIndex pid tid idx]

-- We have to have a more general profiling system. Here are the requirements:
--
-- - Multiple named "threads" (tids)
--
-- - Use an execution stack for each thread.
--
--   - Use complete events ("ph": "X") instead of separate "B" and "E" events
--     to track the beginning and end. This will shrink the file by 50%.
--
--   - Allow the user to have a per thread minimum event duration to not store
--     millions of 1 microsecond events.

type Name = ByteString
type MicroTime = Word64
type Cat = Text
type Pid = Int
type Tid = Int
type RawThreadId = ThreadId

type Args = Map Text (Either Int Text)

data ProfMeta
  = SetProcessName Pid Name
  | SetThreadName Pid Tid Name
  | SetThreadSortIndex Pid Tid Int

data CompleteEv = CompleteEv Name Cat MicroTime MicroTime Pid Tid


-- We need a way to allocate flows to make the

-- Flows
newtype FlowId = FlowId { unflow :: Int }
  deriving (Show)

data FlowEventType
  = FE_START
  | FE_STEP
  | FE_END

data ProfEvent
  = M_EV ProfMeta
  | X_EV {
      name     :: Name,
      cat      :: Cat,
      start    :: MicroTime,
      duration :: MicroTime,
      pid      :: Pid,
      tid      :: Tid,
      args     :: Args
      }
  | B_EV {
      name  :: Name,
      cat   :: Cat,
      start :: MicroTime,
      pid   :: Pid,
      tid   :: Tid
      }
  | E_EV {
      name :: Name,
      cat  :: Cat,
      end  :: MicroTime,
      pid  :: Pid,
      tid  :: Tid,
      args :: Args
      }
  | F_EV {
      name      :: Name,
      cat       :: Cat,
      flowId    :: FlowId,
      eventType :: FlowEventType,
      time      :: MicroTime,
      pid       :: Pid,
      tid       :: Tid
      }
  | I_EV {
      name :: Name,
      cat  :: Cat,
      time :: MicroTime,
      pid  :: Pid,
      tid  :: Tid,
      args :: Args
      }
  | END_PROFILING (MVar ())

data StackEntry = StackEntry Name Cat MicroTime
  deriving (Show)

data ThreadData = ThreadData {
  pid        :: Pid,
  threadName :: ByteString,
  stack      :: IORef [StackEntry]
  }

data ProfSys = PROF_SYS {
  nextProcessId   :: Pid,
  processNames    :: Map ProcessName Pid,
  threadToProcess :: Map RawThreadId Pid,

  traceLaws       :: Bool,

  nextTid         :: Tid,
  threadNames     :: Map (Pid, ThreadName) Tid,
  asyncToTid      :: Map RawThreadId Tid,
  threadData      :: Map Tid ThreadData,

  minDuration     :: MicroTime,

  collectorQ      :: K.Queue [ProfEvent],
  collectorAsync  :: Async ()
  }

vProfSys :: IORef (Maybe ProfSys)
vProfSys = unsafePerformIO (newIORef $ Nothing)

enableProfiler :: FilePath -> Bool -> IO ()
enableProfiler fp traceLaws = do
  readIORef vProfSys >>= \case
    Nothing -> pure ()
    Just _  -> error "Error: enabled the profiler subsystem twice"

  let nextProcessId = 0
      processNames = mempty
      threadToProcess = mempty
      nextTid = 0
      asyncToTid = mempty
      threadNames = mempty
      threadData = mempty
      minDuration = 5 -- todo: make configurable.

  collectorQ <- K.newQueue
  collectorAsync <- async $ writerThread fp collectorQ

  writeIORef vProfSys (Just PROF_SYS{..})
  atomically $ writeTMVar vNextFlow 100

lawProfilingEnabled :: MonadIO m => m Bool
lawProfilingEnabled = readIORef vProfSys <&> maybe False (.traceLaws)

profEnabled :: MonadIO m => m Bool
profEnabled = readIORef vProfSys <&> maybe False (const True)

writerThread :: FilePath -> K.Queue [ProfEvent] -> IO ()
writerThread logFP q = Sys.withFile logFP Sys.WriteMode start
  where
    start :: Handle -> IO ()
    start fh = do
      BS.hPut fh "["
      readQ fh True

    readQ fh first = do
      events <- K.readQueue q
      processEvents events fh first

    processEvents [] fh first = readQ fh first
    processEvents (x:xs) fh first = case x of
      M_EV (SetProcessName pid name) -> do
        unless first $ BS.hPut fh ","
        writeProcessName fh pid name
        processEvents xs fh False
      M_EV (SetThreadName pid tid name) -> do
        unless first $ BS.hPut fh ","
        writeThreadName fh pid tid name
        processEvents xs fh False
      M_EV (SetThreadSortIndex pid tid idx) -> do
        unless first $ BS.hPut fh ","
        writeThreadSortIndex fh pid tid idx
        processEvents xs fh False
      X_EV{..} -> do
        unless first $ BS.hPut fh ","
        writeComplete fh name cat start duration pid tid args
        processEvents xs fh False
      B_EV{..} -> do
        unless first $ BS.hPut fh ","
        writeBegin fh name cat start pid tid
        processEvents xs fh False
      E_EV{..} -> do
        unless first $ BS.hPut fh ","
        writeEnd fh name cat end pid tid args
        processEvents xs fh False
      F_EV{..} -> do
        unless first $ BS.hPut fh ","
        writeFlow fh name cat flowId eventType time pid tid
        processEvents xs fh False
      I_EV{..} -> do
        unless first $ BS.hPut fh ","
        writeInstant fh name cat time pid tid args
        processEvents xs fh False
      END_PROFILING stopMVar -> do
        BS.hPut fh "]"
        Sys.hClose fh
        putMVar stopMVar ()
        -- stop

    writePidTidName h pid tid name = do
      Sys.hPutStr h "\"pid\":"
      Sys.hPutStr h (show pid)
      Sys.hPutStr h ",\"tid\":"
      Sys.hPutStr h (show tid)
      Sys.hPutStr h ",\"name\":\""
      BS.hPut h name
      Sys.hPutStr h "\""

    threadNameLabel = encodeUtf8 "thread_name"
    writeThreadName h pid tid name = do
      Sys.hPutStr h "{\"ph\":\"M\",\"cat\":\"__metadata\","
      writePidTidName h pid tid threadNameLabel
      Sys.hPutStr h ",\"ts\":0,\"args\":{\"name\":\""
      BS.hPut h name
      Sys.hPutStr h "\"}}\n"

    threadSortOrderLabel = encodeUtf8 "thread_sort_index"
    writeThreadSortIndex h pid tid idx = do
      Sys.hPutStr h "{\"ph\":\"M\",\"cat\":\"__metadata\","
      writePidTidName h pid tid threadSortOrderLabel
      Sys.hPutStr h ",\"ts\":0,\"args\":{\"sort_index\":"
      Sys.hPutStr h (show idx)
      Sys.hPutStr h "}}\n"

    processNameLabel = encodeUtf8 "process_name"
    writeProcessName h pid name = do
      Sys.hPutStr h "{\"ph\":\"M\",\"cat\":\"__metadata\","
      writePidTidName h pid 0 processNameLabel
      Sys.hPutStr h ",\"ts\":0,\"args\":{\"name\":\""
      BS.hPut h name
      Sys.hPutStr h "\"}}\n"

    argPairToString (k,v) = concat [
      "\"",
      unpack k,
      "\":",
      case v of
        Left i  -> show i
        Right s -> "\"" <> unpack s <> "\""
      ]

    writeComplete h name cat start duration pid tid args = do
      Sys.hPutStr h "{\"ph\":\"X\",\"cat\":\""
      Sys.hPutStr h $ unpack cat
      Sys.hPutStr h "\","
      writePidTidName h pid tid name
      Sys.hPutStr h ",\"ts\":"
      Sys.hPutStr h (show start)
      Sys.hPutStr h ",\"dur\":"
      Sys.hPutStr h (show duration)
      when (not $ M.null args) $ do
        Sys.hPutStr h ",\"args\":{";
        let out = join $ intersperse "," $ map argPairToString (M.toList args)
        Sys.hPutStr h out
        Sys.hPutStr h "}";
      Sys.hPutStr h "}\n"

    writeBegin h name cat start pid tid = do
      Sys.hPutStr h "{\"ph\":\"B\",\"cat\":\""
      Sys.hPutStr h $ unpack cat
      Sys.hPutStr h "\","
      writePidTidName h pid tid name
      Sys.hPutStr h ",\"ts\":"
      Sys.hPutStr h (show start)
      Sys.hPutStr h "}\n"

    writeEnd h name cat end pid tid args = do
      Sys.hPutStr h "{\"ph\":\"E\",\"cat\":\""
      Sys.hPutStr h $ unpack cat
      Sys.hPutStr h "\","
      writePidTidName h pid tid name
      Sys.hPutStr h ",\"ts\":"
      Sys.hPutStr h (show end)
      when (not $ M.null args) $ do
        Sys.hPutStr h ",\"args\":{";
        let out = join $ intersperse "," $ map argPairToString (M.toList args)
        Sys.hPutStr h out
        Sys.hPutStr h "}";
      Sys.hPutStr h "}\n"

    writeFlow h name cat (FlowId flowInt) eventType time pid tid = do
      Sys.hPutStr h "{\"ph\":\""
      case eventType of
        FE_START -> Sys.hPutStr h "s"
        FE_STEP  -> Sys.hPutStr h "t"
        -- Force enclosure semantics on end events so we have consistent
        -- functions for handling flows.
        FE_END   -> Sys.hPutStr h "f\",\"bp\":\"e"
      Sys.hPutStr h "\",\"cat\":\""
      Sys.hPutStr h $ unpack cat
      Sys.hPutStr h "\","
      writePidTidName h pid tid name
      Sys.hPutStr h ",\"ts\":"
      Sys.hPutStr h (show time)
      Sys.hPutStr h ",\"id\":"
      Sys.hPutStr h (show flowInt)
      Sys.hPutStr h "}\n"

    writeInstant h name cat time pid tid args = do
      Sys.hPutStr h "{\"ph\":\"i\",\"cat\":\""
      Sys.hPutStr h $ unpack cat
      Sys.hPutStr h "\","
      writePidTidName h pid tid name
      Sys.hPutStr h ",\"ts\":"
      Sys.hPutStr h (show time)
      when (not $ M.null args) $ do
        Sys.hPutStr h ",\"args\":{";
        let out = join $ intersperse "," $ map argPairToString (M.toList args)
        Sys.hPutStr h out
        Sys.hPutStr h "}";
      -- TODO: I was intending for this to be thread instant because I both
      -- remember and the documentation claims that thread local instant events
      -- draw the entire height of the event. But that's broken on the current
      -- version of chrome://tracing. So make exceptions even noisier by being
      -- in process scope for now, and think about revisiting this later.
      Sys.hPutStr h ",\"s\":\"p\"}\n"

whenHasThreadData :: MonadIO m
                  => Text
                  -> (ProfSys -> Tid -> ThreadData -> m ()) -> m ()
whenHasThreadData task fun = do
  readIORef vProfSys >>= \case
    Nothing -> pure ()
    Just sys@PROF_SYS{..} -> do
      mybTid <- liftIO $ getCurrentTid
      case mybTid of
        Nothing -> do
          -- rawTid <- liftIO $ myThreadId
          -- putStrLn $ "NO REGISTERED TID FOR RAW " <> tshow rawTid <> " ("
          --         <> task <> ")"
          pure ()
        Just tid -> do
          case lookup tid threadData of
            Nothing -> error "Error: unregistered thread trying to log: "
            Just td -> fun sys tid td

startEvent :: MonadIO m => Name -> Cat -> m ()
startEvent name cat = whenHasThreadData ("startEvent " <> decodeUtf8 name) $
  \PROF_SYS{..} tid ThreadData{..} -> do
    now <- liftIO $ getPOSIXTime
    let wen = round (1000000*now) :: Word64
    atomicModifyIORef' stack \xs -> ((StackEntry name cat wen) : xs, ())

popEvent :: MonadIO m => Bool -> Args -> m ()
popEvent always args =
  whenHasThreadData "popEvent" $ \PROF_SYS{..} tid ThreadData{..} -> do
    now <- liftIO $ getPOSIXTime
    let wen = round (1000000*now) :: Word64

    StackEntry name cat start <-
      atomicModifyIORef' stack \(x:xs) -> (xs, x)

    let duration = fromIntegral (wen - start)
    case (always || duration >= minDuration) of
      True  -> do
        -- If duration<minDuration then `always` is set.
        let dur = max minDuration duration
        liftIO $ K.writeQueue collectorQ [X_EV name cat start dur pid tid args]
      False -> pure ()

-- For simple events, which use no args (and no flow events when I add those).
withSimpleTracingEvent :: MonadUnliftIO m => Name -> Cat -> m a -> m a
withSimpleTracingEvent name cat =
    bracket_ (startEvent name cat) (popEvent False mempty)

-- For simple events, which use no args (and no flow events when I add those).
withSimpleTracingEventPure :: Name -> Cat -> a -> a
withSimpleTracingEventPure name cat a =
    unsafePerformIO do withSimpleTracingEvent name cat do evaluate a

withTracingEventArgs :: MonadUnliftIO m => Name -> Cat -> Args -> m a -> m a
withTracingEventArgs name cat args =
    bracket_ (startEvent name cat) (popEvent False args)

withAlwaysTrace :: MonadUnliftIO m => Name -> Cat -> m a -> m a
withAlwaysTrace name cat =
    bracket_ (startEvent name cat) (popEvent True mempty)

withAlwaysTraceArgs :: MonadUnliftIO m => Name -> Cat -> Args -> m a -> m a
withAlwaysTraceArgs name cat args =
    bracket_ (startEvent name cat) (popEvent True mempty)

withTraceResultArgs :: MonadUnliftIO m => Name -> Cat -> m (Args, a) -> m a
withTraceResultArgs name cat action = do
  mask $ \restore -> do
    startEvent name cat
    (args, ret) <- restore action `onException` popEvent False mempty
    popEvent False mempty
    pure ret

withProfileOutput :: FilePath -> Bool -> IO a -> IO a
withProfileOutput fil traceLaws act = do
  enableProfiler fil traceLaws
  a <- act

  -- Tell the collector to shut down and wait until it's closed the file
  -- handle.
  readIORef vProfSys >>= \case
    Nothing -> pure a
    Just PROF_SYS{..} -> do
      stop <- newEmptyMVar
      K.writeQueue collectorQ [END_PROFILING stop]
      takeMVar stop
      pure a

-- Async wrappers -------------------------------------------------------------

asyncOnCurProcess :: IO a -> IO (Async a)
asyncOnCurProcess = asyncOnCurProcessImpl async

asyncBoundOnCurProcess :: IO a -> IO (Async a)
asyncBoundOnCurProcess = asyncOnCurProcessImpl asyncBound

asyncOnCurProcessImpl :: (IO a -> IO (Async a)) -> IO a -> IO (Async a)
asyncOnCurProcessImpl doAsync action = do
  profEnabled >>= \case
    False -> doAsync action
    True  -> bracket start stop run
  where
    start = do
      getCurrentPid >>= \case
        Just pid -> pure pid
        Nothing  -> getPidForProcessName "UNKNOWN PROCESS"

    run pid = doAsync $ withPid pid $ action

    stop _ = pure ()

-- -----------------------------------------------------------------------

data FlowDirection
  = FlowNoDirection
  | FlowRequest
  | FlowResponse
  deriving (Show)

-- A Flow is Maybe a causal link between events separated across time on
-- different threads.
data Flow
  = FlowDisabled  -- Used when the profiling system is disabled
  | Flow FlowDirection ByteString Cat MicroTime FlowId   -- Used when enabled.
  deriving (Show)

-- Next flow ID. Empty whenever profiling disabled. Separate from `vProfSys`
-- because flows are allocated almost entirely in STM contexts.
vNextFlow :: TMVar Int
vNextFlow = unsafePerformIO newEmptyTMVarIO

rawAllocateFlow :: FlowDirection -> ByteString -> Cat -> STM Flow
rawAllocateFlow direction name cat = tryReadTMVar vNextFlow >>= \case
  Nothing -> pure FlowDisabled
  Just nextFlow -> do
    -- Even though we have access to the time of the event which started this
    -- flow, we need to record the exact start time here because it is a hard
    -- requirement that start < end time and I've seen this not hold when
    -- doing something which spawns an async which runs immediately.
    now <- unsafeIOToSTM $ getPOSIXTime
    let flowStartTime = round (1000000*now) :: Word64
    writeTMVar vNextFlow $! nextFlow + 1
    pure $ Flow direction name cat flowStartTime $ FlowId nextFlow

allocateFlow :: ByteString -> Cat -> STM Flow
allocateFlow = rawAllocateFlow FlowNoDirection

allocateRequestFlow :: ByteString -> Cat -> STM Flow
allocateRequestFlow = rawAllocateFlow FlowRequest

allocateResponseFlow :: Flow -> STM Flow
allocateResponseFlow FlowDisabled = pure FlowDisabled
allocateResponseFlow (Flow _ flowName flowCat _ _) =
  rawAllocateFlow FlowResponse flowName flowCat

popEventWithFlows :: MonadIO m => Args -> [Flow] -> [Flow] -> [Flow] -> m ()
popEventWithFlows args startFlow stepFlow endFlow = do
  whenHasThreadData "popEventWithFlows" $ \PROF_SYS{..} tid ThreadData{..} -> do
    -- We only get the end time: unlike X_EV, we don't calculate a duration
    -- because we can never prune an event which has a flow attached.
    now <- liftIO $ getPOSIXTime
    let end = round (1000000*now) :: Word64

    StackEntry name cat start <-
      atomicModifyIORef' stack \(x:xs) -> (xs, x)

    -- Flow event creation must be "enclosed" by a B/E slice, so we can't use
    -- X_EV.
    liftIO $ K.writeQueue collectorQ $
      [B_EV name cat start pid tid] ++
      flowList startFlow FE_START Nothing pid tid ++
      -- +1 offset on start time because sometimes chrome://tracing with
      -- otherwise assign the flow to the previous slice when rounding means
      -- the previous slice's end time == this slice's start time.
      flowList stepFlow FE_STEP (Just $ start + 1) pid tid ++
      flowList endFlow FE_END (Just $ start + 1) pid tid ++
      [E_EV name cat end pid tid args]
  where
    flowList :: [Flow] -> FlowEventType -> Maybe MicroTime -> Pid -> Tid
             -> [ProfEvent]
    flowList flows flowType forcedTime pid tid = catMaybes $ map asEv flows
      where
        asEv FlowDisabled = Nothing
        asEv (Flow flowDirection flowName flowCat flowStartTime flowId) =
          let time = case forcedTime of
                Nothing -> flowStartTime
                Just t  -> t
              name = case flowDirection of
                FlowNoDirection -> flowName
                FlowRequest     -> "Request " <> flowName
                FlowResponse    -> "Response " <> flowName
              cat = case flowDirection of
                FlowNoDirection -> flowCat
                FlowRequest     -> flowCat <> " request"
                FlowResponse    -> flowCat <> " response"
          in Just $ F_EV name cat flowId flowType time pid tid

-- | Main tracing method which keeps track of flows.
--
-- With this tracing variant, flows which this event ends are passed in, while
-- the callback function must provide a list of the started flows and the flows
-- which "step" here: flows which start somewhere else and end somewhere else
-- but pass through this event.
withTracingFlow :: MonadUnliftIO m
                => Name
                -> Cat
                -> Args
                -> [Flow]
                -> m ([Flow], [Flow], a)
                -> m a
withTracingFlow name cat args endFlows action = do
  mask $ \restore -> do
    startEvent name cat
    (startFlows, stepFlows, ret) <-
      restore action `onException`
        -- During an exception, we drop any startFlows that we might have
        -- created. This should just mean an arrow doesn't get drawn, which is
        -- better than an unbalanced call stack.
        popEventWithFlows args [] [] endFlows
    popEventWithFlows args startFlows stepFlows endFlows
    pure ret

-- | Like `withTracingFlow`, but everything returned by the
withTracingResultsFlow :: MonadUnliftIO m
                       => Name
                       -> Cat
                       -> m (Args, [Flow], [Flow], [Flow], a)
                       -> m a
withTracingResultsFlow name cat action = do
  mask $ \restore -> do
    startEvent name cat
    (args, startFlows, stepFlows, endFlows, ret) <-
      restore action `onException`
        popEventWithFlows mempty [] [] []
    popEventWithFlows args startFlows stepFlows endFlows
    pure ret

-- | In `Evaluator`, we have a thread which can time out and we need to set the
-- flows in such a way that we will still set flows while the stack
-- unwinds. But also, we need to create the new flows after we have started the
-- event. And because this is on a thread which is canceled, we have to be
-- exception safe.
withBuildEndFlows :: MonadUnliftIO m
                  => Name
                  -> Cat
                  -> [Flow]
                  -> [m Flow]
                  -> m a
                  -> m a
withBuildEndFlows name cat endFlows startFlowActions action = do
  mask $ \restore -> do
    startEvent name cat
    ret <- restore action `onException` do
      startFlows <- sequence startFlowActions
      popEventWithFlows mempty startFlows [] endFlows
    startFlows <- sequence startFlowActions
    popEventWithFlows mempty startFlows [] endFlows
    pure ret

recordInstantEvent :: MonadUnliftIO m
                   => Name
                   -> Cat
                   -> Args
                   -> m ()
recordInstantEvent name cat args = do
  whenHasThreadData "recordInstantEvent" $ \PROF_SYS{..} tid ThreadData{..} -> do
    now <- liftIO $ getPOSIXTime
    let start = round (1000000*now) :: Word64
    liftIO $ K.writeQueue collectorQ [I_EV name cat start pid tid args]

data ThreadNamePoolData = THREAD_NAME_POOL_DATA {
  next      :: Int,
  available :: [Int]
  }

data ThreadNamePool
  -- When profiling is disabled
  = TNPDisabled
  | TNP {
      prefix :: Text,
      dat    :: IORef ThreadNamePoolData
      }

mkThreadNamePool :: Text -> IO ThreadNamePool
mkThreadNamePool namePrefix = profEnabled >>= \case
  False -> pure TNPDisabled
  True  -> TNP <$> pure namePrefix <*> newIORef (THREAD_NAME_POOL_DATA 1 [])

withThreadNamePool :: ThreadNamePool -> IO a -> IO a
withThreadNamePool pool action = case pool of
  TNPDisabled     -> action
  TNP{prefix,dat} -> bracket start stop run
    where
      start = atomicModifyIORef' dat \THREAD_NAME_POOL_DATA{next,available} ->
        case available of
          []     -> (THREAD_NAME_POOL_DATA{next=next+1, available},
                     (next, True))
          (x:xs) -> (THREAD_NAME_POOL_DATA{next, available=xs}, (x, False))

      run (num, allocated) =
        withThreadName (encodeUtf8 $ prefix <> " #" <> tshow num) do
          when allocated $ do
            setThreadSortIndex num
          action

      stop (num, _) = atomicModifyIORef' dat
        \THREAD_NAME_POOL_DATA{next,available} ->
          (THREAD_NAME_POOL_DATA{next, available = sort (num:available)}, ())
