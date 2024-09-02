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
    ( spawnProc, Eff(..), EffState(..), CallStateVar(..), DeviceName(..), ResponseTuple(..), kill )
where

import PlunderPrelude

import Control.Monad.State   (execStateT, modify, get)
import Fan                   (Fan(..), PrimopCrash(..), (%%))
-- import Optics                (set)
import Server.Convert        ()

import Fan.Convert
import Fan.Prof
import Server.Debug
import Server.Evaluator
-- import Server.Hardware.Types
import Server.Time
import Server.Types.Logging

import qualified Data.IntMap as IM
-- import qualified Data.Map    as M
import qualified Data.Vector as V

--------------------------------------------------------------------------------

thirtySecondsInMicroseconds :: Nat
thirtySecondsInMicroseconds = 30 * 10 ^ (6::Int)

getCurrentReqNoun :: Fan -> Vector Fan
getCurrentReqNoun = \case
  KLO _ xs ->
      let len = sizeofSmallArray xs in
      case (xs .! (len-1)) of
          ROW x -> V.fromArray x
          _     -> mempty
  _ -> mempty
  
data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

data EffState
    = LIVE           -- ^ The call has not yet returned
    | DONE Fan Flow  -- ^ The call returned a value not yet processed.
    | DEAD           -- ^ Won't return (already processed, invalid, or nonreturning).
  deriving Show

newtype CallStateVar = STVAR { var :: TVar EffState }

newtype DeviceName = DEV_NAME { nat :: Nat }
  deriving newtype (Eq, Ord, FromNoun, ToNoun, IsString)

instance Show DeviceName where
    show nam =
        case natUtf8 nam.nat of
            Left _  -> show nam
            Right t -> show t

instance Show CallStateVar where
    show = const "CallStateVar"

kill :: Eff -> STM ()
kill eff = writeTVar eff.state.var DEAD

-- | A list of parsed out valid requests from `noun`. For every proc, for every
-- index in that proc's requests table, there is a raw fan value and a
-- `LiveRequest` which contains STM variables to listen
type LiveEffs = IntMap LiveEff

data Eff = EFF
  { reqIdx :: RequestIdx
  , fan    :: Vector Fan
  , state  :: CallStateVar
  , cause  :: Flow
  }

instance Show Eff where
  show EFF{reqIdx,fan} = "EFF{idx=" <> show reqIdx <> ", fan=" <> show fan <> "}"

type Outbox = Eff -> STM (Cancel, [Flow])

data LiveEff = LIVE_EFF
  { eff    :: Eff
  , cancel :: Cancel
  }

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { call  :: Outbox
    , init  :: Fan          -- ^ Starting value
    , proc  :: Fan          -- ^ Current value
    , reqs  :: LiveEffs    -- ^ Current requests table
    , inbox :: TQueue ResponseTuple
    }

-- -----------------------------------------------------------------------


data ResponseTuple = RTUP
    { key  :: RequestIdx
    , resp :: Maybe Fan
--  , work :: NanoTime
--  , flow :: Flow
    }
  deriving (Show)

makeFieldLabelsNoPrefix ''Runner

-- -----------------------------------------------------------------------
-- No template haskell beyond this point because optics.
-- -----------------------------------------------------------------------

spawnProc :: Debug => Fan -> Outbox -> IO (Async (), ResponseTuple -> STM ())
spawnProc proc call = do
    let init = proc
    let reqs = mempty
    inbox <- atomically newTQueue
    handle <- asyncOnCurProcess $ withThreadName "Foo"
      $ handle (onErr "runner foo")
      $ runnerFun "procname" RUNNER{..}
    pure (handle, writeTQueue inbox)
  where
    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

-- TODO it would be nice if we could flatten this into just STM ResponseTuple
-- and rely on the Alternative STM instance instead of using Maybe.
receiveResponse :: LiveEff -> STM (Maybe ResponseTuple)
receiveResponse LIVE_EFF{eff} = readTVar eff.state.var >>= \case
    DONE x _ -> Just RTUP{key=eff.reqIdx, resp=Just x} -- TODO Flow
                <$ kill eff
    LIVE     -> pure Nothing
    DEAD     -> pure $ Just RTUP{key=eff.reqIdx, resp=Nothing}

-- Design point: Why not use optics and StateT in Runner? Because StateT in IO
-- doesn't have a MonandUnliftIO instance, which means that we can't bracket
-- our calls to the profiling system, which you really want to do to keep
-- things exception safe. We thus only use it in the one super stateful method,
-- parseRequests.

-- The Proc Runner --------------------------------------------------------------

runnerFun :: Debug => ByteString -> Runner -> IO ()
runnerFun processName st = flip finally cancelOpenSyscalls do
        -- Process the initial syscall vector
        newReqs <- atomically $ parseRequests st

        -- Run the event loop until we're forced to stop.
        procTick st{reqs=newReqs}
  where
    -- TODO right idea but uses the old `st`. Need st.reqs to be a TVar
    -- (or preferably rearchitect in some way)
    cancelOpenSyscalls :: IO ()
    cancelOpenSyscalls = for_ st.reqs \LIVE_EFF{cancel} ->
      atomically cancel.action

    procTick :: Runner -> IO ()
    procTick st = do
      inputs <- withAlwaysTrace "WaitForReponse" "proc" $ atomically do
        responses <- mapMaybeA receiveResponse $ IM.elems st.reqs
        cogOutputs <- flushTQueue st.inbox
        guarded (not . null) $ responses <> cogOutputs
      st' <- withProcessName processName $
              withThreadName ("Proc: ") $
                foldM runResponse st inputs
      procTick st'

{-
    Given a set of responses to syscalls in a procs SysCall table, create
    a new event value and pass that into the proc, to get the new proc state.

    Side Effects:
    -   Any new requests are launched.

    Results:
    -   The PLAN value for the proc is replaced.
    -   The old request is deleted from the request table.
    -   The proc's requests table is updated to reflect the new set of
        requests.
-}
runResponse :: Debug => Runner -> ResponseTuple -> IO Runner
runResponse st@RUNNER{..} rt = flip execStateT st do
    modifying' #reqs (deleteMap rt.key.int)
    whenJust rt.resp \input -> do
      (_, result) <- lift $ withAlwaysTrace "Eval" "proc" $
        evalWithTimeout thirtySecondsInMicroseconds []
          (proc %% toNoun rt.key) (toNoun input)

      assign' #proc case result of
        OKAY _ resultFan -> resultFan
        _                -> init

      assign' #reqs =<< lift . atomically . parseRequests =<< get

-- | Update the requests in a runner according to the current proc.
-- TODO maybe just take the proc Fan value and give back a diff map?
parseRequests :: Debug => Runner -> STM LiveEffs
parseRequests RUNNER{..} =
    flip execStateT reqs $
      for_ (getCurrentReqNoun proc) \reqNoun ->
        whenJust (reqFromFan reqNoun) \(reqIdx, eff) -> do
          liveEff <- lift (buildLiveRequest call reqIdx eff)
          lift (readTVar liveEff.eff.state.var) >>= \case
            DEAD -> pass
            _    -> modify $ insertMap reqIdx.int liveEff
  where
    reqFromFan :: Fan -> Maybe (RequestIdx, Vector Fan)
    reqFromFan v = do
        (idx, ROW eff) <- fromNoun v
        Just (RequestIdx idx, V.fromArray eff)

-- | Given a Request parsed from the proc, turn it into a LiveRequest that can
-- produce a value and that we can listen to.
buildLiveRequest :: Debug => Outbox -> RequestIdx -> Vector Fan -> STM LiveEff
buildLiveRequest call reqIdx fan = do
    state <- STVAR <$> newTVar LIVE
    let eff = EFF reqIdx fan state FlowDisabled
    (cancel, _) <- call eff -- TODO flow
    pure LIVE_EFF{..}

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
