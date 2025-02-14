---Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -freverse-errors #-}

{-# OPTIONS_GHC -Wall   #-}
module Server.Proc
    ( spawnProc
    , Eff(..)
    , DeviceName(..)
    , ResponseTuple(..)
    )
where
import PlunderPrelude
import Control.Monad.State   (execStateT, get)
import Fan                   (Fan(..), PrimopCrash(..), (%%))
import Server.Convert        ()

import Fan.Convert
import Fan.Prof
import Server.Debug
import Server.Evaluator
import Server.Time
import Server.Types.Logging

import qualified Data.Vector as V

--------------------------------------------------------------------------------

thirtySecondsInMicroseconds :: Nat
thirtySecondsInMicroseconds = 30 * 10 ^ (6::Int)

  
data EvalCancelledError = EVAL_CANCELLED
  deriving (Exception, Show)

newtype DeviceName = DEV_NAME { nat :: Nat }
  deriving newtype (Eq, Ord, FromNoun, ToNoun, IsString)

instance Show DeviceName where
    show nam =
        case natUtf8 nam.nat of
            Left _  -> show nam
            Right t -> show t

data Eff = EFF
  { reqIdx  :: RequestIdx
  , fan     :: Vector Fan
  , respond :: forall a. ToNoun a => a -> STM ()
  , cause   :: Flow
  }

instance Show Eff where
  show EFF{reqIdx,fan} = "EFF{idx=" <> show reqIdx <> ", fan=" <> show fan <> "}"

type Outbox = Eff -> STM [Flow]

-- | Data used only by the Runner async. This is all the data needed to
-- run the main thread of Fan evaluation and start Requests that it made.
data Runner = RUNNER
    { initProc :: Fan          -- ^ Starting value
    , currProc :: Fan          -- ^ Current value
    , call     :: Outbox
    , inbox    :: TQueue ResponseTuple
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

spawnProc :: Debug => Text -> Fan -> Outbox -> IO (Async (), ResponseTuple -> STM ())
spawnProc procName initProc call = do
    inbox <- atomically newTQueue
    thread <- asyncOnCurProcess $ withThreadName "Foo"
      $ handle (onErr $ "runner " <> procName)
      $ runnerFun procName RUNNER{currProc=initProc, ..}
    pure (thread, writeTQueue inbox)
  where
    onErr name e = do
      debugText $ name <> " thread was killed by: " <> pack (displayException e)
      throwIO (e :: SomeException)

drainNonEmptyTQueue :: TQueue a -> STM [a]
drainNonEmptyTQueue queue = isEmptyTQueue queue >>= \case
  True -> singleton <$> readTQueue queue
  False -> flushTQueue queue

-- The Proc Runner --------------------------------------------------------------

runnerFun :: Debug => Text -> Runner -> IO ()
runnerFun processName runner = do
        -- Process the initial syscall vector
        atomically $ launchRequests runner

        -- Run the event loop until we're forced to runnerop.
        procTick runner
  where
    procTick :: Runner -> IO ()
    procTick st = do
      inputs <- withAlwaysTrace "WaitForReponse" "proc" $
                 atomically $ drainNonEmptyTQueue st.inbox
      st' <- withProcessName (encodeUtf8 processName) $
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
runResponse st@RUNNER{initProc, currProc} rt = flip execStateT st do
    whenJust rt.resp \input -> do
      (_, result) <- lift $ withAlwaysTrace "Eval" "proc" $
        evalWithTimeout thirtySecondsInMicroseconds []
          (currProc %% toNoun rt.key) (toNoun input)

      assign' #currProc case result of
        OKAY _ resultFan -> resultFan
        _                -> initProc

      get >>= lift . atomically . launchRequests

launchRequests :: Debug => Runner -> STM ()
launchRequests RUNNER{..} = for_ reqs \(reqIdx,fan) -> do
    void $ call EFF
      { cause = FlowDisabled
      , respond = \a -> writeTQueue inbox $ RTUP reqIdx $ Just $ toNoun a
      , .. }
  where
    reqs :: Vector (RequestIdx, Vector Fan)
    reqs = fromMaybe mempty do
      KLO _ xs <- Just currProc
      let len = sizeofSmallArray xs
      ROW reqNouns <- Just $ xs .! (len-1)
      fmap V.fromArray $ for reqNouns \v -> do
        (idx, ROW eff) <- fromNoun v
        Just (RequestIdx idx, V.fromArray eff)

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
