-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}

module Server.Evaluator
    ( Evaluator(..)
    , EvalRequest(..)
    , Evaluation(..)
    , EvalOutcome(..)
    , EvalState
    , evaluator
    , getEvalOutcome
    , pleaseEvaluate
    )
where

import Data.Acquire
import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Common
import Server.Debug
import Server.Hardware.Types (Cancel(CANCEL))
import Server.Time
import Server.Types.Logging

import Control.Concurrent (threadDelay)

import qualified Data.Vector as V

-- Types -----------------------------------------------------------------------

data EvalOutcome
    = OKAY NanoTime Fan -- ^ completed successfully
    | CRASH Nat Fan     -- ^ crashed with an error
    | TIMEOUT           -- ^ canceled by timeout
  deriving (Eq, Ord, Show, Generic, NFData)

data EvalState
    = ONGOING               -- ^ The evaluation is still active.
    | DONE EvalOutcome Flow -- ^ The evaluation succeeded.
    | CANCELED              -- ^ The cog canceled the evaluation.
    | CONSUMED              -- ^ The result has been consumed by the cog.
  deriving (Show)

type EvalStateVar = TVar EvalState

data Evaluator = EVALUATOR
    { workers :: Vector (Async Void)
    , pending :: TVar (Pool (EvalRequest, EvalStateVar))
    }

data EvalRequest = EVAL_REQUEST
    { cogId     :: CogId
    , flow      :: Flow
    , timeoutMs :: Nat
    , func      :: Fan
    , args      :: Vector Fan
    }
  deriving Show

data Evaluation = EVALUATION
    { request :: EvalRequest
    , state   :: TVar EvalState
    , cancel  :: Cancel
    }


-- Utilities -------------------------------------------------------------------

getEvalOutcome :: Evaluation -> STM (Maybe (EvalOutcome, Flow))
getEvalOutcome eva = do
    readTVar eva.state >>= \case
        ONGOING  -> pure Nothing
        CANCELED -> error "Outcome taken after cancel"
        CONSUMED -> error "Outcome taken twice"
        DONE ot flow  -> do
            writeTVar eva.state CONSUMED
            pure (Just (ot, flow))


-- The Evaluator ---------------------------------------------------------------

pleaseEvaluate :: Evaluator -> EvalRequest -> STM Evaluation
pleaseEvaluate st request = do
    state <- newTVar ONGOING
    key <- poolRegister st.pending (request, state)
    pure EVALUATION{request,state,cancel=CANCEL (onCancel key state)}
  where
    onCancel key eSt = do
        poolUnregister st.pending key
        writeTVar eSt CANCELED

runWorker :: Debug => Evaluator -> Int -> Maybe Tid -> IO Void
runWorker st _workerId tid = do
    forever step
  where
    takeWork :: STM (EvalRequest, EvalStateVar)
    takeWork = poolTakeNext st.pending pure

    calcResult req = do
        let exe = (withCalcRuntime . evaluate . force)
                $ (foldl' (%%) req.func req.args)

        try exe >>= \case
                Right (work, val)          -> pure (OKAY work val)
                Left (PRIMOP_CRASH op err) -> pure (CRASH op err)

    step :: IO ()
    step = do
        (req, var) <- atomically takeWork

        vResult       <- newEmptyTMVarIO
        vRespFlow <- newEmptyTMVarIO
        vTimeout      <- newTVarIO False

        let workName = ("Work: (" <>
                        (encodeUtf8 $ tshow req.cogId.int) <> ")")

        execTid <- async $ withCopiedTid tid $ do
          -- Even if we timeout or crash, we always need a response flow to
          -- attach to the response.
          let mkRespFlow = atomically do
                             respFlow <- allocateResponseFlow req.flow
                             putTMVar vRespFlow respFlow
                             pure respFlow

          withBuildEndFlows workName "eval" [req.flow] [mkRespFlow] do
            result <- calcResult req
            atomically (putTMVar vResult result)

        timeTid <- async do
            let microSecs = req.timeoutMs * 1000
            let usInt = fromIntegral microSecs
            when (fromIntegral usInt == microSecs) do -- Check for overflow
                threadDelay usInt
                atomically (writeTVar vTimeout True)

        atomically do
            mResult     <- tryReadTMVar vResult
            f           <- fromMaybe FlowDisabled <$> tryReadTMVar vRespFlow
            timeoutFlag <- readTVar vTimeout
            evalSt      <- readTVar var
            case (mResult, timeoutFlag, evalSt) of
                (Nothing,     False, ONGOING) -> retry
                (Just result, _,     ONGOING) -> writeTVar var (DONE result f)
                (_,           True,  ONGOING) -> writeTVar var (DONE TIMEOUT f)
                (_,           _,     _      ) -> pure ()

        cancel execTid
        cancel timeTid

evaluator :: Debug => Int -> Acquire Evaluator
evaluator numWorkers = do
    mkAcquire mk release
  where
    release eva = do
        traverse_ cancel eva.workers

    execWorker ~st workerId = do
       asyncOnCurProcess do
           let threadName = "Worker-" <> encodeUtf8 (tshow workerId)
           withThreadName threadName do
               tid <- getCurrentTid
               runWorker st workerId tid

    mk = do
        res <- withProcessName "Eval" mdo
            pending <- newTVarIO emptyPool
            workers <- V.generateM numWorkers (execWorker st . succ)
            let ~st = EVALUATOR{workers,pending}
            pure st
        pure res
