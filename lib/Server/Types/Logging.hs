-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE Strict #-}

module Server.Types.Logging where

import PlunderPrelude

import Fan         (Fan)
import Server.Time

--------------------------------------------------------------------------------

-- | A numeric identifier for a cog within a single machine.
newtype CogId = COG_ID { int :: Word64 }
  deriving newtype (Eq, Show, Ord)

-- | A positional index into the machine's Request vector.
newtype RequestIdx = RequestIdx { int :: Int }
  deriving newtype (Eq, Show, Ord)

-- | An identifier for a specific tell instance. Asks need to point back to a
newtype TellId = TellId { int :: Int }
  deriving newtype (Eq, Show, Ord)

-- | A record of one call to a Process and its side effects.
data ReceiptItem
  -- | Receipt of a normal Eval that completed successfully. Since everything
  -- is deterministic, the eval can just be replayed for its value during
  -- replay.
  = ReceiptEvalOK

  -- | Receipt of anything else.
  | ReceiptVal Fan

  -- | Receipt of a serve. {reqIdx} points back to the requesting
  -- cause. `(sender, serveId)` should form a unique identifier for the return
  -- value to be consumed in ReceiptRequest.
  | ReceiptTell { asker :: CogId, reqIdx :: RequestIdx, tellId :: TellId }

  -- | Receipt of a request getting fulfilled from a serve.
  | ReceiptAsk { tellId :: TellId }

  -- | Receipt of a spin. Contains the assigned cog id.
  | ReceiptSpun { cogNum :: CogId }

  -- | Receipt of a reap.
  | ReceiptReap { cogNum :: CogId }

  -- | Receipt of a cog stop.
  | ReceiptStop { cogNum :: CogId }
  deriving (Eq, Ord, Show)

data Receipt
    = RECEIPT_OK { cogNum :: CogId, inputs :: IntMap ReceiptItem }
    | RECEIPT_CRASHED { cogNum :: CogId, op :: Nat, arg :: Fan }
    | RECEIPT_TIME_OUT { cogNum :: CogId, timeoutAmount :: NanoTime }
  deriving (Show)

data CogFailure
    = COG_DOUBLE_CRASH
    | INVALID_COGID_IN_LOGBATCH
    | INVALID_TIMEOUT_IN_LOGBATCH
    | INVALID_CRASHED_IN_LOGBATCH
    | INVALID_OK_RECEIPT_IN_LOGBATCH
    | INVALID_SPUN_RECEIPT_IN_LOGBATCH CogId
    | INVALID_TELL_RECEIPT_IN_LOGBATCH
    | INVALID_ASK_RECEIPT_IN_LOGBATCH
    | INVALID_REAP_RECEIPT_IN_LOGBATCH
    | INVALID_STOP_RECEIPT_IN_LOGBATCH
  deriving (Eq, Ord, Show, Generic, Exception)

-- | Log batches count up from 0.
newtype BatchNum = BatchNum { unBatchNum :: Nat }
  deriving newtype (Eq, Show, Num)

-- | The atomic unit written to the event log. A Plunder Cog should be able to
-- be restored from a snapshot and a sequence of LogBatches which are an
-- execution on top of it. A Cog starts as a snapshot of its initial state and
-- has batches written on top of it, until a new snapshot is written that
-- corresponds to the state after a given LogBatch has been applied.
data LogBatch = LogBatch {
  -- | Monotonically increasing id.
  batchNum  :: BatchNum,

  -- | Time the LogBatch was written.
  writeTime :: NanoTime,

  -- | Events which were run this batch. If there's a snapshot in this LogBatch,
  -- these are run on top of the snapshot state.
  executed  :: [Receipt]
  }
  deriving (Show)

-- -----------------------------------------------------------------------

-- | Whether a cog is spinning, or in an error state.
data CogState
  = CG_SPINNING Fan
  | CG_FINISHED Fan
  | CG_CRASHED { op :: Nat, arg :: Fan, final :: Fan }
  | CG_TIMEOUT { duration :: NanoTime, final :: Fan }
  deriving (Show)

cogSpinningFun :: CogState -> Maybe Fan
cogSpinningFun (CG_SPINNING fan) = Just fan
cogSpinningFun CG_FINISHED{}     = Nothing
cogSpinningFun CG_CRASHED{}      = Nothing
cogSpinningFun CG_TIMEOUT{}      = Nothing

-- -----------------------------------------------------------------------

-- Interface to the logging system.

data LoadCog
  = NewCog
  | ExistingCog BatchNum

-- We always replay on top of a snapshot, where the initial state of the system
-- is the snapshot to the "0th LogBatch".
data ReplayFrom
  -- | Play from the earliest snapshot available.
  = EarliestSnapshot
  -- | Replay from the latest snapshot available.
  | LatestSnapshot
