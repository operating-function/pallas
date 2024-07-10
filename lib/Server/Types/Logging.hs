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

-- TODO we might actually want to keep CogId in the log but just always set it to 0,
-- for forwards compatibility with a multi-cog world.

newtype ProcId = PROC_ID Word64 -- TODO should maybe be generic workerid?
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

  -- | Receipt of an input from a Proc.
  | ReceiptVal Fan -- TODO should maybe be a list/array? and maybe not have ProcId? do we track that outside of this?
  deriving (Eq, Ord, Show)

data Receipt
    = RECEIPT_OK { inputs :: IntMap ReceiptItem }
    | RECEIPT_CRASHED { op :: Nat, arg :: Fan } -- TODO is this necessary?
    | RECEIPT_TIME_OUT { timeoutAmount :: NanoTime }
  deriving (Show)

data CogFailure
    = COG_DOUBLE_CRASH
    | INVALID_COGID_IN_LOGBATCH
    | INVALID_TIMEOUT_IN_LOGBATCH
    | INVALID_CRASHED_IN_LOGBATCH
    | INVALID_OK_RECEIPT_IN_LOGBATCH
    | INVALID_TELL_RECEIPT_IN_LOGBATCH
    | INVALID_ASK_RECEIPT_IN_LOGBATCH
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
