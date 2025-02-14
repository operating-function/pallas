-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.Hardware.Types
    ( callHardware
    , Cancel(..)
    , Device(..)
    , DeviceName(..)
    , DeviceTable(..)
    , SysCall(..)
    , writeResponse
    , fillInvalidSyscall
    , syscallCategory
    , describeSyscall
    )
where

import Fan.Convert
import PlunderPrelude

import Fan      (Fan)
import Fan.Prof (Flow) -- allocateResponseFlow)
import Server.Evaluator (Cancel(..))
import Server.Proc
import Server.Types.Logging (ProcId)

--------------------------------------------------------------------------------

data SysCall = SYSCALL
    { dev    :: DeviceName
    , args   :: Vector Fan
    , cause  :: Flow
    , effect :: Eff
    }
  deriving Show

data Device = DEVICE
    { start    :: ProcId -> IO ()
    , call     :: ProcId -> SysCall -> STM [Flow]
    , stop     :: ProcId -> IO ()
    , category :: Vector Fan -> Text
    , describe :: Vector Fan -> Text
    }

-- | All Hardware Devices
newtype DeviceTable = DEVICE_TABLE { table :: Map Nat Device }


-- Device Utilities ------------------------------------------------------------

{-
    Fills in the response of a syscall.

    If the response has been consumed, it does nothing.

    If the syscall already has a response, but it has not been consumed,
    it is replaced by the new value.

    Does not block.
-}
writeResponse :: ∀a. ToNoun a => SysCall -> a -> STM ()
writeResponse SYSCALL{effect=EFF{respond}} = respond
-- do
--   responseFlow <- allocateResponseFlow call.cause
--   rawWriteResponse call response responseFlow
--   pure responseFlow

{-
    Fills in the response with the empty value and sets its cause as the
    invalid sender; if a cog makes an invalid syscall, the hardware shouldn't
    be considered in the loop.
-}
fillInvalidSyscall :: SysCall -> STM ()
fillInvalidSyscall call = writeResponse call ()

-- rawWriteResponse :: ∀a. ToNoun a => SysCall -> a -> Flow -> STM ()
-- rawWriteResponse call response flow = do
--     readTVar call.state.var >>= \case
--         DEAD     -> pass
--         DONE _ _ -> writeTVar call.state.var $ DONE (toNoun response) flow
--         LIVE     -> writeTVar call.state.var $ DONE (toNoun response) flow
--
-- getCallResponse :: SysCall -> STM (Maybe (Fan, Flow))
-- getCallResponse call = readTVar call.state.var >>= \case
--     DEAD     -> error "Response consumed twice"
--     LIVE     -> pure Nothing
--     DONE x f -> writeTVar call.state.var DEAD $> Just (x, f)

-- TODO: Make this just echo back the incoming Flow so the invalid call
-- directly flows to the invalid 0 response.
callHardware :: DeviceTable -> ProcId -> SysCall -> STM [Flow]
callHardware db idx call = do
    case lookup call.dev.nat db.table of -- TODO should be array index
        Just device -> device.call idx call
        Nothing     -> do
          -- In the case of unrecognized hardware, immediately send
          -- back a 0 response.
          traceM ("no such device: " <> show call.dev)
          fillInvalidSyscall call
          pure []

syscallCategory :: DeviceTable -> DeviceName -> Vector Fan -> Text
syscallCategory db deviceName params = do
    case lookup deviceName.nat db.table of
        Just device -> device.category params
        Nothing     -> "_"

describeSyscall :: DeviceTable -> DeviceName -> Vector Fan -> Text
describeSyscall db deviceName params = do
    case lookup deviceName.nat db.table of
        Just device -> device.describe params
        Nothing     -> "_"
