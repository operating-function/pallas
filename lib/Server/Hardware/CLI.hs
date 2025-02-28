-- Copyright 2025 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE Strict     #-}

{-
    This is the CLI options hardware.
    It provides access to command-line options passed to the runtime.

    Implementation
    ==============

    CLI options are specified with +OPTS and -OPTS markers at startup.
    For example: `plunder ... +OPTS --foo bar -OPTS ...`

    The hardware device provides access to these options via the CLI_OPT syscall.
    It takes a `Bar` as input and returns `Maybe Bar`.

    If the option was specified, it returns `Just value`.
    If the option was not specified, it returns `Nothing`.
-}

module Server.Hardware.CLI (createHardwareCLI) where

import Data.Acquire
import Fan.Convert ()  -- For ToNoun instance
import Fan.Eval
import Fan.Prof (Flow)
import PlunderPrelude
import Server.Hardware.Types

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

data HWState = HW_STATE
    { cliOptions :: Map ByteString ByteString }

-- | Create a new CLI hardware device
createHardwareCLI :: Map ByteString ByteString -> Acquire Device
createHardwareCLI options = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { start = const pass
        , stop = const pass
        , call = const $ runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
  where
    startup :: IO HWState
    startup = pure $ HW_STATE options

    shutdown :: HWState -> IO ()
    shutdown = const pass

runSysCall :: HWState -> SysCall -> STM [Flow]
runSysCall st syscall = do
    fromMaybe (fillInvalidSyscall syscall $> []) $ do
      decodeRequest syscall.args <&> \case
        OPT key -> onOpt syscall st key

decodeRequest :: Vector Fan -> Maybe CLIRequest
decodeRequest = toList <&> \case
    [NAT "opt", BAR key] -> Just $ OPT key
    _                    -> Nothing

data CLIRequest
    = OPT ByteString

onOpt :: SysCall -> HWState -> ByteString -> STM [Flow]
onOpt syscall st key = do
    writeResponse syscall (Map.lookup key st.cliOptions)
    pure []

categoryCall :: Vector Fan -> Text
categoryCall args = case toList args of
  [NAT "opt", BAR _] -> "%cli %opt"
  _                  -> "%cli UNKNOWN"

describeCall :: Vector Fan -> Text
describeCall args = case toList args of
  [NAT "opt", BAR k] -> "%cli %opt " <> decodeUtf8 k
  _                  -> "%cli UNKNOWN"