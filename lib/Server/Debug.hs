-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Server.Debug
    ( debug
    , debugVal
    , debugFan
    , debugFanVal
    , debugText
    , debugTextVal
    , withCogDebugging
    , withDebugOutput
    , Debug
    )
where

import PlunderPrelude

import Data.Time.Format.ISO8601 (iso8601Show)
import Fan.Convert              (ToNoun(toNoun))
import Fan.Eval                 (Fan)
import Loot.ReplExe             (printValue)
import Rex                      (colorsOnlyInTerminal)


--------------------------------------------------------------------------------

type Debug = (?debugOut :: Maybe (Maybe Text, Fan) -> IO ())

debug :: (Debug, ToNoun a, MonadIO m) => a -> m ()
debug x = do
    fan <- (evaluate $ force $ toNoun x)
    liftIO $ ?debugOut (Just (Nothing, fan))

debugText :: (Debug, MonadIO m) => Text -> m ()
debugText = debug

debugTextVal :: (Debug, MonadIO m) => Text -> Text -> m ()
debugTextVal = debugVal

debugVal :: (Debug, ToNoun a, MonadIO m) => Text -> a -> m ()
debugVal nam x = do
    fan <- (evaluate $ force $ toNoun x)
    liftIO $ ?debugOut (Just (Just nam, fan))

debugFan :: (Debug, MonadIO m) => Fan -> m ()
debugFan = debug

debugFanVal :: (Debug, MonadIO m) => Text -> Fan -> m ()
debugFanVal = debugVal

withCogDebugging :: Debug => (Debug => IO a) -> IO a
withCogDebugging act = do
    let q Nothing = do
            ?debugOut Nothing
        q (Just (var, val)) = do
            now <- getCurrentTime
            ?debugOut $ Just
                      $ (var,)
                      $ toNoun (pack (iso8601Show now) :: Text, val)
    (let ?debugOut = q in act)

withDebugOutput :: ∀a. (Debug => IO a) -> IO a
withDebugOutput act =
    colorsOnlyInTerminal do
        q <- newTQueueIO

        let loop = atomically (readTQueue q) >>= \case
                       Nothing -> do
                           pure ()
                       Just (nam, fan) -> do
                           printValue stderr True (utf8Nat <$> nam) fan
                           loop

        tid <- async loop

        let out = atomically . writeTQueue q

        finally (let ?debugOut = out in (act :: IO a)) do
            -- Politely ask it to die once it is done printing.
            atomically (writeTQueue q Nothing)
            wait tid
