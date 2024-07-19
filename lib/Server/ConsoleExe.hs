-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}

module Server.ConsoleExe (main) where

import Data.Acquire
import Fan.Convert
import GHC.IO.Encoding
import Options.Applicative
import PlunderPrelude       hiding (Handler, handle)
import Server.Debug
import Server.Evaluator
import Server.Machine
import Server.Types.Logging
import System.Environment
import System.Posix.Signals hiding (Handler)
import System.Process

import Server.Hardware.Http  (createHardwareHttp)
import Server.Hardware.Port  (createHardwarePort)
import Server.Hardware.Rand  (createHardwareRand)
import Server.Hardware.Types (DeviceTable(..))
import System.Random         (randomIO)
-- ort Server.Hardware.Sock (createHardwareSock)
import Server.Hardware.Time (createHardwareTime)
-- ort Server.Hardware.Wock (createHardwareWock)
import Server.Hardware.Poke (createHardwarePoke)

import Control.Concurrent       (threadDelay)
import Control.Monad.State      (State, execState, modify')
import Data.Time.Format.ISO8601 (iso8601Show)
import Fan.Hash                 (fanHash)
import Hash256                  (hashToBTC)
import Loot.ReplExe             (showFan, trkFan, trkRex)
import Sire.Types               (trkRexM)
import System.Directory         (createDirectoryIfMissing, doesFileExist,
                                 getHomeDirectory, removeFile)
import System.Exit              (ExitCode(..), exitWith)
import System.IO.Error          (catchIOError)
import System.Posix.Types       (CPid(CPid))

import qualified Loot.ReplExe
import qualified Rex
import qualified Sire

import qualified Data.ByteString  as BS
import qualified Data.Char        as C
import qualified Fan              as F
import qualified Fan.Prof         as Prof
import qualified Fan.Seed         as F
import qualified Fan.Types        as F
import qualified Server.LmdbStore as DB

--------------------------------------------------------------------------------

safeDeleteFile :: FilePath -> IO ()
safeDeleteFile pax = catchIOError (removeFile pax) (const $ pure ())

getPidFile :: Debug => FilePath -> Acquire FilePath
getPidFile storeDir =
    mkAcquire start safeDeleteFile
  where
    start = do
        createDirectoryIfMissing True storeDir
        pid <- getCurrentPid
        let pax = (storeDir </> "pid")
        debugVal "pidfile" (pack pax :: Text)
        exists <- doesFileExist pax
        when exists $ do
           debugFan "pidfile_exists"
           pidTxt <- readFileUtf8 pax
           case readMay pidTxt of
               Nothing -> do
                   debug (["malformed_pidfile","overwriting"] :: [Text])
                   pure ()
               Just alien -> do
                   debug [ "found_existing_daemon"
                         , "killing_id" :: Text
                         ]

                   let killIt = do
                           signalProcess sigTERM alien
                           debugText "waiting_for_alien_shut_down"
                           loop 0
                       loop (1000::Int) = do
                           debugText "failed_to_kill_daemon"
                           exitWith (ExitFailure 1)
                       loop i = doesFileExist pax >>= \case
                           True  -> threadDelay 10_000 >> loop (i+1)
                           False -> pure ()

                   catchIOError killIt \exn ->
                       if isDoesNotExistError exn then
                           debugText "daemon_not_actually_running"
                       else
                           throwIO exn
                   debugFan "old_daemon_killed"

        debugFan "write_pidfile"
        writeFileUtf8 pax (tshow (coerce pid :: Int32))
        pure pax

withDirectoryWriteLock :: Debug => FilePath -> IO a -> IO a
withDirectoryWriteLock storeDir a =
    with (getPidFile storeDir) $ \_ -> a

--------------------------------------------------------------------------------

type Prof = Maybe FilePath

data ProfilingOpts = ProfilingOpts Prof Bool

data InterpreterOpts
  = InterpreterOpts Bool -- Warn on jet deopt
                    Bool -- Crash on jet deopt
                    Bool -- Crash on jet mistmatch

data MachineOpts
  = MachineOpts Bool -- snapshots enabled?
                Int  -- Number of EVAL workers

data RunType
    = RTSire FilePath
             ProfilingOpts
             InterpreterOpts
             [FilePath] -- SireFile
    | RTSave ProfilingOpts
             InterpreterOpts
             FilePath -- Seed file
             FilePath -- SireFile
    | RTShow FilePath
    | RTRepl FilePath FilePath InterpreterOpts
    | RTLoot FilePath ProfilingOpts [FilePath]
    | RTBoot ProfilingOpts InterpreterOpts MachineOpts Bool FilePath Text
    | RTUses FilePath Int
    | RTOpen FilePath CogId
    | RTTerm FilePath CogId
    -- TODO: Rename 'run' or 'spin' or 'crank' or something.
    | RTStart FilePath
              ProfilingOpts
              InterpreterOpts
              MachineOpts
              ReplayFrom
 -- | RTPoke FilePath Text FilePath

-- cogIdArg :: Parser CogId
-- cogIdArg = COG_ID <$> argument auto (metavar "COG" <> help helpTxt)
--   where
--     helpTxt = "The cog id number"

replayFromOption :: Parser ReplayFrom
replayFromOption =
    flag LatestSnapshot EarliestSnapshot
        ( long "replay-all"
       <> help "Replay log from beginning."
        )

bootHashArg :: Parser Text
bootHashArg = strArgument
    ( metavar "HASH"
   <> help "Boot using this sire file (or pin hash)"
    )

sireFile :: Parser FilePath
sireFile =
    strArgument (metavar "SIRE" <> help helpTxt)
  where
    helpTxt = "A sire file to load before launching the REPL"

seedFile :: Parser FilePath
seedFile =
    strArgument (metavar "SEED" <> help helpTxt)
  where
    helpTxt = "The seed file to write the result to"

-- lootFile :: Parser FilePath
-- lootFile =
--     strArgument (metavar "LOOT" <> help helpTxt)
--   where
--     helpTxt = "A loot file to load before starting the REPL"

plunderCmd :: String -> String -> Parser a -> Mod CommandFields a
plunderCmd cmd desc parser =
    command cmd (info (parser <**> helper) (progDesc desc))

runType :: FilePath -> Parser RunType
runType defaultDir = subparser
    ( plunderCmd "sire" "Run a standalone Sire repl."
      (RTSire <$> storeOpt
              <*> profilingOpts
              <*> interpreterOpts
              <*> many sireFile)

   -- <> plunderCmd "open" "Open a terminal's GUI interface."
   -- (RTOpen <$> storeOpt
   --          <*> cogIdArg)

   <> plunderCmd "save" "Load a sire file and save a seed."
      (RTSave <$> profilingOpts
              <*> interpreterOpts
              <*> seedFile
              <*> sireFile)

   <> plunderCmd "show" "Print a seed file."
      (RTShow <$> seedFile)

   <> plunderCmd "repl" "Interact with a seed file."
      (RTRepl <$> seedFile
              <*> replWriteOpt
              <*> interpreterOpts)

   <> plunderCmd "start" "Resume an idle machine."
      (RTStart <$> storeArg
               <*> profilingOpts
               <*> interpreterOpts
               <*> machineOpts
               <*> replayFromOption)

   -- <> plunderCmd "loot" "Run a standalone sire repl."
   -- (RTLoot <$> storeOpt <*> profilingOpts <*> many lootFile)

   <> plunderCmd "boot" "Boot a machine."
      (RTBoot <$> profilingOpts
              <*> interpreterOpts
              <*> machineOpts
              <*> startAtBoot
              <*> storeArg
              <*> bootHashArg)

   -- <> plunderCmd "du" "du -ab compatible output for pin state."
   --   (RTUses <$> storeArg <*> numWorkers)

   -- <> plunderCmd "poke" "Pokes a started cog with a value."
   --      -- TODO: should pokePath parse the '/' instead?
   --      (RTPoke <$> storeOpt <*> pokePath
   --              <*> pokeSire)
   )
  where
    -- pokePathHelp = help "Path to send data on"
    -- pokeSireHelp = help "Sire file to parse and send"
    storeHlp = help "Location of pallas data"
    profHelp = help "Where to output profile traces (JSON)"
    storeArg = strArgument (metavar "STORE" <> storeHlp)

    profilingOpts = ProfilingOpts <$> profOutput <*> profLaw
    interpreterOpts = InterpreterOpts <$> doptWarn <*> doptCrash <*> matchCrash
    machineOpts = MachineOpts <$> doSnap <*> numWorkers

    storeOpt =
        strOption ( long "store"
                 <> value defaultDir
                 <> short 'd'
                 <> metavar "STORE"
                 <> storeHlp
                  )

    -- pokePath = strArgument (metavar "PATH" <> pokePathHelp)
    -- pokeSire = strArgument (metavar "SIRE" <> pokeSireHelp)

    profLaw :: Parser Bool
    profLaw = switch ( short 'P'
                    <> long "profile-laws"
                    <> help "Include law-execution in profile traces."
                     )

    startAtBoot :: Parser Bool
    startAtBoot = switch ( long "start"
                        <> help "Immediate start the machine after boot" )

    doptWarn :: Parser Bool
    doptWarn = switch ( short 'f'
                     <> long "law-fallback-warn"
                     <> help ( "Print a warning when a jet falls back to raw"
                            <> "fan execution."
                             )
                      )

    doptCrash :: Parser Bool
    doptCrash = switch ( short 'F'
                      <> long "law-fallback-crash"
                      <> help "Crash when a jet falls back to raw fan execution."
                       )

    matchCrash :: Parser Bool
    matchCrash = switch ( short 'M'
                       <> long "jet-mismatch-crash"
                       <> help "Crash if a jet-match fails"
                        )

    doSnap :: Parser Bool
    doSnap = fmap not
           $ switch ( short 'S'
                    <> long "disable-snapshots"
                    <> help "Disable snapshots"
                     )


    profOutput =
        fmap (\x -> if null x then Nothing else Just x) $
        strOption ( long "profile-output"
                 <> value ""
                 <> short 'p'
                 <> metavar "PROF_FILE"
                 <> profHelp
                  )

    replWriteOpt =
        strOption ( long "save"
                 <> value "/dev/null"
                 <> metavar "OUTPUT_FILE"
                 <> help "Where to write the formal output"
                  )

    numWorkers =
        option auto ( long "eval-workers"
                   <> value 8
                   <> short 'w'
                   <> metavar "NUM_WORKERS"
                   <> help "Number of EVAL workers to use"
                    )

runInfo :: FilePath -> ParserInfo RunType
runInfo defaultDir =
    info (runType defaultDir <**> helper)
        ( fullDesc
       <> progDesc "Pallas"
       <> header "Run a Pallas machine"
        )

data BadPortsFile = BAD_PORTS_FILE Text FilePath Text
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

-- | Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main :: IO ()
main = do
  -- some systems require the following, even if their locale is already UTF-8
  setLocaleEncoding utf8

  Rex.colorsOnlyInTerminal do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    home <- getHomeDirectory
    ddir <- lookupEnv "PLUNDER_DIR" <&> maybe (home </> ".plunder") id
    args <- customExecParser
            (prefs (showHelpOnError <> showHelpOnEmpty <> noBacktrack))
            (runInfo ddir)

    withProfileOutput args $
      withInterpreterOpts args $
      withDebugOutput $
      case args of
        RTBoot _ _ mo start d y    -> do
            bootMachine d y
            when start $ do
              runMachine d EarliestSnapshot mo

        RTUses d w    -> duMachine d w
        RTShow fp     -> showSeed fp
        RTRepl fp o _ -> replSeed fp o
        RTOpen d cog  -> void (openBrowser d cog)
        RTTerm d cog  -> void (openTerminal d cog)

        RTLoot _ _ fz -> do
            liftIO $ Loot.ReplExe.replMain fz

        RTSire _ _ _ fz -> do
            code <- liftIO (Sire.main fz)
            exitWith code

        RTSave _ _ sd sr -> do
            saveSeed sd sr

        RTStart d _ _ mo r -> do
            runMachine d r mo

withProfileOutput :: RunType -> IO () -> IO ()
withProfileOutput args act = do
    case argsProf args of
        Just (ProfilingOpts (Just fil) laws) -> do
            putStrLn ("Profiling Output: " <> pack fil)
            Prof.withProfileOutput fil laws act
        _                                    -> act
  where
    argsProf = \case
        RTSire _ po _ _     -> Just po
        RTSave po _ _ _     -> Just po
        RTLoot _ po _       -> Just po
        RTShow _            -> Nothing
        RTRepl{}            -> Nothing
        RTOpen{}            -> Nothing
        RTTerm{}            -> Nothing
        RTStart _ po _ _ _  -> Just po
        RTUses{}            -> Nothing
        RTBoot po _ _ _ _ _ -> Just po
     -- RTPoke _ _ _ _      -> Nothing

withInterpreterOpts :: RunType -> IO () -> IO ()
withInterpreterOpts args act = do
    case argsInterpreter args of
        Just (InterpreterOpts j c m) -> do
            let onJetMismatch = if m then F.CRASH else F.WARN
            let onJetFallback = case (j, c) of (_, True) -> F.CRASH
                                               (True, _) -> F.WARN
                                               _         -> F.IGNORE
            writeIORef F.vRtsConfig $ F.RTS_CONFIG {..}
            act
        _ -> act
  where
    argsInterpreter = \case
        RTSire _ _ io _     -> Just io
        RTSave _ io _ _     -> Just io
        RTShow _            -> Nothing
        RTRepl _ _ io       -> Just io
        RTLoot _ _ _        -> Nothing
        RTBoot _ io _ _ _ _ -> Just io
        RTUses _ _          -> Nothing
        RTOpen _ _          -> Nothing
        RTTerm _ _          -> Nothing
        RTStart _ _ io _ _  -> Just io

bootMachine :: (Debug, Rex.RexColor) => FilePath -> Text -> IO ()
bootMachine storeDir pash = do
    withDirectoryWriteLock storeDir $ do
        let fil = unpack pash
        e <- liftIO (doesFileExist fil)
        unless e (error $ unpack ("File does not exist: " <> pash))
        val <- liftIO (Sire.loadFile fil)

        with (DB.openDatastore storeDir) $ \lmdb -> do
            DB.hasSnapshot lmdb >>= \case
                False -> do
                    firstCogId <- COG_ID <$> randomIO
                    DB.writeMachineSnapshot lmdb (BatchNum 0)
                                            (singletonMap firstCogId
                                             (CG_SPINNING val))
                True -> do
                    error "Trying to overwrite existing machine"

-- TODO: Output the result of an expression?  Not just "main"?
saveSeed :: (Debug, Rex.RexColor) => FilePath -> FilePath -> IO ()
saveSeed outFile inputFile = do
    val <- liftIO (Sire.loadFile inputFile)
    byt <- F.saveSeed val
    writeFile outFile byt

showSeed :: (Debug, Rex.RexColor) => FilePath -> IO ()
showSeed seedFileToShow = do
    writeIORef F.vShowFan showFan
    writeIORef F.vTrkFan  trkFan
    writeIORef F.vTrkRex  trkRex
    byt <- readFile seedFileToShow
    pin <- F.loadSeed byt >>= either throwIO pure
    print pin
    fullPrint pin
  where
    fullPrint x = trkRexM $ Sire.planRexFull $ toNoun x

-- TODO: If given something like $path.sire:main, load that instead of
-- just using a seed.
replSeed :: (Debug, Rex.RexColor) => FilePath -> FilePath -> IO ()
replSeed seedFileToShow outputFile = do
    let onJetFallback = F.WARN
    let onJetMismatch = F.WARN
    writeIORef F.vJetMatch           $! F.jetMatch
    writeIORef F.vShowFan            $! showFan
    writeIORef F.vTrkFan             $! trkFan
    writeIORef F.vTrkRex             $! trkRex
    writeIORef F.vRtsConfig          $! F.RTS_CONFIG{..}
    byt <- readFile seedFileToShow
    pin <- F.loadSeed byt >>= either throwIO pure
    withFile outputFile WriteMode \h ->
        interactive h pin
  where
    fullPrint x = trkRexM $ Sire.planRexFull $ toNoun x

    interactive h st0 = do
        input <- (try $ BS.hGetSome stdin 80) >>= \case
                     Left (e :: IOError) | isEOFError e -> pure mempty
                     Left (e :: IOError)                -> throwIO e
                     Right ln                           -> pure ln
        let result = (st0 F.%% F.BAR input)
        case fromNoun result of
            Nothing -> do
                fullPrint result
                error ("bad noun")
            Just (output, st1) -> do
                BS.hPutStr h output
                unless (null input) do
                    interactive h st1

-- pokeCog :: (Debug, Rex.RexColor) => FilePath -> Text -> FilePath
--         -> IO ()
-- pokeCog d c p pash = do
--   withDaemon d $ do
--       let fil = unpack pash
--       e <- liftIO (doesFileExist fil)
--       unless e (error $ unpack ("File does not exist: " <> pash))
--       mVl <- liftIO (Sire.ReplExe.loadFile fil)
--       val <- case mVl of
--                 Nothing -> (error . unpack) $
--                                ("No value at end of file : " <> pash)
--                 Just vl -> pure vl
--       reqPoke c (splitOn "/" p) (JELLY_PACK val)

shellFg :: String -> [String] -> IO ExitCode
shellFg c a = do
    let p = (proc c a) { std_in        = Inherit
                       , std_out       = Inherit
                       , std_err       = Inherit
                       , close_fds     = True
                       , delegate_ctlc = True
                       }
    (_, _, _, ph) <- createProcess p
    waitForProcess ph

openBrowser :: FilePath -> CogId -> IO ExitCode
openBrowser dir cogId = do
    let cogNm = tshow cogId.int
    let pax = (dir </> unpack (cogNm <> ".http.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve HTTP")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "http" pax cont)
                   Just pt -> pure pt
    let url = "http://localhost:" <> show port
    shellFg "xdg-open" [url]

openTerminal :: FilePath -> CogId -> IO ExitCode
openTerminal dir cogId = do
    let cogNm = tshow cogId.int
    let pax = (dir </> unpack (cogNm <> ".telnet.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve Telnet")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "telnet" pax cont)
                   Just pt -> pure pt
    shellFg "nc" ["localhost", show port]

-- {-
--     Deliver a noun from the outside to a given cog.
-- -}
-- doPoke :: Debug => ServerState -> [Text] -> JellyPack -> IO ()
-- doPoke st path pak = do
--     debug ["poke_cog"]
--     st.poke (fromList path) pak.fan

withMachineIn :: Debug
              => FilePath
              -> Int
              -> Bool
              -> (MachineContext -> IO a)
              -> IO a
withMachineIn storeDir numWorkers enableSnaps machineAction = do
  withDirectoryWriteLock storeDir do
    -- Setup plunder interpreter state.
    writeIORef F.vTrkFan $! \x -> do
        now <- getCurrentTime
        debug (["trk"::Text, pack (iso8601Show now)], x)

    writeIORef F.vShowFan  $! Loot.ReplExe.showFan
    writeIORef F.vJetMatch $! F.jetMatch

    -- TODO: Thing about all the shutdown signal behaviour. Right now, we're
    -- ignoring it, but there's a bunch of things the old system did to catch
    -- Ctrl-C.

    let devTable db hw_poke = do
            hw1_rand          <- createHardwareRand
          --(hw4_wock, wsApp) <- createHardwareWock
            let wsApp _cogId _ws = pure ()
            hw2_http          <- createHardwareHttp storeDir db wsApp
          --hw3_sock          <- createHardwareSock storeDir
            hw5_time          <- createHardwareTime
            hw6_port          <- createHardwarePort
            (pure . DEVICE_TABLE . mapFromList) $
                [ ( "rand", hw1_rand )
                , ( "http", hw2_http )
                --( "sock", hw3_sock )
                --( "wock", hw4_wock )
                , ( "time", hw5_time )
                , ( "port", hw6_port )
                , ( "poke", hw_poke  )
                ]

    let machineState = do
            lmdb <- DB.openDatastore storeDir
            (pokeHW, _submitPoke) <- createHardwarePoke
            hw <- devTable lmdb pokeHW
            eval <- evaluator numWorkers
            pure MACHINE_CONTEXT{lmdb,hw,eval,enableSnaps}
    with machineState machineAction

runMachine :: Debug => FilePath -> ReplayFrom -> MachineOpts -> IO ()
runMachine storeDir replayFrom (MachineOpts enableSnaps numWorkers) = do
    cache <- DB.CUSHION <$> newIORef mempty
    withMachineIn storeDir numWorkers enableSnaps $ \ctx -> do
        machine <- withCogDebugging $ replayAndCrankMachine cache ctx replayFrom

        -- Listen for Ctrl-C and external shutdown signals.
        termSignal <- newEmptyTMVarIO
        for_ [sigTERM, sigINT] $ \sig -> do
            installHandler sig
                           (Catch (atomically $ putTMVar termSignal ()))
                           Nothing

        c <- atomically $ (Left  <$> readTMVar termSignal <|>
                           Right <$> readTMVar machine.shutdownComplete)
        case c of
            Left () -> do
                -- We got an external shutdown signal, so shut down the machine
                -- and wait for things to sync to disk.
                shutdownMachine machine
            Right () -> do
                -- The machine exited on its own and we don't have to wait for
                -- its
                --
                -- TODO: Print a report here about the state of why the machine
                -- shutdown on its own.
                pure ()


duMachine :: Debug => FilePath -> Int -> IO ()
duMachine storeDir numWorkers = do
    withMachineIn storeDir numWorkers False $ \ctx -> do
        retLines <- walkNoun ctx
        forM_ retLines $ putStrLn
  where
    walkNoun :: MachineContext -> IO [Text]
    walkNoun ctx = do
      cache <- DB.CUSHION <$> newIORef mempty

      -- Replay the machine to get the current noun.
      (_, MOMENT noun _) <- performReplay cache ctx LatestSnapshot

      pin <- F.mkPin' (toNoun noun)
      (pins, _hed, blob) <- F.savePin pin

      pure $ execState (fanDu pins blob) []

    fanDu :: Vector F.Pin -> ByteString -> State [Text] ()
    fanDu refs blob = do
      refSize <- sum <$> mapM (pinDu []) refs
      _ <- calcEntry [] refSize blob
      pure ()

    pinDu :: [Text] -> F.Pin -> State [Text] Int
    pinDu path p = do
      error "TODO: This is more complex now" path
      error "We should probably store sizes and edge-lists in the database" p
      error "And this should be a read transaction" binName ok ugul
      -- let pinName = binName p.item
      -- refSize <- sum <$> mapM (pinDu (pinName : path)) p.refs
      -- calcEntry (pinName : path) refSize p.blob

    calcEntry :: [Text] -> Int -> ByteString -> State [Text] Int
    calcEntry path refSize blob = do
      let blobSize = BS.length blob
          totalSize = refSize + blobSize
      let displayPath :: Text = concat $ intersperse "/" $ reverse path
      modify' ((tshow totalSize <> " " <> displayPath):)
      pure totalSize

    -- TODO: Better way to identify grains that don't have law names?
    binName :: F.Fan -> Text
    binName = \case
        F.FUN law -> ugul law.name.nat
        F.PIN pin -> binName pin.item
        f         -> hashToBTC (fanHash f)

    ok '_' = True
    ok c   = C.isAlphaNum c

    ugul :: Nat -> Text
    ugul 0   = "anon"
    ugul nat = case natUtf8 nat of
        Right t | all ok t -> t
        _                  -> tshow nat
