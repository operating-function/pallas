-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}

module Server.ConsoleExe (main) where

import Data.Acquire
import Options.Applicative
import PlunderPrelude       hiding (Handler, handle)
import Server.Debug
import Server.Evaluator
import Server.Machine
import Server.Types.Logging
import System.Environment
import System.Posix.Signals hiding (Handler)
import System.Process
import Fan.Convert

import Server.Hardware.Http  (createHardwareHttp)
import Server.Hardware.Types (DeviceTable(..))
import System.Random         (randomIO)
import Server.Hardware.Port (createHardwarePort)
import Server.Hardware.Rand (createHardwareRand)
-- ort Server.Hardware.Sock (createHardwareSock)
import Server.Hardware.Time (createHardwareTime)
-- ort Server.Hardware.Wock (createHardwareWock)
import Server.Hardware.Poke (createHardwarePoke)

import Control.Concurrent       (threadDelay)
import Control.Monad.State      (State, execState, modify')
import Data.Time.Format.ISO8601 (iso8601Show)
import Fan.Hash                 (fanHash)
import Hash256                  (hashToBTC)
import Loot.ReplExe             (showFan, trkFan)
import Sire.Types               (trkM)
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
import qualified Fan.Seed         as F
import qualified Fan.Prof         as Prof
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

data RunType
    = RTSire FilePath Prof Bool
                           Bool       -- Warn on jet deopt
                           Bool       -- Crash on jet deopt
                           [FilePath] -- SireFile
    | RTSave Prof Bool
                  Bool     -- Warn on jet deopt
                  Bool     -- Crash on jet deopt
                  FilePath -- Seed file
                  FilePath -- SireFile
    | RTShow FilePath
    | RTRepl FilePath Bool Bool
    | RTLoot FilePath Prof Bool [FilePath]
    | RTBoot Prof Bool FilePath Text
    | RTUses FilePath Int
    | RTOpen FilePath CogId
    | RTTerm FilePath CogId
    -- TODO: Rename 'run' or 'spin' or 'crank' or something.
    | RTStart FilePath
              Prof
              Bool -- profiling file
              Bool -- profile laws
              Bool -- Warn on jet deopt
              Bool -- Crash on jet deopt
              Int  -- Number of EVAL workers
              ReplayFrom
    -- | RTPoke FilePath Text FilePath

cogIdArg :: Parser CogId
cogIdArg = COG_ID <$> argument auto (metavar "COG" <> help helpTxt)
  where
    helpTxt = "The cog id number"

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

lootFile :: Parser FilePath
lootFile =
    strArgument (metavar "LOOT" <> help helpTxt)
  where
    helpTxt = "A loot file to load before starting the REPL"

plunderCmd :: String -> String -> Parser a -> Mod CommandFields a
plunderCmd cmd desc parser =
    command cmd (info (parser <**> helper) (progDesc desc))

runType :: FilePath -> Parser RunType
runType defaultDir = subparser
    ( plunderCmd "term" "Connect to the terminal of a cog."
      (RTTerm <$> storeOpt
              <*> cogIdArg)

   <> plunderCmd "open" "Open a terminal's GUI interface."
      (RTOpen <$> storeOpt
              <*> cogIdArg)

   <> plunderCmd "sire" "Runs an standalone Sire repl."
      (RTSire <$> storeOpt
              <*> profOpt
              <*> profLaw
              <*> doptWarn
              <*> doptCrash
              <*> many sireFile)

   <> plunderCmd "save" "Loads a sire file ane save a seed"
      (RTSave <$> profOpt
              <*> profLaw
              <*> doptWarn
              <*> doptCrash
              <*> seedFile
              <*> sireFile)

   <> plunderCmd "show" "Print a seed file"
      (RTShow <$> seedFile)

   <> plunderCmd "repl" "Interact with a seed file"
      (RTRepl <$> seedFile
              <*> doptWarn
              <*> doptCrash)

   <> plunderCmd "start" "Resume an idle machine."
      (RTStart <$> storeArg
               <*> profOpt
               <*> profLaw
               <*> doSnap
               <*> doptWarn
               <*> doptCrash
               <*> numWorkers
               <*> replayFromOption)

   <> plunderCmd "loot" "Runs an standalone sire repl."
      (RTLoot <$> storeOpt <*> profOpt <*> profLaw <*> many lootFile)

   <> plunderCmd "boot" "Boots a machine."
      (RTBoot <$> profOpt
              <*> profLaw
              <*> storeArg
              <*> bootHashArg)

   <> plunderCmd "du" "du -ab compatible output for pin state."
        (RTUses <$> storeArg <*> numWorkers)

   -- <> plunderCmd "poke" "Pokes a started cog with a value."
   --      -- TODO: should pokePath parse the '/' instead?
   --      (RTPoke <$> storeOpt <*> pokePath
   --              <*> pokeSire)
   )
  where
    -- pokePathHelp = help "Path to send data on"
    -- pokeSireHelp = help "Sire file to parse and send"
    storeHlp = help "Location of plunder data"
    profHelp = help "Where to output profile traces (JSON)"
    storeArg = strArgument (metavar "STORE" <> storeHlp)

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

    doSnap :: Parser Bool
    doSnap = fmap not
           $ switch ( short 'S'
                    <> long "disable-snapshots"
                    <> help "Disable snapshots"
                     )


    profOpt =
        fmap (\x -> if null x then Nothing else Just x) $
        strOption ( long "profile-output"
                 <> value ""
                 <> short 'p'
                 <> metavar "PROF_FILE"
                 <> profHelp
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
       <> progDesc "Let's run plunder."
       <> header "new-network - a test for running plunder machines"
        )

data BadPortsFile = BAD_PORTS_FILE Text FilePath Text
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

-- | Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main :: IO ()
main = do
  Rex.colorsOnlyInTerminal do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    home <- getHomeDirectory
    ddir <- lookupEnv "PLUNDER_DIR" <&> maybe (home </> ".plunder") id
    args <- customExecParser
            (prefs (showHelpOnError <> showHelpOnEmpty <> noBacktrack))
            (runInfo ddir)

    withProfileOutput args $
      withDebugOutput $
      case args of
        RTBoot _ _ d y  -> bootMachine d y
        RTUses d w      -> duMachine d w
        RTShow fp       -> showSeed fp
        RTRepl fp j c   -> do
            writeIORef F.vWarnOnJetFallback j
            writeIORef F.vCrashOnJetFallback c
            replSeed fp

        RTOpen d cog    -> void (openBrowser d cog)
        RTTerm d cog    -> void (openTerminal d cog)

        RTLoot _ _ _ fz -> do
            liftIO $ Loot.ReplExe.replMain fz

        RTSire _ _ _ j c fz -> do
            writeIORef F.vWarnOnJetFallback j
            writeIORef F.vCrashOnJetFallback c
            liftIO $ Sire.main fz

        RTSave _ _ j c sd sr -> do
            writeIORef F.vWarnOnJetFallback j
            writeIORef F.vCrashOnJetFallback c
            saveSeed sd sr

        RTStart d _ _ s j c w r -> do
            writeIORef F.vWarnOnJetFallback j
            writeIORef F.vCrashOnJetFallback c
            runMachine d r s w

withProfileOutput :: RunType -> IO () -> IO ()
withProfileOutput args act = do
    case argsProf args of
        Nothing          -> act
        Just (fil, laws) -> do
            putStrLn ("Profiling Output: " <> pack fil)
            Prof.withProfileOutput fil laws act
  where
    argsProf = \case
        RTSire _ p l _ _ _      -> (,l) <$> p
        RTSave p l _ _ _ _      -> (,l) <$> p
        RTLoot _ p l _          -> (,l) <$> p
        RTShow _                -> Nothing
        RTRepl _ _ _            -> Nothing
        RTOpen _ _              -> Nothing
        RTTerm _ _              -> Nothing
        RTStart _ p l _ _ _ _ _ -> (,l) <$> p
        RTUses _ _              -> Nothing
        RTBoot p l _ _          -> (,l) <$> p
        -- RTPoke _ _ _ _       -> Nothing

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
    e <- liftIO (doesFileExist inputFile)
    unless e (error $ unpack ("File does not exist: " <> inputFile))
    val <- liftIO (Sire.loadFile inputFile)
    pin <- F.mkPin' val
    byt <- F.saveSeed pin
    writeFile outFile byt

showSeed :: (Debug, Rex.RexColor) => FilePath -> IO ()
showSeed seedFileToShow = do
    writeIORef F.vShowFan showFan
    writeIORef F.vTrkFan  trkFan
    byt <- readFile seedFileToShow
    pin <- F.loadSeed byt >>= either throwIO pure
    print pin.item
    fullPrint pin.item
  where
    fullPrint x = trkM $ F.REX $ Sire.planRexFull $ toNoun x

-- TODO: If given something like $path.sire:main, load that instead of
-- just using a seed.
replSeed :: (Debug, Rex.RexColor) => FilePath -> IO ()
replSeed seedFileToShow = do
    writeIORef F.vJetMatch           $! F.jetMatch
    writeIORef F.vShowFan            $! showFan
    writeIORef F.vTrkFan             $! trkFan
    writeIORef F.vWarnOnJetFallback  $! False -- True
    writeIORef F.vCrashOnJetFallback $! False -- True
    byt <- readFile seedFileToShow
    pin <- F.loadSeed byt >>= either throwIO pure
    interactive pin.item
  where
    fullPrint x = trkM $ F.REX $ Sire.planRexFull $ toNoun x

    interactive st0 = do
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
                BS.putStr output
                unless (null input) do
                    interactive st1

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

runMachine :: Debug => FilePath -> ReplayFrom -> Bool -> Int -> IO ()
runMachine storeDir replayFrom enableSnaps numWorkers = do
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
