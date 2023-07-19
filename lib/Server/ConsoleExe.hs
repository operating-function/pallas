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
import Servant
import Servant.Client
import Server.Debug
import Server.Evaluator
import Server.Machine
import Server.Types.Logging
import System.Environment
import System.Posix.Signals hiding (Handler)
import System.Process

import Server.Hardware.Http  (createHardwareHttp)
import Server.Hardware.Types (DeviceTable(..))
import System.Random         (randomIO)
-- ort Server.Hardware.Port (createHardwarePort)
import Server.Hardware.Rand (createHardwareRand)
-- ort Server.Hardware.Sock (createHardwareSock)
import Server.Hardware.Time (createHardwareTime)
-- ort Server.Hardware.Wock (createHardwareWock)
import Server.Hardware.Poke (SubmitPoke, createHardwarePoke)

import Control.Concurrent       (threadDelay)
import Control.Exception        (handle)
import Control.Monad.Fail       (fail)
import Control.Monad.State      (State, execState, modify')
import Data.Time.Format.ISO8601 (iso8601Show)
import Fan.Convert
import Fan.Hash                 (fanHash)
import Fan.Save                 (loadPack, savePack)
import Jelly.Types              (hashToBTC)
import System.Directory         (createDirectoryIfMissing, doesFileExist,
                                 getHomeDirectory, removeFile)
import System.Exit              (ExitCode(..), exitWith)
import System.IO.Error          (catchIOError)
import System.Posix.Types       (CPid(CPid))

import qualified Loot.ReplExe
import qualified Rex
import qualified Sire

import qualified Data.Aeson               as A
import qualified Data.ByteString          as BS
import qualified Data.Char                as C
import qualified Fan                      as F
import qualified Fan.Prof                 as Prof
import qualified Network.HTTP.Client      as HTTP
import qualified Network.Socket           as N
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W
import qualified Server.LmdbStore         as DB

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

runControlServer :: Debug => ServerState -> Acquire (Async ())
runControlServer st = do
    (_, port, sock) <- mkAcquire openPort (safeDeleteFile . view _1)
    mkAcquire (async $ ctrlServer port sock) cancel
  where
    openPort = do
        -- TODO Only accept input from localhost
        let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
        let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
        let tcp   = 6
        let addr  = (N.SockAddrInet 0 localhost)
        let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
        listenSocket <- N.openSocket ainfo
        N.listen listenSocket 5 -- TODO Should this be 5?
        listenPort <- fromIntegral <$> N.socketPort listenSocket

        debugVal "control_port" (tshow listenPort)

        let portsFile = st.storeDir </> "ctl.port"

        debugVal "rpc_port_file" (pack @Text portsFile)

        writeFileUtf8 portsFile (tshow listenPort)

        pure (portsFile, listenPort, listenSocket)

    ctrlServer port sock = do
        let set = W.defaultSettings & W.setPort port
                                    & W.setTimeout 600
        W.runSettingsSocket set sock (pathLogged (ctlServer st))

pathLogged :: Debug => W.Application -> W.Application
pathLogged app r k = do
    debug (r.requestMethod, r.rawPathInfo)
    app r k

data FailedToStartDaemon = FAILED_TO_START_DAEMON
    { lastErr :: SomeException
    , cmd     :: String
    , args    :: [String]
    }
  deriving (Show, Exception)

ensureServer :: Debug => FilePath -> IO (Bool, W.Port)
ensureServer d = do
    createDirectoryIfMissing True d

    let portFile = d </> "ctl.port"

    let logFile = d </> "logs"

    -- TODO Handle situation where file exists but daemon is not actually
    -- running.
    hadToStart <- do
        doesFileExist portFile >>= \case
            True -> do
                debugText "daemon_running"
                pure False
            False -> do
                p <- getExecutablePath
                debugVal "logFile" (pack @Text logFile)
                logH <- openFile logFile WriteMode
                let cmd = "nohup"
                let args = [p, "server", d]
                debugVal "daemonCmd" $ map (pack @Text) ([cmd] <> args)
                void (shellBg cmd args (logH, logH))
                loop Nothing cmd args (0::Int)
                pure True

    txt <- readFileUtf8 portFile
    readMay txt & \case
        Nothing -> throwIO (BAD_PORTS_FILE "daemon" portFile txt)
        Just pt -> pure (hadToStart, pt)
  where
    loop mErr cmd args i =
        case mErr of
            Just err | i>= 1000 ->
                throwIO (FAILED_TO_START_DAEMON err cmd args)
            _ ->
                flip handle (clientRequest d reqIsUp) \e -> do
                    threadDelay 10_000
                    loop (Just e) cmd args (i+1)

-- JellyPack -------------------------------------------------------------------

newtype JellyPack = JELLY_PACK { fan :: F.Fan }

instance MimeRender OctetStream JellyPack where
    mimeRender _ = fromStrict . unsafePerformIO . savePack . (.fan)

instance MimeUnrender OctetStream JellyPack where
    mimeUnrender _ = either (Left . unpack) (Right . JELLY_PACK)
                   . unsafePerformIO
                   . loadPack
                   . toStrict


-- MachineStatus ---------------------------------------------------------------

data MachineStatus
    = IDLE
    | RUNNING
    | STARTING
  deriving (Show, Read)

instance A.ToJSON MachineStatus where
  toJSON = A.toJSON . toLower . show

instance A.FromJSON MachineStatus where
    parseJSON x = do
       txt <- A.parseJSON x
       let die = fail ("invalid machine status: " <> txt)
       maybe die pure $ readMay (toUpper txt)


--------------------------------------------------------------------------------

type MachineCap = Capture "machine" MachineName

type ReplayFromCap = Capture "replay-from" ReplayFrom

type PackCap = ReqBody '[OctetStream] JellyPack

--type PokePath = CaptureAll "path" Text

type GET  = Get '[JSON]
type POST = Post '[JSON]

data CtlApi a = CTL_API
    { isUp     :: a :- "up"                                 :> GET ()
    , halt     :: a :- "halt"                               :> POST ()
    , pins     :: a :- "pins"
                    :> GET (Map Text [Text])
    , machines :: a :- "machines"
                    :> GET (Map MachineName MachineStatus)
    , doDu     :: a :- "du"       :> MachineCap             :> GET [Text]
    , start    :: a :- "machines" :> MachineCap :> "start"  :> ReplayFromCap
                    :> POST ()
    , replay   :: a :- "machines" :> MachineCap :> "replay" :> ReplayFromCap
                    :> POST ()
    , boot     :: a :- "boot    " :> MachineCap :> PackCap  :> POST ()
    -- , poke  :: a :- "poke"     :> MachineCap :> PokePath :> PackCap
    --              :> POST ()
    }
  deriving (Generic)

ctlServer :: Debug => ServerState -> W.Application
ctlServer st =
    serve (Proxy :: Proxy (NamedRoutes CtlApi)) CTL_API{..}
  where
    isUp :: Handler ()
    isUp = pure ()

    pins :: Handler (Map Text [Text])
    pins = pure (error "TODO: Re-implement")

    boot :: MachineName -> JellyPack -> Handler ()
    boot n pkg = liftIO (doBoot st n pkg)

    -- poke :: MachineName -> [Text] -> JellyPack -> Handler ()
    -- poke n path package = liftIO (doPoke st n path package)

    {-|
        If we were to respond, there would be a race condition between the
        HTTP response completing and process shutting down (and thereby
        killing this thread) The `forever` just causes the request to
        never respond.  Instead the server will always simply terminate
        the connection with no response.
    -}
    halt :: Handler ()
    halt = forever (putMVar st.termSignal ())

    machines :: Handler (Map MachineName MachineStatus)
    machines = do
        cgz <- liftIO (DB.getMachineNames st.lmdb)
        let tab = mapFromList (cgz <&> \c -> (c, c))
        liftIO $ for tab (getMachineStatus >=> maybe (error "ctlServer: impossible") pure)

    start :: MachineName -> ReplayFrom -> Handler ()
    start machine rf = do
        void $ liftIO $ startMachine st rf machine

    replay :: MachineName -> ReplayFrom -> Handler ()
    replay machine rf = do
        void $ liftIO $ startMachine st rf machine

    getMachineStatus :: MachineName -> IO (Maybe MachineStatus)
    getMachineStatus machine = do
        cgz <- liftIO (setFromList <$> DB.getMachineNames st.lmdb)
        if not (member machine (cgz :: Set MachineName)) then
            pure Nothing
        else atomically do
            (lookup machine <$> readTVar st.machineHandles) >>= \case
                Nothing -> pure (Just IDLE)
                Just vc -> readTVar vc >>= \case
                   Nothing -> pure (Just STARTING)
                   Just{}  -> pure (Just RUNNING)

    doDu :: MachineName -> Handler [Text]
    doDu = liftIO . machineDu st


reqIsUp    :: ClientM ()
reqBoot    :: MachineName -> JellyPack -> ClientM ()
_reqHalt   :: ClientM ()
reqMachines    :: ClientM (Map MachineName MachineStatus)
reqStart    :: MachineName -> ReplayFrom -> ClientM ()
_reqReplay :: MachineName -> ReplayFrom -> ClientM ()
reqDu      :: MachineName -> ClientM [Text]
-- reqPoke    :: MachineName -> [Text] -> JellyPack -> ClientM ()

CTL_API { isUp   = reqIsUp
        , boot   = reqBoot
        , halt   = _reqHalt
        , machines   = reqMachines
        , start   = reqStart
        , replay = _reqReplay
        , doDu   = reqDu
        -- , poke   = reqPoke
        } = client (Proxy @(NamedRoutes CtlApi))

--------------------------------------------------------------------------------

type Prof = Maybe FilePath

data RunType
    = RTSire FilePath Prof Bool
                           Bool       -- Warn on jet deopt
                           Bool       -- Crash on jet deopt
                           [FilePath] -- SireFile
    | RTLoot FilePath Prof Bool [FilePath]
    | RTBoot FilePath Prof Bool MachineName Text
    | RTUses FilePath MachineName
    | RTServ FilePath Prof Bool -- profiling file
                           Bool -- profile laws
                           Bool -- Warn on jet deopt
                           Bool -- Crash on jet deopt
                           Int  -- Number of EVAL workers
    | RTDamn FilePath Prof Bool DaemonAction
    | RTOpen FilePath MachineName CogId
    | RTTerm FilePath MachineName CogId
    | RTMachines FilePath
    | RTStat FilePath
    | RTStart FilePath ReplayFrom (Maybe MachineName)
    -- | RTPoke FilePath MachineName Text FilePath

machineNameArg :: Parser MachineName
machineNameArg = MACHINE_NAME <$> strArgument (metavar "NAME" <> help helpTxt)
  where
    helpTxt = "A name for the machine"

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
              <*> machineNameArg
              <*> cogIdArg)

   <> plunderCmd "open" "Open a terminal's GUI interface."
      (RTOpen <$> storeOpt
              <*> machineNameArg
              <*> cogIdArg)

   <> plunderCmd "sire" "Runs an standalone Sire repl."
      (RTSire <$> storeOpt
              <*> profOpt
              <*> profLaw
              <*> doptWarn
              <*> doptCrash
              <*> many sireFile)

   <> plunderCmd "machines" "List available machines."
      (RTMachines <$> (storeArg <|> storeOpt))

   <> plunderCmd "status" "List machines with their status."
      (RTStat <$> (storeArg <|> storeOpt))

   <> plunderCmd "start" "Resume an idle machine."
      (RTStart <$> storeOpt
               <*> replayFromOption
               <*> fmap Just machineNameArg)

   <> plunderCmd "start-all" "Resume all idle machines."
      (RTStart <$> storeOpt
               <*> replayFromOption
               <*> pure Nothing)

   <> plunderCmd "loot" "Runs an standalone sire repl."
      (RTLoot <$> storeOpt <*> profOpt <*> profLaw <*> many lootFile)

   <> plunderCmd "boot" "Boots a machine."
      (RTBoot <$> storeOpt
              <*> profOpt
              <*> profLaw
              <*> machineNameArg
              <*> bootHashArg)

   <> plunderCmd "du" "du -ab compatible output for pin state."
        (RTUses <$> storeOpt
                <*> machineNameArg)

   <> plunderCmd "server" "Replays the events in a machine."
        (RTServ <$> (storeArg <|> storeOpt)
                <*> profOpt
                <*> profLaw
                <*> doSnap
                <*> doptWarn
                <*> doptCrash
                <*> numWorkers)

   <> plunderCmd "daemon" "Run a daemon (if not already running)."
        (RTDamn <$> (storeArg <|> storeOpt) <*> profOpt <*> profLaw
                <*> daemonAction)

   -- <> plunderCmd "poke" "Pokes a started cog with a value."
   --      -- TODO: should pokePath parse the '/' instead?
   --      (RTPoke <$> storeOpt <*> machineNameArg <*> pokePath
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

data DaemonAction = START | STOP | RESTART

daemonAction :: Parser DaemonAction
daemonAction
    = flag' START   (long "start")
  <|> flag' STOP    (long "stop")
  <|> flag' RESTART (long "restart")

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

data StartDuringShutdown = START_DURING_SHUTDOWN MachineName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

data MachineAlreadyStarted = MACHINE_ALREADY_RUNNING MachineName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

data NoSuchMachine = NO_SUCH_MACHINE MachineName
  deriving (Eq, Ord, Show)
  deriving anyclass Exception

withDaemon :: Debug => FilePath -> ClientM a -> IO a
withDaemon storeDir act = do
    (_, port) <- ensureServer storeDir

    manager <- HTTP.newManager HTTP.defaultManagerSettings
                                { HTTP.managerResponseTimeout =
                                    HTTP.responseTimeoutNone }

    let baseUrlScheme = Http
    let baseUrlHost   = "localhost"
    let baseUrlPort   = port
    let baseUrlPath   = ""
    either throwIO pure =<< runClientM act (mkClientEnv manager BaseUrl{..})

clientRequest :: FilePath -> ClientM a -> IO a
clientRequest storeDir act = do
    portTxt <- readFileUtf8 (storeDir </> "ctl.port")
    Just (port::Int) <- pure (readMay portTxt)

    manager <- HTTP.newManager HTTP.defaultManagerSettings
                                { HTTP.managerResponseTimeout =
                                    HTTP.responseTimeoutNone }

    let baseUrlScheme = Http
    let baseUrlHost   = "localhost"
    let baseUrlPort   = port
    let baseUrlPath   = ""
    either throwIO pure =<< runClientM act (mkClientEnv manager BaseUrl{..})

-- | Initial test here. We create a store, create one machine in it, and then
-- write one artificial logbatch, and then read it back.
main :: IO ()
main = Rex.colorsOnlyInTerminal do
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
        RTLoot d _ _ fz      -> withDaemon d $ liftIO $ Loot.ReplExe.replMain fz
        RTSire _ _ _ j c fz  -> do writeIORef F.vWarnOnJetFallback j
                                   writeIORef F.vCrashOnJetFallback c
                                   liftIO $ Sire.main fz
        RTOpen d machine cog -> void (openBrowser d machine cog)
        RTTerm d machine cog -> void (openTerminal d machine cog)
        RTMachines d         -> listMachines d False
        RTStat d             -> listMachines d True
        RTStart d r m        -> maybe (startAll d r) (startOne d r) m
        RTBoot d _ _ x y     -> bootMachine d x y
        RTUses d machine         -> duMachine d machine
        RTServ d _ _ s j c w -> do writeIORef F.vWarnOnJetFallback j
                                   writeIORef F.vCrashOnJetFallback c
                                   runServer s w d
        RTDamn d _ _ START   -> startDaemon d
        RTDamn d _ _ STOP    -> killDaemon d
        RTDamn d _ _ RESTART -> killDaemon d >> startDaemon d
        -- RTPoke d cog p y     -> pokeCog d cog p y

withProfileOutput :: RunType -> IO () -> IO ()
withProfileOutput args act = do
    case argsProf args of
        Nothing          -> act
        Just (fil, laws) -> do
            putStrLn ("Profiling Output: " <> pack fil)
            Prof.withProfileOutput fil laws act
  where
    argsProf = \case
        RTSire _ p l _ _ _   -> (,l) <$> p
        RTLoot _ p l _       -> (,l) <$> p
        RTOpen _ _ _         -> Nothing
        RTTerm _ _ _         -> Nothing
        RTMachines _         -> Nothing
        RTStat _             -> Nothing
        RTStart _ _ _        -> Nothing
        RTUses _ _           -> Nothing
        RTBoot _ p l _ _     -> (,l) <$> p
        RTServ _ p l _ _ _ _ -> (,l) <$> p
        RTDamn _ p l _       -> (,l) <$> p
        -- RTPoke _ _ _ _       -> Nothing

startDaemon :: Debug => FilePath -> IO ()
startDaemon d =
     ensureServer d >>= \case
         (True,  _) -> debugText "Daemon started."
         (False, _) -> debugText "Daemon already running."

bootMachine :: (Debug, Rex.RexColor) => FilePath -> MachineName -> Text -> IO ()
bootMachine d c pash = do
    withDaemon d $ do
        let fil = unpack pash
        e <- liftIO (doesFileExist fil)
        unless e (error $ unpack ("File does not exist: " <> pash))
        val <- liftIO (Sire.loadFile fil)
        reqBoot c (JELLY_PACK val)

-- pokeCog :: (Debug, Rex.RexColor) => FilePath -> MachineName -> Text -> FilePath
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

duMachine :: Debug => FilePath -> MachineName -> IO ()
duMachine d c = do
  withDaemon d $ do
      retLines <- reqDu c
      liftIO $ forM_ retLines $ putStrLn

killDaemon :: Debug => FilePath -> IO ()
killDaemon d = do
    let pax = (d </> "pid")
    exists <- doesFileExist pax
    when exists $ do
       pidTxt <- readFileUtf8 pax
       mPid   <- pure (readMay pidTxt)
       whenJust mPid \alien -> do
           debugText "killing_daemon"
           -- TODO Handle situation where process does not actually exist.
           signalProcess sigTERM (alien :: CPid)

startOne :: Debug => FilePath -> ReplayFrom -> MachineName -> IO ()
startOne d r machine =
    withDaemon d (reqStart machine r)

startAll :: Debug => FilePath -> ReplayFrom -> IO ()
startAll d r = do
    withDaemon d do
        status <-  reqMachines
        for_ (keys status) \machine -> do
            reqStart machine r

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

shellBg :: String -> [String] -> (Handle, Handle) -> IO ()
shellBg c a (out, err) = do
    let p = (proc c a) { std_in        = NoStream
                       , std_out       = UseHandle out
                       , std_err       = UseHandle err
                       , close_fds     = True
                       , delegate_ctlc = False
                       }
    (_, _, _, _) <- createProcess p
    pure ()

openBrowser :: FilePath -> MachineName -> CogId -> IO ExitCode
openBrowser dir machineName cogId = do
    let cogNm = tshow cogId.int
    let pax = (dir </> unpack (machineName.txt <> "." <> cogNm <> ".http.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve HTTP")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "http" pax cont)
                   Just pt -> pure pt
    let url = "http://localhost:" <> show port
    shellFg "xdg-open" [url]

openTerminal :: FilePath -> MachineName -> CogId -> IO ExitCode
openTerminal dir machineName cogId = do
    let cogNm = tshow cogId.int
    let pax = (dir </> unpack (machineName.txt <> "." <> cogNm <>
                               ".telnet.port"))
    exists <- doesFileExist pax
    unless exists (error "Cog does not serve Telnet")
    port <- do cont <- readFileUtf8 pax
               case readMay @Text @Word cont of
                   Nothing -> throwIO (BAD_PORTS_FILE "telnet" pax cont)
                   Just pt -> pure pt
    shellFg "nc" ["localhost", show port]

data ServerState = SERVER_STATE
    { storeDir       :: FilePath
    , enableSnaps    :: Bool
    , isShuttingDown :: TVar Bool
    , machineHandles :: TVar (Map MachineName (TVar (Maybe Machine)))
    , termSignal     :: MVar ()
    , lmdb           :: DB.LmdbStore
    , hardware       :: DeviceTable
    , poke           :: SubmitPoke
    , evaluator      :: Evaluator
    }

runServer :: Debug => Bool -> Int -> FilePath -> IO ()
runServer enableSnaps numWorkers storeDir = do
    debugFan "run_server"

    -- Setup plunder interpreter state.
    writeIORef F.vTrkFan $! \x -> do
        now <- getCurrentTime
        debug (["trk"::Text, pack (iso8601Show now)], x)

    writeIORef F.vShowFan  $! Loot.ReplExe.showFan
    writeIORef F.vJetMatch $! F.jetMatch

    termSignal <- newEmptyMVar
    for_ [sigTERM, sigINT] $ \sig -> do
        installHandler sig (Catch (putMVar termSignal ())) Nothing

    isShuttingDown <- newTVarIO False
    machineHandles <- newTVarIO mempty

    let devTable db hw_poke = do
            hw1_rand          <- createHardwareRand
          --(hw4_wock, wsApp) <- createHardwareWock
            let wsApp _machineName _cogId _ws = pure ()
            hw2_http          <- createHardwareHttp storeDir db wsApp
          --hw3_sock          <- createHardwareSock storeDir
            hw5_time          <- createHardwareTime
          --hw6_port          <- createHardwarePort
            (pure . DEVICE_TABLE . mapFromList) $
                [ ( "rand", hw1_rand )
                , ( "http", hw2_http )
                --( "sock", hw3_sock )
                --( "wock", hw4_wock )
                , ( "time", hw5_time )
                --( "port", hw6_port )
                , ( "poke", hw_poke  )
                ]

    let serverState = do
            _  <- getPidFile storeDir
            db <- DB.openDatastore storeDir
            (pokeHW, submitPoke) <- createHardwarePoke
            hw <- devTable db pokeHW
            ev <- evaluator numWorkers
            st <- pure SERVER_STATE
                { storeDir
                , isShuttingDown
                , machineHandles
                , lmdb      = db
                , hardware  = hw
                , poke      = submitPoke
                , evaluator = ev
                , termSignal
                , enableSnaps
                }
            cs <- runControlServer st
            pure (st, cs)

    with serverState shutdownOnSigkill

{-
    To boot a machine, we just write the initial value as a single Init
    to the log, and then exit.
-}
doBoot :: Debug => ServerState -> MachineName -> JellyPack -> IO ()
doBoot st machineName pak = do
    debug ["booting_machine", machineName.txt]

    createdMachine <- DB.newMachine st.lmdb machineName
    firstCogId <- COG_ID <$> randomIO

    if not createdMachine then
        error ("Trying to overwrite existing machine " <> show machineName)
    else
        DB.writeMachineSnapshot st.lmdb machineName (BatchNum 0)
                                (singletonMap firstCogId (CG_SPINNING pak.fan))

-- {-
--     Deliver a noun from the outside to a given cog.
-- -}
-- doPoke :: Debug => ServerState -> MachineName -> [Text] -> JellyPack -> IO ()
-- doPoke st machineName path pak = do
--     debug ["poke_cog", machineName.txt]
--     st.poke machineName (fromList path) pak.fan

{-
    `vShutdownFlag` serves as a guard which prevents new machines from
    started up (and therefore being added to `vHandles` while we are
    trying to shut everything down.

    `vHandles` is a `MachineHandle` for each running machine.  These handles
    are in the `Nothing` state while starting up.

    This assumes that a machine will never fail to start up.  Make sure that
    is true!
-}
shutdownOnSigkill :: Debug => (ServerState, Async ()) -> IO ()
shutdownOnSigkill (st, rpcServer) = do
    readMVar st.termSignal

    handles <-
        atomically do
            writeTVar st.isShuttingDown $! True
            readTVar st.machineHandles

    debugText "beginning_shutdown"

    cancel rpcServer

    -- Ask each machine to halt, and wait for their thread to exit.
    forConcurrently_ (mapToList handles)
        \(machineNm, vHandle) -> do
            debug ["asking_for_machine_to_stop", machineNm.txt]
            machine <- atomically $ readTVar vHandle >>= maybe retry pure
            shutdownMachine machine
            debug ["machine_halted", machineNm.txt]

    debugText "finished_shutdownOnSigKill"

listMachines :: Debug => FilePath -> Bool -> IO ()
listMachines d showStatus = do
    withDaemon d $ do
        debugText "list_machines"
        status <- reqMachines
        liftIO $ showStatus & \case
            False ->  debug ((.txt) <$> keys status)
            True  ->  for_ (mapToList status) \(k,v) -> do
                          debugTextVal k.txt (toLower $ tshow v)

startMachine :: Debug => ServerState -> ReplayFrom -> MachineName -> IO ()
startMachine st replayFrom machineName = do
    debug ["starting_machine", machineName.txt]

    cache <- DB.CUSHION <$> newIORef mempty

    DB.loadMachine st.lmdb machineName >>= \case
        Nothing -> do
            throwIO (NO_SUCH_MACHINE machineName)
        Just _bn -> do
            vHandle <- join $ atomically do
                halty <- readTVar st.isShuttingDown
                table <- readTVar st.machineHandles
                case (halty, lookup machineName table) of
                    (True, _) ->
                        pure $ throwIO (START_DURING_SHUTDOWN machineName)
                    (_, Just{}) ->
                        pure $ throwIO (MACHINE_ALREADY_RUNNING machineName)
                    (False, Nothing) -> do
                        h <- newTVar Nothing
                        modifyTVar' st.machineHandles (insertMap machineName h)
                        pure (pure h)

            debug ["found_machine", "starting_replay"::Text]
            let hw   = st.hardware
            let eval = st.evaluator
            let enableSnaps = st.enableSnaps
            let ctx  = MACHINE_CONTEXT{eval, hw, machineName, lmdb=st.lmdb,
                                       enableSnaps}
            h <- withCogDebugging machineName.txt
                   (replayAndCrankMachine cache ctx replayFrom)
            atomically (writeTVar vHandle $! Just h)
            debugText "machine_started"

machineDu :: Debug => ServerState -> MachineName -> IO [Text]
machineDu st machineName = do
  cache <- DB.CUSHION <$> newIORef mempty

  -- Read the current noun if the machine is started and alive.
  mybNoun <- atomically $ do
    machines <- readTVar st.machineHandles
    case lookup machineName machines of
      Nothing -> pure Nothing
      Just machineVar -> do
        mybM <- readTVar machineVar
        case mybM of
          Nothing -> pure Nothing
          Just c -> do
            (MOMENT n _) <- readTVar c.liveVar
            pure $ Just n

  -- If there's no noun (machine isn't started), replay the machine.
  noun <- case mybNoun of
    Just noun -> pure noun
    Nothing -> do
      let enableSnaps = st.enableSnaps
      let hw   = st.hardware  -- not used
      let eval = st.evaluator -- not used
      let ctx  = MACHINE_CONTEXT{eval, hw, machineName, lmdb=st.lmdb,
                                 enableSnaps}
      (_, MOMENT noun _) <- performReplay cache ctx LatestSnapshot
      pure noun

  -- TODO: Unexpectedly, this still works with multicog, it just doesn't
  -- associate nouns with their cogs, meaning all toplevel cog state gets put
  -- into one pin. A nicer implementation would separate out by ship id.
  (pins, _hed, blob) <- F.saveFan $ toNoun noun

  pure $ execState (fanDu (txt machineName) pins blob) []
  where
    fanDu :: Text -> Vector F.Pin -> ByteString -> State [Text] ()
    fanDu name refs blob = do
      refSize <- sum <$> mapM (pinDu [name]) refs
      _ <- calcEntry [name] refSize blob
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
