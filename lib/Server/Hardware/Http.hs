-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}
{-# OPTIONS_GHC -Wall            #-}
{-# OPTIONS_GHC -Werror          #-}

{-|

    Hardware Device 1 -- Localhost Static HTTP

    There is only one request:

        type ContentType = Cord
        type Path        = Bar

        SERV :: Tab (contentType, Path) -> IO ()


    TODOs
    -----

    - TODO When a SERV request is canceled, remove it from the set.

    - TODO If there are multiple SERV requests, only error-out for paths
      that exist in one of the SERV requests, still fallback to the
      dynamic request system if none of the SERV requests would serve that.

      - This is a weird edge-case to be so picky about, but this enables
        us to build external code to test this behavior.

    - TODO Rename the `%serv` call to `%file`.

    - TODO Make sure to log an error in every situation where a syscall
      is malformed.

    - TODO Restrict incoming requests to localhost-only (do this when we
      open the socket).


    SERV
    ----

    `SERV` sets the static content to be served:

    -   SERV return 0 if they are invalid, and never return otherwise.

    -   The tab keys are matched against filepath cords (e.g. "/index.html").

    -   If there are multiple active SERV requests, all requests will
        return a simple 500 response.

    -   The tab values are a pair of a content content-type and a payload,
        the content type is a Nat, and the payload is a Bar.

        All the content-type values must non-empty cords containing only
        printable ASCII characters.  Invalid requests will get an
        immediate `0` response.

            TODO: Don't restrict the content-type, just refuse to include
            invalidate characters in the actual HTTP response.

When called:
    HEAR ->
        Add to pool of %hear requests.
          (On cancel, remove from the pool).
    HOLD ->
        Add to pool of %hold requests.
          (On cancel, remove from the pool)
    ECHO ->
        If there is no corresponding request, reply right away.
          (On cancel, do nothing)
        Otherwise, fill the http-response TMVar
          (writeTMVar)
    SERV ->
        Add to pool of %serv requests.
          (On cancel, remove from the pool)

On simple GET
    look at the SERV pool,
        If empty, block
        Otherwise, lookup in SERV table
            If not found, 404
            If found, 200

Any other request
    Look at the HEAR pool.
        If empty, block
        Otherwise:
            - Send the response
            - Delete the HEAR
            - Add a TMVar to the ECHO pool.

On timeout:
    Remove TMVar from ECHO pool.
    Send a response to anything matching in the HOLD pool.
-}

module Server.Hardware.Http where

import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Common
import Server.Debug
import Server.Hardware.Types
import Server.LmdbStore

import Control.Concurrent.STM (writeTMVar)
import Data.Acquire           (Acquire, mkAcquire)
import Data.IntMap.Strict     (minView)
import Fan.Convert            (FromNoun(..), ToNoun(..), fromNoun, toNoun)
import Optics                 (set)
import Server.Convert         ()
import Server.Types.Logging   (CogId(..), MachineName(..))
import System.Directory       (removeFile)
import System.IO.Error        (catchIOError)

import qualified Data.CaseInsensitive           as CI
import qualified Data.Vector                    as V
import qualified Network.HTTP.Types             as W
import qualified Network.Socket                 as N
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as W
import qualified Network.WebSockets             as WS

--------------------------------------------------------------------------------

data RawRequest = RR
    { machineName :: MachineName
    , cogId       :: CogId
    , vals        :: Vector Fan
    , callback    :: TVar (Maybe (Fan -> STM ()))
    }

data PathLeaf = PL
    { contentType :: ByteString
    , body        :: File
    }

newtype File = FILE { bytes :: LByteString }

type Bar = ByteString

data StaticRequest = STAT_REQ
    { method  :: Nat
    , path    :: Bar
    , headers :: Vector (Bar, Bar)
    , body    :: Bar
    }

data StaticResponse = STAT_RESP
    { statusCode :: Nat
    , statusMsg  :: Bar
    , headers    :: Vector (Bar, Bar)
    , body       :: Bar
    }

data DynamicRequest = DYN_REQ
    { reqId   :: Nat
    , method  :: Nat
    , path    :: Bar
    , headers :: Vector (Bar, Bar)
    , body    :: Pin
    }

data DynamicResponse = DYN_RESP
    { reqId      :: Nat
    , statusCode :: Nat
    , statusMsg  :: Bar
    , headers    :: Vector (Bar, Bar)
    , body       :: Bar
    }

data Req
    = SERV Fan             -- (StaticRequest -> Maybe StaticResponse) -> IO Void
    | HEAR                 -- IO DynamicRequest
    | HOLD Nat             -- IO ()
    | ECHO DynamicResponse -- IO ()

--------------------------------------------------------------------------------

instance ToNoun StaticRequest where
    toNoun r = toNoun (r.method, r.path, r.headers, mkPin $ BAR $ r.body)

instance FromNoun StaticResponse where
    fromNoun n = do
      (statusCode, statusMsg, headers, body) <- fromNoun n
      pure STAT_RESP{..}

instance ToNoun DynamicRequest where
    toNoun r = toNoun (r.reqId, r.method, r.path, r.headers, r.body)

decodeHttpRequest :: Vector Fan -> Maybe Req
decodeHttpRequest top = (dec . toList) top
  where
    dec ["serv", ss]       = SERV <$> fromNoun ss
    dec ["hear"]           = pure HEAR
    dec ["hold", NAT n]    = pure (HOLD n)
    dec ["echo",r,c,m,h,b] = ECHO <$> echo r c m h b
    dec _                  = Nothing

    echo r c m h b =
        DYN_RESP <$> fromNoun r
                 <*> fromNoun c
                 <*> fromNoun m
                 <*> fromNoun h
                 <*> fromNoun b

--------------------------------------------------------------------------------

getTab :: Fan -> Maybe (Map Fan Fan)
getTab (TAB x) = pure x
getTab _       = Nothing

instance FromNoun File where
    fromNoun (PIN p) = fromNoun p.item
    fromNoun (BAR b) = Just (FILE $ fromStrict b)
    fromNoun _       = Nothing

--------------------------------------------------------------------------------

{-|
-}
data CogState = COG_STATE
    { sock :: N.Socket -- ^ HTTP Socket
    , port :: Int      -- ^ HTTP Port
    , file :: FilePath -- ^ File containing the HTTP Port
    , stik :: Async () -- ^ Webserver thread

    , serv :: TVar (Pool Fan)        -- ^ Active SERV syscalls
    , hold :: TVar (Pool SysCall)    -- ^ Active HOLD syscalls

    , hear :: TVar (Pool SysCall)    -- ^ Active HEAR syscalls
    , lock :: MVar ()                -- ^ Fair ordering for hear

      -- | Map from request-id to a response channel and a list of pending
      -- hold requests.
    , live :: TVar (Pool (TMVar (DynamicResponse, Flow), [Int]))
    }

data HWState = HW_STATE
    { mach  :: FilePath
    , wsApp :: MachineName -> CogId -> WS.ServerApp
    , cogs  :: TVar (Map (MachineName, CogId) CogState)
    , store :: LmdbStore
    }

cancelCog :: CogState -> IO ()
cancelCog cog = do
    cancel cog.stik
    catchIOError (removeFile cog.file) (const pass)

{-
startCog :: Debug => LmdbStore -> HWState -> CogName -> IO CogState
startCog lmdbStore st cogName = do
        let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
        let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
        let tcp   = 6
        let addr  = (N.SockAddrInet 0 localhost)
        let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
        listenSocket <- N.openSocket ainfo
        N.listen listenSocket 5 -- TODO Should this be 5?
        listenPort <- fromIntegral <$> N.socketPort listenSocket
        debugVal (cogName.txt <> "_http_port")
                 (fromIntegral listenPort :: Nat)
-}

stopCogById :: HWState -> MachineName -> CogId -> IO ()
stopCogById st machineName cogId = do
    maybeCogState <-
        atomically do
            oldTab <- readTVar st.cogs
            writeTVar st.cogs (deleteMap (machineName, cogId) oldTab)
            pure (lookup (machineName, cogId) oldTab)

    maybe pass cancelCog maybeCogState

spinCog :: Debug => HWState -> MachineName -> CogId -> IO ()
spinCog st machineName cogId = do
    traceM (unpack ("HTTP_SPINNING:" <> machineName.txt))
    let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
    let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
    let tcp   = 6
    let addr  = (N.SockAddrInet 0 localhost)
    let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
    listenSocket <- N.openSocket ainfo
    N.listen listenSocket 5 -- TODO Should this be 5?
    listenPort <- fromIntegral <$> N.socketPort listenSocket
    debugVal (machineName.txt <> "_http_port")
             (fromIntegral listenPort :: Nat)

    let baseName  = machineName.txt <> ".http.port"
    let portFile = st.mach </> unpack baseName
    debugTextVal (machineName.txt <> "_http_port_file") (pack portFile)
    writeFileUtf8 portFile (tshow listenPort)

    cogState <- do
        serv <- newTVarIO emptyPool
        hold <- newTVarIO emptyPool
        hear <- newTVarIO emptyPool
        lock <- newMVar ()
        sock <- pure listenSocket
        port <- pure listenPort
        live <- newTVarIO emptyPool
        mdo
            stik <- async (servThread st.store (st.wsApp machineName cogId) cs)
            let ~cs = COG_STATE{sock,port,file=portFile,stik,serv,hear,lock,
                                hold,live}
            pure cs

    -- TODO: For now, we just assume that a second cog with the same
    -- name will never be spin while the other one is still spinning.
    --
    -- This is safe, but is an implicit invariant that would be nice
    -- to factor-out eventually.
    atomically $ modifyTVar st.cogs $ insertMap (machineName, cogId) cogState
{-
            sock <- pure listenSocket
            port <- pure listenPort
            live <- newTVarIO []
            wipe <- async (staticWipeThread stat)
            dype <- async (dynoWipeThread stat)
            serv <- async (servThread lmdbStore (st.wsApp machineName) stat)
            stik <- pure (SSS live wipe)
            dyno <- DRS <$> newTVarIO mempty
                        <*> newTVarIO []
                        <*> newTVarIO mempty
                        <*> newTVarIO 1
                        <*> pure dype
            stat <- pure (COG_STATE sock serv port portsFile stik dyno)
            pure stat
-}

vHTTPNamePool :: IORef ThreadNamePool
vHTTPNamePool = unsafePerformIO $ (mkThreadNamePool "HTTP" >>= newIORef)

servThread :: Debug => LmdbStore -> WS.ServerApp -> CogState -> IO ()
servThread lmdbStore wsApp cog = do
    let prefs = W.defaultSettings & W.setPort (fromIntegral cog.port)
    W.runSettingsSocket prefs cog.sock
        $ W.websocketsOr WS.defaultConnectionOptions wsApp
        $ \req k -> do
            -- Manually set the process name since we're a few layers deep
            -- inside WAI's implementation.
            withProcessName "HTTP" $ do
              namePool <- readIORef vHTTPNamePool
              withThreadNamePool namePool $ do
                -- We can't connect the causing serv because it's not 1:1.
                let args = mapFromList [
                      ("url", Right $ decodeUtf8 $ req.rawPathInfo)
                      ]
                withTracingFlow "serv" "http" args [] $ do
                  serv <- atomically do
                              pool <- readTVar cog.serv
                              maybe retry pure $ headMay (toList pool.tab)

                  body <- toStrict <$> W.strictRequestBody req

                  servResponse <-
                    withSimpleTracingEvent "run" "http" $ do
                      evaluate $ fromNoun @(Maybe StaticResponse) $
                                 serv %% toNoun (staticRequestFromWai req body)

                  (waiResponse, flows) <- do
                    case servResponse of
                      Nothing        -> noFlow <$> k badStaticResponse
                      Just (Just sr) -> noFlow <$> k (staticResponseToWai sr)
                      Just Nothing   -> doDynamic req body k

                  pure (flows, [], waiResponse)
  where
    badStaticResponse = W.responseLBS W.status500 [] "Bad response from SERV"

    staticRequestFromWai :: W.Request -> ByteString -> StaticRequest
    staticRequestFromWai req body = STAT_REQ
        { method  = bytesNat (req.requestMethod)
        , path    = req.rawPathInfo
        , headers = V.fromList req.requestHeaders <&>
                       over _1 CI.foldedCase
        , body    = body
        }

    staticResponseToWai :: StaticResponse -> W.Response
    staticResponseToWai r = W.responseLBS stat hedr (fromStrict r.body)
      where
        stat = W.Status (fromIntegral r.statusCode) r.statusMsg
        hedr = toList r.headers <&> over _1 CI.mk

    noFlow :: a -> (a, [Flow])
    noFlow a = (a, [])

    doDynamic
        :: W.Request
        -> ByteString
        -> (W.Response -> IO W.ResponseReceived)
        -> IO (W.ResponseReceived, [Flow])
    doDynamic req body k = do
        withReqHandle cog \(key, mResp) -> do

            -- Intern the pin here that we stick the bar into so it can both be
            -- written to disk now, and so that its representation can be used
            -- mmapped.
            bodyPin <- withSimpleTracingEvent "Intern body" "http" do
              pin     <- mkPin' (BAR body)
              cushion <- CUSHION <$> newIORef mempty
              internPin lmdbStore pin cushion

            let reqHed = V.fromList req.requestHeaders <&>
                             over _1 CI.foldedCase

            let dynReq = DYN_REQ
                    { reqId   = fromIntegral key
                    , method  = bytesNat (req.requestMethod)
                    , path    = req.rawPathInfo
                    , headers = reqHed
                    , body    = bodyPin
                    }

            withTracingResultsFlow "Wait for HEAR" "http" do
              (hearFlow, dynReqFlow) <- withMVar cog.lock \_ -> atomically do
                  pool <- readTVar cog.hear
                  case minView pool.tab of
                      Nothing     -> retry
                      Just (x,xs) -> do
                          dynReqFlow <- writeResponse x dynReq
                          writeTVar cog.hear (set #tab xs $ pool)
                          pure $ (x.cause, dynReqFlow)

              pure (mempty, [dynReqFlow], [], [hearFlow], ())

            (r, flow) <- withSimpleTracingEvent "Wait for response" "http" $
              atomically (readTMVar mResp)

            withTracingFlow "Handle response" "http" mempty [flow] $ do
              let stat = W.Status (fromIntegral r.statusCode) r.statusMsg
              let hedr = toList r.headers <&> over _1 CI.mk
              ret <- k (W.responseLBS stat hedr (fromStrict r.body))
              pure ([], [], ret)

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall = do
    mCog <- lookup (syscall.machine, syscall.cog) <$> readTVar st.cogs
    case mCog of
        Nothing -> do
            -- traceM "NO COG"
            fillInvalidSyscall syscall
            pure (CANCEL pass, [])

        Just cog -> do
            -- traceM "FOUND COG"
            case decodeHttpRequest syscall.args of
                Nothing        -> fillInvalidSyscall syscall $>
                                  (CANCEL pass, [])
                Just HEAR      -> onHear cog
                Just (HOLD r)  -> onHold cog r
                Just (SERV ss) -> onServ cog ss
                Just (ECHO r)  -> onEcho cog r syscall.cause
  where
    {-
        When the cog is ready to receive requests, we just register
        the responder.

        On cancel, we just unregister it.
    -}
    onHear :: CogState -> STM (Cancel, [Flow])
    onHear cog = do
        -- traceM "<onHear>"
        key <- poolRegister cog.hear syscall
        -- traceM "</onHear>"
        pure (CANCEL (poolUnregister cog.hear key), [])

    {-
        When the cog is ready to receive requests, we just register
        the responder.

        On cancel, we just unregister it.
    -}
    onEcho :: CogState -> DynamicResponse -> Flow -> STM (Cancel, [Flow])
    onEcho cog resp cause = do
        -- traceM "<onEcho>"
        livePool <- readTVar cog.live
        holdPool <- readTVar cog.hold
        let reqNat = resp.reqId
        let reqInt = fromIntegral reqNat

        holdResponses <- case lookup reqInt livePool.tab of
            Just (chan, holds) | reqNat == fromIntegral reqInt ->  do
                writeTMVar chan (resp, cause)
                mybResponses <- for holds \holdKey ->
                    case lookup holdKey holdPool.tab of
                        Nothing -> pure Nothing
                        Just kl -> Just <$> writeResponse kl ()
                poolUnregister cog.live reqInt
                -- traceM ("</onEcho key=" <> show reqInt <> ">")
                pure $ catMaybes mybResponses
            _ -> do
                pure []
                -- traceM "</onEcho err=noMatch>"

        echoReq <- writeResponse syscall ()
        pure (CANCEL pass, (echoReq:holdResponses))

    {-
        For static site requests, we just register the fileset.

        On cancel, we just unregister it.

        We don't need to register the responder because SERV syscalls
        never return.
    -}
    onServ :: CogState -> Fan -> STM (Cancel, [Flow])
    onServ cog ss = do
        -- traceM "<onServ>"
        key <- poolRegister cog.serv ss
        -- traceM "</onServ>"
        pure (CANCEL (poolUnregister cog.serv key), [])

    {-
        If the hold doesn't correspond to an active request, respond
        immediately.

        Otherwise, register the active hold.

        On cancel, just remove the hold from the holds pool.  We don't
        need to de-register the hold from the live table, because that
        will clean itself up when the request finishes.
    -}
    onHold :: CogState -> Nat -> STM (Cancel, [Flow])
    onHold cog reqIdNat = do
        -- traceM "<onHold>"
        let reqId = fromIntegral reqIdNat

        live <- readTVar cog.live

        if (fromIntegral reqId /= reqIdNat) then do
            fillInvalidSyscall syscall
            -- traceM "</onHold err=badKey>"
            pure (CANCEL pass, [])
        else
          case lookup reqId live.tab of
            Nothing -> do
                fillInvalidSyscall syscall
                -- traceM "</onHold err=noMatch>"
                pure (CANCEL pass, [])
            Just (mResp, hears) -> do
                key <- poolRegister cog.hold syscall
                let newHears = (key : hears)
                let newEntry = (mResp, newHears)
                let newTab   = insertMap reqId newEntry live.tab
                let newLive  = set #tab newTab $ live
                writeTVar cog.live newLive
                -- traceM ("</onHold key=" <> show key <> ">")
                pure (CANCEL (poolUnregister cog.hold key), [])

{-
    Acquire a new handle ID.  Add a response TMVar to the `dyno.live`
    table.

    And on exit, we remove the entry from the table and cause all the
    associated HOLD calls to return.
-}
-- Custom acquire so we can return the list of flows created in the cleanup.
withReqHandle
    :: CogState
    -> ((Int, TMVar (DynamicResponse, Flow)) -> IO W.ResponseReceived)
    -> IO (W.ResponseReceived, [Flow])
withReqHandle cog action = do
  mask $ \restore -> do
    (key, mResp) <- register
    ret <- restore (action (key, mResp)) `onException`
      -- When we have a timeout exception, we're going to not be able to
      -- connect the hold response with thread being killed.
      unregisterDroppingFlows key
    startedFlows <- unregister key
    pure (ret, startedFlows)
  where
    register = atomically do
        mResp <- newEmptyTMVar
        key   <- poolRegister cog.live (mResp, [])
        pure (key, mResp)

    getHoldsForRequest key = do
        livePool <- readTVar cog.live
        case lookup key livePool.tab of
            Nothing         -> pure mempty
            Just (_, holds) -> do
                holdPool <- readTVar cog.hold
                pure (catMaybes ((`lookup` holdPool.tab) <$> holds))

    unregister key = atomically do
        holds <- getHoldsForRequest key
        startFlows <- for holds $ \h -> writeResponse h ()
        poolUnregister cog.live key
        pure startFlows

    unregisterDroppingFlows = void . unregister

categoryCall :: Vector Fan -> Text
categoryCall args = "%http " <> case decodeHttpRequest args of
  Nothing                -> "UNKNOWN"
  Just (SERV _)          -> "%serv"
  Just HEAR              -> "%hear"
  Just (HOLD _)          -> "%hold"
  Just (ECHO DYN_RESP{}) -> "%echo"

describeCall :: Vector Fan -> Text
describeCall args = "%http " <> case decodeHttpRequest args of
  Nothing                     -> "UNKNOWN"
  Just (SERV _)               -> "%serv"
  Just HEAR                   -> "%hear"
  Just (HOLD n)               -> "%hold " <> tshow n
  Just (ECHO DYN_RESP{reqId}) -> "%echo " <> tshow reqId

createHardwareHttp
    :: Debug
    => FilePath
    -> LmdbStore
    -> (MachineName -> CogId -> WS.ServerApp)
    -> Acquire Device
createHardwareHttp mach store wsApp = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { spin = spinCog st
        , stop = stopCogById st
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }

  where
    shutdown :: HWState -> IO ()
    shutdown st = do
        cogs <- atomically (readTVar st.cogs)
        for_ cogs cancelCog

    startup :: IO HWState
    startup = do
        cogs <- newTVarIO mempty
        pure HW_STATE{store,mach,wsApp,cogs}
