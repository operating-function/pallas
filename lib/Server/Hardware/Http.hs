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
    REQ ->
        Add to pool of pending http client requests.
          (On cancel, remove from the pool).
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

import Data.Sorted
import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Common
import Server.Debug
import Server.Hardware.Types
import Server.LmdbStore

import Data.Acquire         (Acquire, mkAcquire)
import Data.CaseInsensitive (CI)
import Data.IntMap.Strict   (minView)
import Fan.Convert          (FromNoun(..), ToNoun(..), fromNoun, toNoun)
import Optics               (set)
import Server.Convert       ()
import Server.Types.Logging (CogId(..))
import System.Directory     (removeFile)
import System.IO.Error      (catchIOError)

import qualified Data.CaseInsensitive           as CI
import qualified Data.Vector                    as V
import qualified Network.HTTP.Client            as Client
import qualified Network.HTTP.Client.TLS        as Client
import qualified Network.HTTP.Types             as W
import qualified Network.Socket                 as N
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as W
import qualified Network.WebSockets             as WS

--------------------------------------------------------------------------------

data RawRequest = RR
    { cogId    :: CogId
    , vals     :: Vector Fan
    , callback :: TVar (Maybe (Fan -> STM ()))
    }

data PathLeaf = PL
    { contentType :: ByteString
    , body        :: File
    }

newtype File = FILE { bytes :: LByteString }

type Bar = ByteString

type List a = [a]

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
    | REQ ClientReq        -- ClientReq -> IO ClientResp
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
    dec ["serv", ss]        = SERV <$> fromNoun ss
    dec ["hear"]            = pure HEAR
    dec ["hold", NAT n]     = pure (HOLD n)
    dec ["echo",r,c,m,h,b]  = ECHO <$> echo r c m h b
    dec ["req",m,u,h,b,r,t] = REQ <$> req m u h b r t
    dec _                   = Nothing

    req m u h b r t = do
        traceM $ show (m,u,h,b,r,t)
        traceM $ show $
            CLIENT_REQ <$> fromNoun m
                       <*> fromNoun u
                       <*> (fromNoun h <&> map \(n,v) -> (CI.mk n, v))
                       <*> fromNoun b
                       <*> fromNoun r
                       <*> fromNoun t
        CLIENT_REQ <$> fromNoun m
                   <*> fromNoun u
                   <*> (fromNoun h <&> map \(n,v) -> (CI.mk n, v))
                   <*> fromNoun b
                   <*> fromNoun r
                   <*> fromNoun t

    echo r c m h b =
        DYN_RESP <$> fromNoun r
                 <*> fromNoun c
                 <*> fromNoun m
                 <*> fromNoun h
                 <*> fromNoun b


getTab :: Fan -> Maybe (Tab Fan Fan)
getTab (TAb x) = pure x
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

    , serverThread :: Async ()
        -- ^ The thread that actually runs the HTTP server.

    , serv :: TVar (Pool Fan)        -- ^ Active SERV syscalls
    , hold :: TVar (Pool SysCall)    -- ^ Active HOLD syscalls

    , hear :: TVar (Pool SysCall)    -- ^ Active HEAR syscalls
    , lock :: MVar ()                -- ^ Fair ordering for hear

    , live :: TVar (Pool (TMVar (DynamicResponse, Flow), [Int]))
      -- ^ Map from request-id to a response channel and a list of pending
      -- hold requests.

    , liveClientReqs    :: TVar (Pool (ClientReq, SysCall))
        -- ^ Live HTTP-Client syscalls and their coresponding requests.

    , pendingClientReqs :: TVar [Int]
         -- ^ List of requested HTTP requests that haven't been launched
         -- yet.

    , clientThread      :: Async Void
          -- ^ HTTP Client worker thread.
    }

data HWState = HW_STATE
    { mach    :: FilePath
    , wsApp   :: CogId -> WS.ServerApp
    , cogs    :: TVar (Map CogId CogState)
    , store   :: LmdbStore
    , manager :: Client.Manager
    }

cancelCog :: CogState -> IO ()
cancelCog cog = do
    cancel cog.serverThread
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

stopCogById :: HWState -> CogId -> IO ()
stopCogById st cogId = do
    maybeCogState <-
        atomically do
            oldTab <- readTVar st.cogs
            writeTVar st.cogs (deleteMap cogId oldTab)
            pure (lookup cogId oldTab)

    maybe pass cancelCog maybeCogState

spinCog :: Debug => HWState -> CogId -> IO ()
spinCog st cogId = do
    traceM "HTTP_SPINNING"
    let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
    let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
    let tcp   = 6
    let addr  = (N.SockAddrInet 0 localhost)
    let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
    listenSocket <- N.openSocket ainfo
    N.listen listenSocket 5 -- TODO Should this be 5?
    listenPort <- fromIntegral <$> N.socketPort listenSocket
    debugVal ("_http_port")
             (fromIntegral listenPort :: Nat)

    let baseName  = (tshow cogId.int) <> ".http.port"
    let portFile = st.mach </> unpack baseName
    debugTextVal ("_http_port_file") (pack portFile)
    writeFileUtf8 portFile (tshow listenPort)

    cogState <- do
        serv <- newTVarIO emptyPool
        hold <- newTVarIO emptyPool
        hear <- newTVarIO emptyPool
        lock <- newMVar ()
        sock <- pure listenSocket
        port <- pure listenPort
        live <- newTVarIO emptyPool
        liveClientReqs    <- newTVarIO emptyPool
        pendingClientReqs <- newTVarIO []
        mdo
            serverThread <- async (servThread st.store (st.wsApp cogId) cs)
            clientThread <- async (clientWorker st.manager cs)
            let file = portFile
            let ~cs  = COG_STATE{sock,port,file,serverThread,serv,hear,lock,
                                 hold,live,liveClientReqs,pendingClientReqs,
                                 clientThread}
            pure cs

    -- TODO: For now, we just assume that a second cog with the same
    -- name will never be spin while the other one is still spinning.
    --
    -- This is safe, but is an implicit invariant that would be nice
    -- to factor-out eventually.
    atomically $ modifyTVar st.cogs $ insertMap cogId cogState
{-
            sock <- pure listenSocket
            port <- pure listenPort
            live <- newTVarIO []
            wipe <- async (staticWipeThread stat)
            dype <- async (dynoWipeThread stat)
            serv <- async (servThread lmdbStore (st.wsApp machineName) stat)
            serverThread <- pure (SSS live wipe)
            dyno <- DRS <$> newTVarIO mempty
                        <*> newTVarIO []
                        <*> newTVarIO mempty
                        <*> newTVarIO 1
                        <*> pure dype
            stat <- pure (COG_STATE sock serv port portsFile serverThread dyno)
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

{-
    Monitors the set of pending reqs.

    -   Whenever there's a new request, launch a thread to perform
        the request.

        -   Also launch a thread to monitor the pool.  If the syscall
            is removed from the pool, cancel the thread.

        -   When the request finishes, it should write it's results
            into the syscall return slot.
-}
clientWorker :: Client.Manager -> CogState -> IO Void
clientWorker manager cog = forever do

    newReq <- atomically do

                  key <- readTVar cog.pendingClientReqs >>= \case
                             []   -> retry
                             t:ts -> writeTVar cog.pendingClientReqs ts $> t

                  poolLookup key cog.liveClientReqs >>= \case
                      Nothing             -> pure Nothing
                      Just (req, syscall) -> pure $ Just (key, req, syscall)

    traceM $ show ("newReq"::Text, newReq)

    -- If the requests wasn't already canceled, launch a thread to make
    -- the HTTP request.
    whenJust newReq \(key, req, syscall) -> do

        reqThread     <- async (launchRequest key req syscall)
        _cancelThread <- async (launchCanceler key reqThread)
        pure ()

  where
    -- If the syscall is every unregistered (happens on
    -- syscall cancel), then we cancel the request thread.
    --
    -- In the happy case, the request is succeeds and is
    -- unregistered through that.  In that case, we still
    -- cancel the requesting thread, but that does nothing
    -- because that thread is already done.
    launchCanceler key reqThread = do
        () <- atomically $ poolLookup key cog.liveClientReqs >>= \case
                  Nothing -> pure ()
                  Just{}  -> retry
        cancel reqThread

    launchRequest key req syscall = do
        traceM $ show ("launchRequest"::Text, key, req, syscall)
        result <- try do request <- mkClientReq req;
                         print request
                         Client.httpLbs request manager
        print result
        atomically do
            flow <- writeResponse syscall
                        (either clientExnNoun (toNoun . loadClientResp) result)
            todoHandleFlow flow
            poolUnregister cog.liveClientReqs key

    -- TODO: Rework this to make it tracable.
    todoHandleFlow _flow = do
        pure ()

clientExnNoun :: Client.HttpException -> Fan
clientExnNoun = \case
    Client.InvalidUrlException _url p ->
        toNoun ("INVALID_URL"::Text, BAR (encodeUtf8 $ pack p))

    Client.HttpExceptionRequest _req err -> case err of
        Client.StatusCodeException _ _  -> "BAD_STATUS_CODE"
        Client.OverlongHeaders          -> "OVERLONG_HEADERS"
        Client.ResponseTimeout          -> "RESPONSE_TIMEOUT"
        Client.ConnectionTimeout        -> "CONNECTION_TIMEOUT"
        Client.ConnectionFailure _e     -> "CONNECTION_FAILURE"
        Client.InvalidStatusLine st     -> n (t "INVALID_STATUS_LINE",st)
        Client.InvalidHeader h          -> n (t "INVALID_HEADER",h)
        Client.InvalidRequestHeader h   -> n (t "INVALID_REQUEST_HEADER",h)
        Client.NoResponseDataReceived   -> "NO_RESPONSE_DATA"
        Client.ResponseBodyTooShort e a -> n (t "RESPONSE_BODY_TOO_SHORT",e,a)
        Client.InvalidChunkHeaders      -> "INVALID_CHUNK_HEADERS"
        Client.IncompleteHeaders        -> "INCOMPLETE_HEADERS"
        Client.InvalidDestinationHost h -> n (t "INVALID_DESTINATION_HOST",h)
        Client.ConnectionClosed         -> "CONNECTION_CLOSED"
        Client.TooManyRedirects rs      -> n (t "TOO_MANY_REDIRECTS", resps)
          where resps = ROW $ fromList (toNoun . loadClientResp <$> rs)

        -- These are errors that should never happen, because we don't
        -- supply the expose the setting that triggers these areas of
        -- the API.
        Client.InternalException{}               -> "INTERNAL_ERROR"
        Client.ProxyConnectException{}           -> "INTERNAL_ERROR"
        Client.TlsNotSupported                   -> "INTERNAL_ERROR"
        Client.WrongRequestBodyStreamSize{}      -> "INTERNAL_ERROR"
        Client.HttpZlibException{}               -> "INTERNAL_ERROR"
        Client.InvalidProxyEnvironmentVariable{} -> "INTERNAL_ERROR"
        Client.InvalidProxySettings{}            -> "INTERNAL_ERROR"
  where
    t = id :: Text -> Text

    n :: forall a. ToNoun a => a -> Fan
    n = toNoun

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall = do
    mCog <- lookup syscall.cog <$> readTVar st.cogs
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
                Just (REQ q)   -> onReq cog q
                Just HEAR      -> onHear cog
                Just (HOLD r)  -> onHold cog r
                Just (SERV ss) -> onServ cog ss
                Just (ECHO r)  -> onEcho cog r syscall.cause
  where
    onReq :: CogState -> ClientReq -> STM (Cancel, [Flow])
    onReq cog req = do
        key <- poolRegister cog.liveClientReqs (req, syscall)
        traceM (show ("key"::Text, key))
        modifyTVar' cog.pendingClientReqs (key:)
        pure (CANCEL (traceM "canceled" >> poolUnregister cog.liveClientReqs key), [])

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
  Just REQ{}             -> "%req"
  Just (HOLD _)          -> "%hold"
  Just (ECHO DYN_RESP{}) -> "%echo"

describeCall :: Vector Fan -> Text
describeCall args = "%http " <> case decodeHttpRequest args of
  Nothing                     -> "UNKNOWN"
  Just SERV{}                 -> "%serv"
  Just HEAR                   -> "%hear"
  Just REQ{}                  -> "%req"
  Just (HOLD n)               -> "%hold " <> tshow n
  Just (ECHO DYN_RESP{reqId}) -> "%echo " <> tshow reqId

createHardwareHttp
    :: Debug
    => FilePath
    -> LmdbStore
    -> (CogId -> WS.ServerApp)
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
        cogs    <- newTVarIO mempty
        manager <- Client.newManager Client.tlsManagerSettings
        pure HW_STATE{store,mach,wsApp,cogs,manager}


-- HTTP Client Stuff -----------------------------------------------------------

type HeaderName = CI Bar
type Header     = (HeaderName, Bar)

data ClientReq = CLIENT_REQ
    { method        :: Bar
    , url           :: Bar
    , headers       :: Array Header
    , body          :: Bar
    , redirectCount :: Nat
    , timeoutMicros :: Nat
    }
  deriving Show

data ClientResp = RESP
    { statusCode :: Nat
    , statusMsg  :: Bar
    , headers    :: Array Header
    , body       :: Bar
    }


--------------------------------------------------------------------------------

instance ToNoun ClientResp where
    toNoun (RESP code msg headers body) =
        ROW $ arrayFromListN 4 $
            [ NAT code
            , BAR msg
            , ROW $ headers <&> \(n,v) -> toNoun (CI.original n, v)
            , mkPin (BAR body)
            ]

instance FromNoun ClientReq where
    fromNoun x = do
        row :: Array Fan <- fromNoun x
        guard (length row == 6)
        CLIENT_REQ <$> fromNoun (row ! 0)
                   <*> fromNoun (row ! 1)
                   <*> (fromHeaders <$> fromNoun (row ! 2))
                   <*> fromNoun (row ! 3)
                   <*> fromNoun (row ! 4)
                   <*> fromNoun (row ! 5)
      where
        fromHeaders :: Array (Bar, Bar) -> Array Header
        fromHeaders heads = heads <&> \(n,v) -> (CI.mk n, v)


mkClientReq :: ClientReq -> IO Client.Request
mkClientReq req = do
    res <- Client.parseRequest (unpack $ decodeUtf8 req.url)

    let tout = if (||) (req.timeoutMicros == 0)
                       (req.timeoutMicros > fromIntegral (maxBound :: Int))
               then Client.responseTimeoutNone
               else Client.responseTimeoutMicro (fromIntegral req.timeoutMicros)

    pure $ res { Client.requestHeaders  = toList req.headers
               , Client.requestBody     = Client.RequestBodyBS req.body
               , Client.method          = req.method
               , Client.responseTimeout = tout
               , Client.redirectCount   = fromIntegral req.redirectCount
               , Client.cookieJar       = Nothing
               }

loadClientResp :: Client.Response LByteString -> ClientResp
loadClientResp r =
    let W.Status scode smsg = r.responseStatus in
    RESP { statusCode = fromIntegral scode
         , statusMsg  = smsg
         , headers    = fromList r.responseHeaders
         , body       = toStrict r.responseBody
         }
