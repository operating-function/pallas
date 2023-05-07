-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-|
    Hardware Device 4 -- WebSocket Server

    OPEN = 0       :: IO Handle              -- Returns when socket is opened.
    SHUT = [1 h]   :: Handle -> IO ()        -- Returns when the socket closes.
    WAIT = [2 h]   :: Handle -> IO ()        -- Returns when socket dies.
    RECV = [3 h]   :: Handle -> IO Bar       -- Returns with data.
    SEND = [4 h b] :: Handle -> Bar -> IO () -- Returns after write is sent

    - Open a socket on localhost (port=0).
    - Figure out what port we got.
    - Send a .ports file.
    - Wait for connections.
    - When a connection comes in, redirect it to an active LISTEN request.
    - If there are no active LISTEN requests, just close the socket.
-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall            #-}
{-# OPTIONS_GHC -Werror          #-}

module Server.Hardware.Wock where

{-
import Fan.Eval
import PlunderPrelude
import Server.Debug
import Server.Hardware.Types

import Control.Concurrent   (threadDelay)
import Data.Acquire         (Acquire, mkAcquire)
import Server.Types.Logging (CogName)

import qualified Network.WebSockets as WS

--------------------------------------------------------------------------------

data RawRequest = RR
    { mach     :: !CogName
    , vals     :: !(Vector Fan)
    , callback :: !(TVar (Maybe (Fan -> STM ())))
    }

data WockReq
    = OPEN
    | WAIT Nat
    | RECV Nat
    | SEND Nat ByteString
    | SHUT Nat

{-
    The list of cancelable construct is strange, but these lists almost
    always have lengths zero or one.

    For example, if there are multiple simultaneous send requests to
    the same handle, there is no guarentee about what order they should
    be executed in.  We just use more-recent-first order since that has
    the most straight-forward code.

    It might be good to revisit this later and use a more sophisticated
    approach, but it probably has no effect in practice.  For example,
    if a machine continueous creates waits and then cancels them,
    then that will accumulate a huge list of canceled waits, only cleared
    when the socket closes.  However, this this shouldn't happen in practice.

    There is no queue of shuts.  Shuts are executed immediatly.  In a
    single transaction, all live requests get error responsesand the
    handle is shutdown.  Then the socket is closed.

    `sender` and `receiver` are `Maybe` values only because we cant
    create these things in the STM transaction that create the HANDLE.
    We create the handle, run the threads against it, and then fill in
    the Maybe values.  They are only `Nothing` during handle initialization.
-}
data HandleState = HANDLE
    { socket   :: WS.Connection
    , isShut   :: TVar Bool
    , waits    :: TVar [Callback Bool]
    , sends    :: TVar [(ByteString, Callback Bool)]
    , recvs    :: TVar [Callback (Maybe ByteString)]
    , receiver :: TVar (Maybe (Async ()))
    , sender   :: TVar (Maybe (Async ()))
    }

data CogState = COG_STATE
    { listeners   :: TVar [Callback Nat]
    , nextHandle  :: TVar Nat
    , liveHandles :: TVar (Map Nat HandleState)
    }

data HWState = STATE
    { cogs         :: TVar (Map CogName CogState)
    , listenSocket :: MVar (CogName, WS.Connection, TVar Bool)
    , listenThread :: Async Void
    , inputThread  :: Async Void
    , inputQueue   :: TBQueue RawRequest
    }

--------------------------------------------------------------------------------

decodeWockRequest :: Vector Fan -> Maybe WockReq
decodeWockRequest = dec . toList
  where
    dec ["open"]                    = pure OPEN
    dec ["shut", NAT sock]          = pure (SHUT sock)
    dec ["wait", NAT sock]          = pure (WAIT sock)
    dec ["recv", NAT sock]          = pure (RECV sock)
    dec ["send", NAT sock, BAR byt] = pure (SEND sock byt)
    dec _                           = Nothing


--------------------------------------------------------------------------------

-- |A shut request kills the handle, and causes all other operations on
-- that handle to error out.
onShut :: Callback Bool -> Nat -> CogState -> STM (IO ())
onShut cb h cog = do
    readTVar cb.act >>= \case
        Nothing -> pure pass
        Just{}  -> do
            hand <- readTVar cog.liveHandles
            case lookup h hand of
                Nothing -> do
                    callback cb False
                    pure pass
                Just hs -> do
                    cancelHandle cog h hs
                    callback cb True
                    pure (wsClose "SHUT" hs.socket)

cancelHandle :: CogState -> Nat -> HandleState -> STM ()
cancelHandle cog h hs = do
    recvs <- swapTVar hs.recvs []
    sends <- swapTVar hs.sends []
    waits <- swapTVar hs.waits []
    modifyTVar' cog.liveHandles (deleteMap h)
    for_ recvs \v     -> callback v Nothing
    for_ sends \(_,v) -> callback v False
    for_ waits \v     -> callback v False



--------------------------------------------------------------------------------

data ConnectionClosed = ConnectionClosed
  deriving (Show, Exception)

createHardwareWock
    :: Debug
    => Acquire (HardwareFun, CogName -> WS.ServerApp)
createHardwareWock = do
    debugText "websocket_startup"
    st <- mkAcquire startAll shutdown
    let cb = \m v k -> writeTBQueue st.inputQueue (RR m v k)
    pure (cb, wsApp st.listenSocket)

wsApp
    :: Debug
    => MVar (CogName, WS.Connection, TVar Bool)
    -> CogName
    -> WS.PendingConnection
    -> IO ()
wsApp chan cogName conn = do
    debugText "WEBSOCKET: Incoming connection"
    debugText ("WEBSOCKET: " <> tshow (WS.pendingRequest conn))
    sock <- WS.acceptRequest conn
    isShut <- newTVarIO False
    debugText "WEBSOCKET: Hello!"
    putMVar chan (cogName, sock, isShut)
    debugText "WEBSOCKET: Put that shit."

    let logPing = debugText "WEBSOCKET: ping"
    WS.withPingThread sock 30 logPing do
        atomically $ readTVar isShut >>= \case
            False -> retry
            True  -> pass

-- Shut Down -------------------------------------------------------------------

shutdown :: HWState -> IO ()
shutdown st = do
    cancel st.inputThread
    cancel st.listenThread
    cogs <- atomically (readTVar st.cogs)
    for_ cogs \cog -> do
        liveHands <- atomically (readTVar cog.liveHandles)
        for_ liveHands \hs -> wsClose "DYING" hs.socket


-- Start Up --------------------------------------------------------------------

startAll :: Debug => IO HWState
startAll = mdo
    inputQueue   <- newTBQueueIO 100
    listenSocket <- newEmptyMVar
    cogs         <- newTVarIO mempty

    listenThread <- async $ forever $ acceptConnections listenSocket st
    inputThread  <- async $ forever do
                        r <- atomically(readTBQueue inputQueue)
                        routeRequest st r

    let st = STATE{..}
    pure st


-- Accept New Connections ------------------------------------------------------

-- TODO: Not sure how to handle this correctly.  We are supposed to
-- suggest a close, and then wait for the other side to close the
-- connection?  How does that fit with the rest of this flow
wsClose :: Text -> WS.Connection -> IO ()
wsClose reason conn =
    void $ async do
        WS.sendClose conn reason
        forever (void $ WS.receiveDataMessage conn)

startCog :: HWState -> CogName -> IO CogState
startCog _ _ = do
    nextHandle  <- newTVarIO 1
    liveHandles <- newTVarIO mempty
    listeners   <- newTVarIO mempty
    pure COG_STATE{..}

getCog :: HWState -> CogName -> IO CogState
getCog st cogName = do
    cogs <- atomically (readTVar st.cogs)
    case lookup cogName cogs of
        Just cog -> pure cog
        Nothing -> do
            cog <- startCog st cogName
            atomically $ modifyTVar' st.cogs (insertMap cogName cog)
            pure cog

{-
   Accept a single connection
-}
acceptConnections
    :: Debug
    => MVar (CogName, WS.Connection, TVar Bool)
    -> HWState
    -> IO a
acceptConnections sok st = forever do
    debugText "websocket_wait_for_connection"
    (cogName, socket, isShut) <- takeMVar sok
    debugText "websocket_got_connection"

    debugText "websocket_writing_hello_handshake"
    res <- try (WS.sendTextData socket ("Welcome"::Text))
    debugText (tshow (res :: Either SomeException ()))

    cog <- getCog st cogName

    let loop [] = pure Nothing
        loop (k : ks) = do
            isCanceled k >>= \case
                True  -> loop ks
                False -> do
                    (key, hSt) <- newHandle cog socket isShut
                    callback k key
                    pure (Just (key, hSt))

    mHan <- atomically (readTVar cog.listeners >>= loop)

    case mHan of
        Nothing -> do
            wsClose "Nobody is listening" socket
            let rej = ( "WEBSOCKET: "
                     <> "WebSocket Connection Rejected because no WAIT request"
                      )
            debugText rej
        Just (_key, hSt) -> do
            debug ["websocket_accepted_connection", "spawn_ze_threads"::Text]
            !receiver <- async (recvLoop hSt)
            threadDelay 1000
            !sender   <- async (sendLoop hSt)
            atomically $ do
                writeTVar hSt.receiver $! Just receiver
                writeTVar hSt.sender   $! Just sender

{-
    Allocate a new handle and insert it into the state, bumping
    `HWState.nextHandle`.

    This returns a `HandleState` with empty (`Nothing`) threads, we
    expect the caller to create threads and fill them in.
-}
newHandle
    :: CogState -> WS.Connection -> TVar Bool
    -> STM (Nat, HandleState)
newHandle cog socket isShut = do
    key <- readTVar cog.nextHandle
    writeTVar cog.nextHandle (key+1)

    waits    <- newTVar []
    sends    <- newTVar []
    recvs    <- newTVar []
    sender   <- newTVar Nothing
    receiver <- newTVar Nothing
    let hSt = HANDLE{isShut,socket,waits,sends,recvs,sender,receiver}
    modifyTVar' cog.liveHandles (insertMap key hSt)
    pure (key, hSt)

{-|
    This happens when a a socket gets closed on the remote end.  We move
    the handle into a "shut" state and wait for the machine to explicitly
    KILL it.

    -    Trigger all `WAIT` requests that are waiting for us to close.

    -    Mark the handle as closed.  This causes all current and future
         SEND and RECV requests to fail.
-}
closeHandle :: Debug => HandleState -> IO ()
closeHandle hSt = do
    debugText "websocket_respond_to_all_wait_request"
    atomically do

        writeTVar hSt.isShut True
        ws <- swapTVar hSt.waits []
        for_ ws \w -> callback w True

recvLoop :: Debug => HandleState -> IO ()
recvLoop hSt = do
    bracket (pure hSt) closeHandle (forever . recvOne)

recvOne :: Debug => HandleState -> IO ()
recvOne hSt = do
    sht <- atomically (readTVar hSt.isShut)
    when sht $ do
        debugText "WEBSOCKET: CONNECTION CLOSED"
        throwIO ConnectionClosed
    debugText "WEBSOCKET: WAITING FOR DATA FROM WEBSOCKET"
    buf <- WS.receiveData hSt.socket
    when (length buf == 0) do
        debugText "WEBSOCKET: CONNECTION CLOSED"
        throwIO ConnectionClosed
    atomically do
        let loop []     = retry
            loop (r:rs) =
                isCanceled r >>= \case
                    True  -> loop rs
                    False -> do
                        callback r (Just buf)
                        writeTVar hSt.recvs rs
        readTVar hSt.isShut >>= \case
            True  -> pass
            False -> readTVar hSt.recvs >>= loop

sendLoop :: Debug => HandleState -> IO ()
sendLoop hSt = do
    bracket (pure hSt) closeHandle (forever . sendOne)

doSend :: Debug => HandleState -> ByteString -> Callback Bool -> IO ()
doSend hSt bs k = do
    debugText "WEBSOCKET: SENDING"
    try (WS.sendBinaryData hSt.socket bs) >>= \case
        Right ()                -> do debugText "WEBSOCKET: SENT"
                                      atomically (callback k True)
        Left (e::SomeException) -> do atomically (callback k False)
                                      throwIO e

sendOne :: Debug => HandleState -> IO ()
sendOne hSt = do
    debugText "WEBSOCKET: WAITING TO SEND"
    join $ atomically do
        sends <- readTVar hSt.sends
        isSht <- readTVar hSt.isShut
        case (isSht, sends) of
            (True, _)           -> pure (throwIO ConnectionClosed)
            (False, [])         -> retry
            (False, ((b,k):ss)) -> do writeTVar hSt.sends ss
                                      isCanceled k <&> \case
                                          True  -> pass
                                          False -> doSend hSt b k


-- Handling Incoming Requests --------------------------------------------------

routeRequest :: HWState -> RawRequest -> IO ()
routeRequest st rr = do
    cog <- getCog st rr.mach
    case decodeWockRequest rr.vals of
        Nothing         -> atomically (callback (CB id act) (NAT 0))
        Just (SHUT h)   -> join $ atomically $ reqShut cog h
        Just OPEN       -> atomically $ reqOpen cog
        Just (WAIT h)   -> atomically $ reqWait cog h
        Just (RECV h)   -> atomically $ reqRecv cog h
        Just (SEND h b) -> atomically $ reqSend cog h b
  where
    act = rr.callback

    reqShut :: CogState -> Nat -> STM (IO ())
    reqShut cog h = do
        let f = \case { True -> NAT 1; False -> NAT 0 }
        onShut (CB f act) h cog

    reqOpen cog = do
        let f = NAT
        modifyTVar cog.listeners (CB f act :)

    reqWait cog h = do
        hands <- readTVar cog.liveHandles
        let f = \case { True -> NAT 1; False -> NAT 0 }
        let k = CB f act
        case lookup h hands of
            Nothing -> callback k False
            Just hs ->
                 readTVar hs.isShut >>= \case
                    True  -> callback k True
                    False -> modifyTVar' hs.waits (k:)

    reqRecv :: CogState -> Nat -> STM ()
    reqRecv cog h = do
        let k = CB (maybe (NAT 0) BAR) act
        hands <- readTVar cog.liveHandles
        case lookup h hands of
            Nothing -> callback k Nothing
            Just hs -> do modifyTVar hs.recvs (k:)

    reqSend :: CogState -> Nat -> ByteString -> STM ()
    reqSend cog h b = do
        let f = \case { False -> NAT 0; True -> NAT 1 }
        let k = CB f act
        hands <- readTVar cog.liveHandles
        case lookup h hands of
            Nothing -> callback k False
            Just hs -> modifyTVar' hs.sends ((b,k):)
-}
