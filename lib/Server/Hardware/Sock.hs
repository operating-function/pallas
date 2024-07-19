-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-|
    Hardware Device 2 -- Telenet Server

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


    ----------------------------------------------------------------------------

    -   TODO Put the ports file somewhere more sensible.

    -   TODO There should probably be an `Int` key to refer to a machine,
        to avoid using this string as a key for every single request.

-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall            #-}
{-# OPTIONS_GHC -Werror          #-}

module Server.Hardware.Sock where

{-
import Fan.Eval
import PlunderPrelude
import Server.Debug
import Server.Hardware.Types

import Data.Acquire         (Acquire, mkAcquire)
import Server.Types.Logging (CogName(..))
import System.Directory     (removeFile)
import System.IO.Error      (catchIOError)

import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N


--------------------------------------------------------------------------------

data RawRequest = RR
    { cogName  :: !CogName
    , vals     :: !(Vector Fan)
    , callback :: !(TVar (Maybe (Fan -> STM ())))
    }

data SockRequest
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
    { socket   :: N.Socket
    , isShut   :: TVar Bool
    , waits    :: TVar [Callback Bool]
    , sends    :: TVar [(ByteString, Callback Bool)]
    , recvs    :: TVar [Callback (Maybe ByteString)]
    , receiver :: TVar (Maybe (Async ()))
    , sender   :: TVar (Maybe (Async ()))
    }

data CogState = COG_STATE
    { portsFile    :: FilePath
    , listeners    :: TVar [Callback Nat]
    , nextHandle   :: TVar Nat
    , liveHandles  :: TVar (Map Nat HandleState)
    , listenSocket :: N.Socket
    , listenPort   :: N.PortNumber
    , listenThread :: Async Void
    }

data SockState = STATE
    { mach :: FilePath
    , cogs :: TVar (Map CogName CogState)
    , inpQ :: TBQueue RawRequest
    , inpT :: Async Void
    }

--------------------------------------------------------------------------------

decodeSockRequest :: Vector Fan -> Maybe SockRequest
decodeSockRequest = dec . toList
  where
    dec ["open"]                    = pure OPEN
    dec ["shut", NAT sock]          = pure (SHUT sock)
    dec ["wait", NAT sock]          = pure (WAIT sock)
    dec ["recv", NAT sock]          = pure (RECV sock)
    dec ["send", NAT sock, BAR byt] = pure (SEND sock byt)
    dec _                           = Nothing

--------------------------------------------------------------------------------

-- | A shut request kills the handle, and causes all other operations
-- on that handle to error out.
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
                    pure (N.close hs.socket)

cancelHandle :: CogState -> Nat -> HandleState -> STM ()
cancelHandle cog h hs = do
    modifyTVar' cog.liveHandles (deleteMap h)
    recvs <- swapTVar hs.recvs []
    sends <- swapTVar hs.sends []
    waits <- swapTVar hs.waits []
    for_ recvs \v     -> callback v Nothing
    for_ sends \(_,v) -> callback v False
    for_ waits \v     -> callback v False


--------------------------------------------------------------------------------

data ConnectionClosed = ConnectionClosed
  deriving (Show, Exception)

createHardwareSock :: Debug => FilePath -> Acquire HardwareFun
createHardwareSock machineDir = do
    tt <- mkAcquire (startAll machineDir) shutdown
    pure \m v k -> writeTBQueue tt.inpQ (RR m v k)

-- Shut Down -------------------------------------------------------------------

shutdown :: SockState -> IO ()
shutdown st = do
    cogs <- atomically (readTVar st.cogs)
    cancel st.inpT
    for_ cogs \cog -> do
        catchIOError (removeFile cog.portsFile) (const pass)
        N.close cog.listenSocket
        cancel cog.listenThread
        liveHands <- atomically (readTVar cog.liveHandles)
        for_ liveHands \hs -> N.close hs.socket

-- Start Up --------------------------------------------------------------------

startCog :: Debug => FilePath -> CogName -> IO CogState
startCog machineDir cogName = do
    nextHandle  <- newTVarIO 1
    liveHandles <- newTVarIO mempty
    listeners   <- newTVarIO mempty

    -- TODO Only accept input from localhost
    let localhost = N.tupleToHostAddress (0x7f, 0, 0, 1)
    let flags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
    let tcp   = 6
    let addr  = (N.SockAddrInet 0 localhost)
    let ainfo = N.AddrInfo flags N.AF_INET N.Stream tcp addr Nothing
    listenSocket <- N.openSocket ainfo
    N.listen listenSocket 5
    listenPort <- N.socketPort listenSocket
    debugVal (cogName.txt <> "_telnet_port") (fromIntegral listenPort :: Nat)

    let baseName  = cogName.txt <> ".telnet.port"
    let portsFile = machineDir </> unpack baseName

    debugTextVal (cogName.txt <> "_telnet_port_file") (pack portsFile)

    writeFileUtf8 portsFile (tshow listenPort)

    mdo
        let newCog = COG_STATE{..}
        listenThread <- async $ forever (acceptConnections listenSocket newCog)
        pure newCog

startAll :: Debug => FilePath -> IO SockState
startAll mach = do
    inpQ <- newTBQueueIO 100
    cogs <- newTVarIO mempty

    mdo inpT <- async $ forever $ do
                    r <- atomically (readTBQueue inpQ)
                    routeRequest st r
        let st = STATE{..}
        pure st

-- Accept New Connections ------------------------------------------------------

acceptConnections :: Debug => N.Socket -> CogState -> IO a
acceptConnections sok cog = forever do
    (socket, sockAddr) <- N.accept sok
    debugText "TELNET: GOT ONE"

    let loop [] = pure Nothing
        loop (k : ks) = do
            isCanceled k >>= \case
                True  -> loop ks
                False -> do
                    (key, hSt) <- newHandle cog socket sockAddr
                    callback k key
                    pure (Just (key, hSt))

    mHan <- atomically (readTVar cog.listeners >>= loop)

    case mHan of
        Nothing -> do
            N.close socket
            debugText "Sock Connection Rejected because no WAIT request"
        Just (_key, hSt) -> do
            !receiver <- async (recvLoop hSt)
            !sender   <- async (sendLoop hSt)
            atomically $ do
                writeTVar hSt.receiver $! Just receiver
                writeTVar hSt.sender   $! Just sender

{-
    Allocate a new handle and insert it into the state, bumping
    `SockState.nextHandle`.

    This returns a `HandleState` with empty (`Nothing`) threads, we
    expect the caller to create threads and fill them in.
-}
newHandle :: CogState -> N.Socket -> N.SockAddr -> STM (Nat, HandleState)
newHandle cog socket _addr = do
    key <- readTVar cog.nextHandle
    writeTVar cog.nextHandle (key+1)

    waits    <- newTVar []
    sends    <- newTVar []
    recvs    <- newTVar []
    isShut   <- newTVar False
    sender   <- newTVar Nothing
    receiver <- newTVar Nothing
    let hSt = HANDLE{..}
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
    debugText "RESOLVING ALL WAIT REQUESTS"
    atomically do
        writeTVar hSt.isShut True
        ws <- swapTVar hSt.waits []
        for_ ws \w -> callback w True

recvLoop :: Debug => HandleState -> IO ()
recvLoop hSt =
    bracket (pure hSt) closeHandle (forever . recvOne)

recvOne :: Debug => HandleState -> IO ()
recvOne hSt = do
    sht <- atomically (readTVar hSt.isShut)
    when sht $ do
        debugText "CONNECTION CLOSED"
        throwIO ConnectionClosed
    buf <- N.recv hSt.socket 1024
    when (length buf == 0) do
        debugText "CONNECTION CLOSED"
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
    debugText "SENDING"
    try (N.sendAll hSt.socket bs) >>= \case
        Right ()                -> do debugText "SENT"
                                      atomically (callback k True)
        Left (e::SomeException) -> do atomically (callback k False)
                                      throwIO e

sendOne :: Debug => HandleState -> IO ()
sendOne hSt = do
    debugText "WAITING TO SEND"
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

--
-- TODO Handle Race condition here.
--
--  This needs to take a lock on the cogName so that two simultaneous
--  requests to the same cog cannot both create new CogStates.
--
getCog :: Debug => SockState -> CogName -> IO CogState
getCog st cogName = do
    cogs <- atomically (readTVar st.cogs)
    case lookup cogName cogs of
        Just cog -> pure cog
        Nothing -> do
            cog <- startCog st.mach cogName
            atomically $ modifyTVar' st.cogs (insertMap cogName cog)
            pure cog

routeRequest :: Debug => SockState -> RawRequest -> IO ()
routeRequest st raw = do
    cog <- getCog st raw.cogName

    case decodeSockRequest raw.vals of
        Nothing         -> atomically (callback (CB id act) (NAT 0))
        Just (SHUT h)   -> join $ atomically (reqShut cog h)
        Just OPEN       -> atomically $ reqOpen cog
        Just (WAIT h)   -> atomically $ reqWait cog h
        Just (RECV h)   -> atomically $ reqRecv cog h
        Just (SEND h b) -> atomically $ reqSend cog h b

  where
    act = raw.callback

    reqShut :: CogState -> Nat -> STM (IO ())
    reqShut cog h = do
        let f = \case { True -> NAT 1; False -> NAT 0 }
        onShut (CB f act) h cog

    reqOpen :: CogState -> STM ()
    reqOpen cog = do
        let f = NAT
        modifyTVar cog.listeners (CB f act :)

    reqWait :: CogState -> Nat -> STM ()
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
