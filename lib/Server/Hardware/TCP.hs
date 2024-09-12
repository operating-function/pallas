module Server.Hardware.TCP where

import Data.Acquire
import Data.Bits (toIntegralSized)
import Data.Word (Word16)
import Fan.Convert
import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Hardware.Types
import Server.Common
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data TCPState = TCP_STATE
    { listenSocket :: Socket
    , port         :: PortNumber
    , connections  :: TVar (IntMap Socket)
    , nextConnId   :: TVar Int
    , hearPool     :: TVar (Pool SysCall)
    , openPool     :: TVar (Pool SysCall)
    , takePool     :: TVar (Pool SysCall)
    , givePool     :: TVar (Pool SysCall)
    , shutPool     :: TVar (Pool SysCall)
    }

createHardwareTCP :: Acquire Device
createHardwareTCP = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { stop     = pass -- const $ stopTCP st
        , call     = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
  where
    startup :: IO TCPState
    startup = do
        listenSocket <- socket AF_INET Stream defaultProtocol
        setSocketOption listenSocket ReuseAddr 1
        bind listenSocket $ SockAddrInet 0 0
        listen listenSocket 5
        port <- getSocketName listenSocket >>= \case
            SockAddrInet port _      -> pure port
            SockAddrInet6 port _ _ _ -> pure port
            _                        -> error "couldn't get tcp port"
        connections <- newTVarIO IntMap.empty
        nextConnId <- newTVarIO 0
        hearPool <- newTVarIO emptyPool
        openPool <- newTVarIO emptyPool
        takePool <- newTVarIO emptyPool
        givePool <- newTVarIO emptyPool
        shutPool <- newTVarIO emptyPool
        let st = TCP_STATE {..}
        void $ async $ hearWorker st -- TODO save handles to shut down
        void $ async $ takeWorker st
        void $ async $ openWorker st
        void $ async $ giveWorker st
        void $ async $ shutWorker st
        pure st

    shutdown :: TCPState -> IO ()
    shutdown st = do
        close st.listenSocket
        mapM_ close =<< readTVarIO st.connections

-- stopTCP :: TCPState -> IO ()
-- stopTCP = shutdown

runSysCall :: TCPState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall = case decodeRequest syscall.args of
    Just MINE                  -> onMine st syscall
    Just HEAR                  -> onHear st syscall
    Just (OPEN ip port)        -> onOpen st syscall ip port
    Just (TAKE handle)         -> onTake st syscall handle
    Just (GIVE handle payload) -> onGive st syscall handle payload
    Just (SHUT handle)         -> onShut st syscall handle
    Nothing                    -> fillInvalidSyscall syscall $> (CANCEL pass, [])

decodeRequest :: Vector Fan -> Maybe TCPRequest
decodeRequest = toList <&> \case
    [NAT "mine"]                          -> Just MINE
    [NAT "hear"]                          -> Just HEAR
    [NAT "take", NAT handle]              -> Just $ TAKE (fromIntegral handle)
    [NAT "give", NAT handle, BAR payload] -> Just $ GIVE (fromIntegral handle) payload
    [NAT "open", NAT ip, NAT port] -> do
        ip <- toIntegralSized ip
        port <- toIntegralSized @Integer @Word16 $ toInteger port
        Just $ OPEN ip (fromIntegral port)
    [NAT "shut", NAT handle]              -> Just $ SHUT (fromIntegral handle)
    _ -> Nothing

data TCPRequest
    = MINE
    | HEAR
    | OPEN HostAddress PortNumber
    | TAKE Int
    | GIVE Int ByteString
    | SHUT Int

onMine :: TCPState -> SysCall -> STM (Cancel, [Flow])
onMine TCP_STATE{port} syscall = do
    flow <- writeResponse syscall (NAT $ fromIntegral port)
    pure (CANCEL pass, [flow])

onHear :: TCPState -> SysCall -> STM (Cancel, [Flow])
onHear st syscall = do
    key <- poolRegister st.hearPool syscall
    pure (CANCEL (poolUnregister st.hearPool key), [])

onOpen :: TCPState -> SysCall -> HostAddress -> PortNumber -> STM (Cancel, [Flow])
onOpen st syscall ip port = do
    key <- poolRegister st.openPool syscall
    pure (CANCEL (poolUnregister st.openPool key), [])

onTake :: TCPState -> SysCall -> Int -> STM (Cancel, [Flow])
onTake st syscall handle = do
    key <- poolRegister st.takePool syscall
    pure (CANCEL (poolUnregister st.takePool key), [])

onGive :: TCPState -> SysCall -> Int -> ByteString -> STM (Cancel, [Flow])
onGive st syscall handle payload = do
    key <- poolRegister st.givePool syscall
    pure (CANCEL (poolUnregister st.givePool key), [])

onShut :: TCPState -> SysCall -> Int -> STM (Cancel, [Flow])
onShut st syscall handle = do
    key <- poolRegister st.shutPool syscall
    pure (CANCEL (poolUnregister st.shutPool key), [])

categoryCall :: Vector Fan -> Text
categoryCall args = "%tcp " <> case decodeRequest args of
  Nothing -> "UNKNOWN"
  Just MINE -> "%mine"
  Just HEAR -> "%hear"
  Just OPEN{} -> "%open"
  Just TAKE{} -> "%take"
  Just GIVE{} -> "%give"
  Just SHUT{} -> "%shut"

describeCall :: Vector Fan -> Text
describeCall args = "%tcp " <> case decodeRequest args of
  Nothing -> "UNKNOWN"
  Just MINE -> "%mine"
  Just HEAR -> "%hear"
  Just OPEN{} -> "%open" -- TODO args
  Just TAKE{} -> "%take" -- TODO args
  Just GIVE{} -> "%give" -- TODO args
  Just SHUT{} -> "%shut" -- TODO args

hearWorker :: TCPState -> IO Void
hearWorker st = forever do
    syscall <- atomically $ poolTakeNext st.hearPool \syscall ->
        case decodeRequest syscall.args of
          Just HEAR -> pure syscall
          _         -> error "Unexpected syscall in hearPool"
    (socket, SockAddrInet port ip) <- accept st.listenSocket -- TODO IPv6
    atomically do
        connId <- stateTVar st.nextConnId \prev -> (prev, prev+1)
        modifyTVar st.connections $ IntMap.insert connId socket
        void $ writeResponse syscall (NAT $ fromIntegral connId, ip, NAT $ fromIntegral port)

openWorker :: TCPState -> IO Void
openWorker st = forever do
    (syscall, ip, port) <- atomically $ poolTakeNext st.openPool $ \syscall ->
        case decodeRequest syscall.args of
            Just (OPEN ip port) -> pure (syscall, ip, port)
            _                   -> error "Unexpected syscall in openPool"
    socket <- socket AF_INET Stream defaultProtocol
    connect socket $ SockAddrInet port ip
    atomically do
        connId <- readTVar st.nextConnId
        writeTVar st.nextConnId (connId + 1)
        modifyTVar st.connections $ insertMap connId socket
        void $ writeResponse syscall (NAT $ fromIntegral connId)

takeWorker :: TCPState -> IO ()
takeWorker st = forever do
    (syscall, socket, connId) <- atomically $ poolTakeNext st.takePool $ \syscall ->
        case decodeRequest syscall.args of
            Just (TAKE connId) -> do
                mconn <- IntMap.lookup connId <$> readTVar st.connections
                case mconn of
                    Just socket -> pure (syscall, socket, connId)
                    Nothing   -> retry
            _ -> error "Unexpected syscall in takePool"
    payload <- recv socket 4096
    atomically do
        when (BS.null payload) $ modifyTVar st.connections $ deleteMap connId
        void $ writeResponse syscall payload

giveWorker :: TCPState -> IO Void
giveWorker st = forever do
    (syscall, socket, payload) <- atomically $ poolTakeNext st.givePool $ \syscall ->
        case decodeRequest syscall.args of
            Just (GIVE handle payload) -> do
                msocket <- IntMap.lookup handle <$> readTVar st.connections
                case msocket of
                    Just socket -> pure (syscall, socket, payload)
                    Nothing     -> retry
            _ -> error "Unexpected syscall in givePool"
    sendAll socket payload -- TODO catch exception
    void $ atomically $ writeResponse syscall ()

shutWorker :: TCPState -> IO Void
shutWorker st = forever do
    (syscall, socket) <- atomically $ poolTakeNext st.shutPool $ \syscall ->
        case decodeRequest syscall.args of
            Just (SHUT handle) -> (syscall,) <$>
                stateTVar st.connections
                  (updateLookupWithKey (\_ _ -> Nothing) handle)
            _ -> error "Unexpected syscall in shutPool"
    mapM_ close socket
    void $ atomically $ writeResponse syscall ()
