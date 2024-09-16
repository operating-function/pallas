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
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data TCPState = TCP_STATE
    { listenSocket :: Socket
    , port         :: PortNumber
    , connections  :: TVar (IntMap Socket)
    , nextConnId   :: TVar Int
    , hearReqs:: TQueue SysCall
    , openReqs     :: TQueue (SysCall, HostAddress, PortNumber)
    , takeReqs     :: TQueue (SysCall, Int)
    , giveReqs     :: TQueue (SysCall, Int, ByteString)
    , shutReqs     :: TQueue (SysCall, Int)
    }

createHardwareTCP :: Acquire Device
createHardwareTCP = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { stop     = pass -- TODO ??
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
        hearReqs <- newTQueueIO
        openReqs <- newTQueueIO
        takeReqs <- newTQueueIO
        giveReqs <- newTQueueIO
        shutReqs <- newTQueueIO
        let st = TCP_STATE {..}
        async $ hearWorker st -- TODO save handles to shut down
        async $ takeWorker st
        async $ openWorker st
        async $ giveWorker st
        async $ shutWorker st
        pure st

    shutdown :: TCPState -> IO ()
    shutdown st = do
        close st.listenSocket
        mapM_ close =<< readTVarIO st.connections

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
    writeTQueue st.hearReqs syscall
    pure (CANCEL pass, [])

onOpen :: TCPState -> SysCall -> HostAddress -> PortNumber -> STM (Cancel, [Flow])
onOpen st syscall ip port = do
    writeTQueue st.openReqs (syscall, ip, port)
    pure (CANCEL pass, [])

onTake :: TCPState -> SysCall -> Int -> STM (Cancel, [Flow])
onTake st syscall handle = do
    writeTQueue st.takeReqs (syscall, handle)
    pure (CANCEL pass, [])

onGive :: TCPState -> SysCall -> Int -> ByteString -> STM (Cancel, [Flow])
onGive st syscall handle payload = do
    writeTQueue st.giveReqs (syscall, handle, payload)
    pure (CANCEL pass, [])

onShut :: TCPState -> SysCall -> Int -> STM (Cancel, [Flow])
onShut st syscall handle = do
    writeTQueue st.shutReqs (syscall, handle)
    pure (CANCEL pass, [])

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
    syscall <- atomically $ readTQueue st.hearReqs
    (socket, SockAddrInet port ip) <- accept st.listenSocket -- TODO IPv6
    atomically do
        connId <- stateTVar st.nextConnId \prev -> (prev, prev+1)
        modifyTVar st.connections $ insertMap connId socket
        void $ writeResponse syscall (NAT $ fromIntegral connId, ip, NAT $ fromIntegral port)

openWorker :: TCPState -> IO Void
openWorker st = forever do
    (syscall, ip, port) <- atomically $ readTQueue st.openReqs
    socket <- socket AF_INET Stream defaultProtocol
    try (connect socket $ SockAddrInet port ip) >>= \case
        Left (_ :: IOError) -> do -- TODO propagate errors?
            close socket
            void $ atomically $ writeResponse syscall (Nothing :: Maybe Nat)
        Right _ -> do
            atomically do
                connId <- stateTVar st.nextConnId \prev -> (prev, prev+1)
                modifyTVar st.connections $ insertMap connId socket
                void $ writeResponse syscall (Just $ NAT $ fromIntegral connId)

takeWorker :: TCPState -> IO ()
takeWorker st = forever do
    (syscall, connId) <- atomically $ readTQueue st.takeReqs
    mconn <- atomically $ IntMap.lookup connId <$> readTVar st.connections
    case mconn of
        Just socket -> do
            payload <- recv socket 4096
            atomically do
                when (BS.null payload) $ modifyTVar st.connections $ deleteMap connId
                void $ writeResponse syscall payload
        Nothing -> void $ atomically $ writeResponse syscall BS.empty

giveWorker :: TCPState -> IO Void
giveWorker st = forever do
    (syscall, handle, payload, msocket) <- atomically do
      (syscall, handle, payload) <- readTQueue st.giveReqs
      msocket <- lookup handle <$> readTVar st.connections
      pure (syscall, handle, payload, msocket)
    case msocket of
      Nothing -> atomically $ writeResponse syscall (Nothing :: Maybe Nat)
      Just socket -> try (send socket payload) >>= \case
        Left (_ :: IOError) -> atomically do
          modifyTVar st.connections $ deleteMap handle
          writeResponse syscall (Nothing :: Maybe Nat)
        Right bytesSent -> atomically $
          writeResponse syscall $ Just $ NAT $ fromIntegral bytesSent

shutWorker :: TCPState -> IO Void
shutWorker st = forever do
    (syscall, msocket) <- atomically do
      (syscall, handle) <- readTQueue st.shutReqs
      (syscall,) <$>
        stateTVar st.connections (updateLookupWithKey (\_ _ -> Nothing) handle)
    whenJust msocket close
    void $ atomically $ writeResponse syscall ()
