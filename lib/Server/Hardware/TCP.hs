module Server.Hardware.TCP where

import Data.Acquire
import Data.Bits (toIntegralSized)
import Data.Word (Word16)
import Fan.Convert
import Fan.Eval
import Fan.Prof
import PlunderPrelude
import Server.Types.Logging (ProcId)
import Server.Hardware.Types
import Server.Common
import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

type ConnKey = (HostAddress, PortNumber)

data HWState = HW_STATE
    { procs :: TVar (Map ProcId TCPState) }

data TCPState = TCP_STATE
    { listenSocket :: Socket
    , port         :: PortNumber
    , connections  :: TVar (Map ConnKey Socket)
    , hearReqs     :: TQueue SysCall
    , openReqs     :: TQueue (SysCall, HostAddress, PortNumber)
    , takeReqs     :: TQueue (SysCall, HostAddress, PortNumber)
    , giveReqs     :: TQueue (SysCall, HostAddress, PortNumber, ByteString)
    , shutReqs     :: TQueue (SysCall, HostAddress, PortNumber)
    , workers      :: [Async Void]
    }

createHardwareTCP :: Acquire Device
createHardwareTCP = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { start = spinProc st
        , stop = stopProcById st
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
    where
    startup :: IO HWState
    startup = HW_STATE <$> newTVarIO mempty

    shutdown :: HWState -> IO ()
    shutdown st = do
        procs <- atomically (readTVar st.procs)
        for_ procs cancelProc

cancelProc :: TCPState -> IO ()
cancelProc tcp = do
    mapM_ cancel tcp.workers
    close tcp.listenSocket
    mapM_ close =<< readTVarIO tcp.connections

stopProcById :: HWState -> ProcId -> IO ()
stopProcById st procId = do
    maybeProcState <- atomically do
        oldTab <- readTVar st.procs
        writeTVar st.procs (deleteMap procId oldTab)
        pure (lookup procId oldTab)
    maybe pass cancelProc maybeProcState

spinProc :: HWState -> ProcId -> IO ()
spinProc st procId = do
    tcpState <- mdo
        listenSocket <- socket AF_INET Stream defaultProtocol
        setSocketOption listenSocket ReuseAddr 1
        bind listenSocket $ SockAddrInet 0 0
        listen listenSocket 5
        port <- getSocketName listenSocket >>= \case
            SockAddrInet port _ -> pure port
            SockAddrInet6 port _ _ _ -> pure port
            _ -> error "couldn't get tcp port"
        connections <- newTVarIO Map.empty
        hearReqs <- newTQueueIO
        openReqs <- newTQueueIO
        takeReqs <- newTQueueIO
        giveReqs <- newTQueueIO
        shutReqs <- newTQueueIO
        let workers = []
        let st = TCP_STATE{..}
        workers' <- sequence
            [ async $ hearWorker st
            , async $ takeWorker st
            , async $ openWorker st
            , async $ giveWorker st
            , async $ shutWorker st
            ]
        pure st{workers=workers'}

    atomically $ modifyTVar st.procs $ insertMap procId tcpState

runSysCall :: HWState -> ProcId -> SysCall -> STM (Cancel, [Flow])
runSysCall st idx syscall = do
    tcp <- lookup idx <$> readTVar st.procs
    fromMaybe (fillInvalidSyscall syscall $> (CANCEL pass, [])) $
      decodeRequest syscall.args >>= \case
        MINE                 -> onMine syscall <$> tcp
        HEAR                 -> onHear syscall <$> tcp
        OPEN ip port         -> onOpen syscall ip port <$> tcp
        TAKE ip port         -> onTake syscall ip port <$> tcp
        GIVE ip port payload -> onGive syscall ip port payload <$> tcp
        SHUT ip port         -> onShut syscall ip port <$> tcp

decodeRequest :: Vector Fan -> Maybe TCPRequest
decodeRequest = toList <&> \case
    [NAT "mine"]                                -> Just MINE
    [NAT "hear"]                                -> Just HEAR
    [NAT "take", NAT ip, NAT port]              -> uncurry TAKE <$> convertAddr ip port
    [NAT "give", NAT ip, NAT port, BAR payload] -> uncurry GIVE <$> convertAddr ip port <*> pure payload
    [NAT "open", NAT ip, NAT port]              -> uncurry OPEN <$> convertAddr ip port
    [NAT "shut", NAT ip, NAT port]              -> uncurry SHUT <$> convertAddr ip port
    _ -> Nothing
    where
    convertAddr :: Natural -> Natural -> Maybe (HostAddress, PortNumber)
    convertAddr ip port = do
        ip' <- toIntegralSized ip
        port' <- toIntegralSized @Integer @Word16 $ toInteger port
        pure (ip', fromIntegral port')

data TCPRequest
    = MINE
    | HEAR
    | OPEN HostAddress PortNumber
    | TAKE HostAddress PortNumber
    | GIVE HostAddress PortNumber ByteString
    | SHUT HostAddress PortNumber

onMine :: SysCall -> TCPState -> STM (Cancel, [Flow])
onMine syscall TCP_STATE{port} = do
    flow <- writeResponse syscall (NAT $ fromIntegral port)
    pure (CANCEL pass, [flow])

onHear :: SysCall -> TCPState -> STM (Cancel, [Flow])
onHear syscall st = do
    writeTQueue st.hearReqs syscall
    pure (CANCEL pass, [])

onOpen :: SysCall -> HostAddress -> PortNumber -> TCPState -> STM (Cancel, [Flow])
onOpen syscall ip port st = do
    writeTQueue st.openReqs (syscall, ip, port)
    pure (CANCEL pass, [])

onTake :: SysCall -> HostAddress -> PortNumber -> TCPState -> STM (Cancel, [Flow])
onTake syscall ip port st = do
    writeTQueue st.takeReqs (syscall, ip, port)
    pure (CANCEL pass, [])

onGive :: SysCall -> HostAddress -> PortNumber -> ByteString -> TCPState -> STM (Cancel, [Flow])
onGive syscall ip port payload st = do
    writeTQueue st.giveReqs (syscall, ip, port, payload)
    pure (CANCEL pass, [])

onShut :: SysCall -> HostAddress -> PortNumber -> TCPState -> STM (Cancel, [Flow])
onShut syscall ip port st = do
    writeTQueue st.shutReqs (syscall, ip, port)
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
        modifyTVar st.connections $ Map.insert (ip,port) socket
        void $ writeResponse syscall (ip, NAT $ fromIntegral port)

openWorker :: TCPState -> IO Void
openWorker st = forever do
    (syscall, ip, port) <- atomically $ readTQueue st.openReqs
    whenM (Map.member (ip,port) <$> readTVarIO st.connections)
      do void $ atomically $ writeResponse syscall True
    socket <- socket AF_INET Stream defaultProtocol
    try (connect socket $ SockAddrInet port ip) >>= \case
        Left (_ :: IOError) -> do
            close socket
            void $ atomically $ writeResponse syscall False
        Right _ -> do
            atomically do
                modifyTVar st.connections $ Map.insert (ip,port) socket
                void $ writeResponse syscall True

takeWorker :: TCPState -> IO Void
takeWorker st = forever do
    (syscall, ip, port) <- atomically $ readTQueue st.takeReqs
    mconn <- atomically $ Map.lookup (ip,port) <$> readTVar st.connections
    case mconn of
        Just socket -> do
            payload <- recv socket 4096
            atomically do
                when (BS.null payload) $ modifyTVar st.connections $ Map.delete (ip,port)
                void $ writeResponse syscall payload
        Nothing -> void $ atomically $ writeResponse syscall BS.empty

giveWorker :: TCPState -> IO Void
giveWorker st = forever do
    (syscall, ip, port, payload, msocket) <- atomically do
      (syscall, ip, port, payload) <- readTQueue st.giveReqs
      msocket <- Map.lookup (ip,port) <$> readTVar st.connections
      pure (syscall, ip, port, payload, msocket)
    case msocket of
      Nothing -> atomically $ writeResponse syscall (Nothing :: Maybe Nat)
      Just socket -> try (send socket payload) >>= \case
        Left (_ :: IOError) -> atomically do
          modifyTVar st.connections $ Map.delete (ip,port)
          writeResponse syscall (Nothing :: Maybe Nat)
        Right bytesSent -> atomically $
          writeResponse syscall $ Just $ NAT $ fromIntegral bytesSent

shutWorker :: TCPState -> IO Void
shutWorker st = forever do
    (syscall, ip, port, msocket) <- atomically do
      (syscall, ip, port) <- readTQueue st.shutReqs
      msocket <- Map.lookup (ip,port) <$> readTVar st.connections
      modifyTVar st.connections $ Map.delete (ip,port)
      pure (syscall, ip, port, msocket)
    whenJust msocket close
    void $ atomically $ writeResponse syscall ()
