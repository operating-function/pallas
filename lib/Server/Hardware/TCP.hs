module Server.Hardware.TCP where

import Data.Acquire
import Data.Bits (toIntegralSized)
import Data.Word (Word16)
import Fan.Convert
import Fan.Eval
import Data.Monoid
import Fan.Prof
import GHC.IO.Exception (IOErrorType(..))
import PlunderPrelude
import Server.Types.Logging (ProcId)
import Server.Hardware.Types
import Server.Common
import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap

type ConnKey = (HostAddress, PortNumber)

data HWState = HW_STATE
    { procs :: TVar (Map ProcId TCPState) }

data TCPState = TCP_STATE
    { listenSocket :: Socket
    , port :: PortNumber
    , connections :: TVar (IntMap Socket)
    , hearReqs :: TQueue (SysCall, ())
    , openReqs :: TQueue (SysCall, (HostAddress, PortNumber))
    , takeReqs :: TQueue (SysCall, Int)
    , giveReqs :: TQueue (SysCall, (Int, ByteString))
    , shutReqs :: TQueue (SysCall, Int)
    , workers :: [Async Void]
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
        procs <- readTVarIO st.procs
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
    tcpState <- do
        listenSocket <- socket AF_INET Stream defaultProtocol
        setSocketOption listenSocket ReuseAddr 1
        bind listenSocket $ SockAddrInet 0 0
        listen listenSocket 5
        port <- getSocketName listenSocket >>= \case
            SockAddrInet port _ -> pure port
            SockAddrInet6 port _ _ _ -> pure port
            _ -> error "couldn't get tcp port"
        connections <- newTVarIO mempty
        hearReqs <- newTQueueIO
        openReqs <- newTQueueIO
        takeReqs <- newTQueueIO
        giveReqs <- newTQueueIO
        shutReqs <- newTQueueIO
        let workers = []
        let st = TCP_STATE{..}
        workers' <- sequence
            [ async $ hearWorker st
            , async $ openWorker st
            , async $ takeWorker st
            , async $ giveWorker st
            , async $ shutWorker st
            ]
        pure st{workers=workers'}

    atomically $ modifyTVar st.procs $ insertMap procId tcpState

runSysCall :: HWState -> ProcId -> SysCall -> STM [Flow]
runSysCall st idx syscall = do
    mTcp <- lookup idx <$> readTVar st.procs
    fromMaybe (fillInvalidSyscall syscall $> []) $ do
      tcp@TCP_STATE{..} <- mTcp
      decodeRequest syscall.args <&> \case
        MINE                 -> onMine syscall tcp.port
        HEAR                 -> queueSysCall hearReqs (syscall,())
        OPEN ip port         -> queueSysCall openReqs (syscall,(ip,port))
        TAKE sockId          -> queueSysCall takeReqs (syscall,sockId)
        GIVE sockId payload  -> queueSysCall giveReqs (syscall,(sockId,payload))
        SHUT sockId          -> queueSysCall shutReqs (syscall,sockId)

decodeRequest :: Vector Fan -> Maybe TCPRequest
decodeRequest = toList <&> \case
    [NAT "mine"]                               -> Just MINE
    [NAT "hear"]                               -> Just HEAR
    [NAT "open", NAT ip, NAT port]             -> uncurry OPEN <$> convertAddr ip port
    [NAT "take", NAT sockId]                   -> Just $ TAKE (fromIntegral sockId)
    [NAT "give", NAT sockId, BAR payload]      -> Just $ GIVE (fromIntegral sockId) payload
    [NAT "shut", NAT sockId]                   -> Just $ SHUT (fromIntegral sockId)
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
    | TAKE Int
    | GIVE Int ByteString
    | SHUT Int

onMine :: SysCall -> PortNumber -> STM [Flow]
onMine syscall port = do
    flow <- writeResponse syscall (NAT $ fromIntegral port)
    pure []

queueSysCall :: TQueue a -> a -> STM [Flow]
queueSysCall queue req = writeTQueue queue req $> []

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

insertNewSocket :: Socket -> TVar (IntMap Socket) -> STM Int
insertNewSocket socket connections = do 
    conns <- readTVar connections
    let newId = if IntMap.null conns then 0 else 1 + fst (IntMap.findMax conns)
    modifyTVar connections $ IntMap.insert newId socket
    pure newId

worker :: ToNoun o => TQueue (SysCall, i) -> (i -> IO o) -> IO Void
worker q f = forever $
    atomically (flushNonEmptyTQueue q) >>=
    mapConcurrently_ \(syscall, args) ->
      atomically . writeResponse syscall =<< f args

openWorker :: TCPState -> IO Void
openWorker st = worker st.openReqs \(ip, port) ->
    open ip port
  where
    open :: HostAddress -> PortNumber -> IO (Maybe Nat)
    open ip port = do
        socket <- socket AF_INET Stream defaultProtocol
        try (connect socket $ SockAddrInet port ip) >>= \case
          Left (_ :: IOError) -> close socket $> Nothing
          Right _ -> atomically $
            Just . fromIntegral <$> insertNewSocket socket st.connections

hearWorker :: TCPState -> IO Void
hearWorker st = worker st.hearReqs \_ -> do
    (socket,ip,port) <- accept st.listenSocket <&> \case
      (s,SockAddrInet p i)      -> (s,i,p)
      (s,SockAddrInet6 p i _ _) -> (s,i,p)
    socketId <- atomically $ insertNewSocket socket st.connections
    pure ( NAT $ fromIntegral socketId
         , NAT $ fromIntegral ip
         , NAT $ fromIntegral port )

takeWorker :: TCPState -> IO Void
takeWorker st = worker st.takeReqs \sockId ->
    lookup sockId <$> readTVarIO st.connections >>=
    foldMap \socket -> do -- Monoid ByteString => mempty = BS.empty
      payload <- recv socket 4096
      when (BS.null payload) $
        atomically $ modifyTVar st.connections $ IntMap.delete sockId
      pure payload

giveWorker :: TCPState -> IO Void
giveWorker st = worker st.giveReqs \(sockId, payload) ->
    lookup sockId <$> readTVarIO st.connections >>=
    foldMap \socket -> fmap Alt $ try (send socket payload) >>= \case
      Right bytesSent -> pure $ Just (fromIntegral bytesSent :: Nat)
      Left (_ :: IOError) -> do
        close socket
        atomically $ modifyTVar st.connections $ IntMap.delete sockId
        pure Nothing

instance ToNoun (f a) => ToNoun (Alt f a) where
  toNoun = toNoun . getAlt

shutWorker :: TCPState -> IO Void
shutWorker st = worker st.shutReqs \sockId ->
    whenM (member sockId <$> readTVarIO st.connections) do
      mSocket <- atomically $ stateTVar st.connections $
        updateLookupWithKey (\_ _ -> Nothing) sockId
      whenJust mSocket close
