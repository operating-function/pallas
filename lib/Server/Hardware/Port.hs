-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-|

    Hardware Device 6: Packet Tranport

-}

{-# OPTIONS_GHC -Wall    #-}
{-# OPTIONS_GHC -Werror  #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData       #-}

module Server.Hardware.Port where

import PlunderPrelude

import Data.Acquire
import Fan.Eval
import Fan.Prof (Flow)
import LocalhostRouter
import Server.Hardware.Types
import Server.Util.TCP           (resolveUntilSuccess, runTCPServer')

import Codec.Serialise           (Serialise)
import Control.Concurrent        (threadDelay)
import Crypto.Sign.Ed25519       (PublicKey(..), SecretKey(..),
                                  createKeypairFromSeed_, dsign)
import Data.ByteString.Builder   (byteStringHex, toLazyByteString)
import Data.Fixed                (Fixed(MkFixed))
import Data.Time.Clock           (NominalDiffTime,
                                  nominalDiffTimeToSeconds,
                                  secondsToNominalDiffTime)
import Data.Time.Clock.POSIX     (getPOSIXTime)
import GHC.Records               (HasField)
import Fan.Convert               (FromNoun(..), ToNoun(..), fromNoun)
import Network.Run.TCP           (runTCPClient)
import Network.Socket            (Socket, close)
import Network.Socket.ByteString (recv, sendAll)
import System.Entropy            (getEntropy)
import System.Random             (getStdRandom, random)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import qualified Deque.Lazy            as Dq
import qualified Data.Map              as M
import qualified GHC.Exts              as Exts


-- API Types -------------------------------------------------------------------

-- 32 byte public key
newtype Addr = ADDR { bytes :: ByteString }
  deriving newtype (Eq, Ord)
  deriving (Generic)

instance Serialise Addr

instance Show Addr where
    show addr = summarize addr.bytes

-- 32 byte seed that can be used to generate a keypair (a public key
-- and a secret key).
newtype Seed = SEED { bytes :: ByteString }
  deriving newtype (Eq, Ord)

instance Show Seed where
    show seed = "SEED(" <> show (seedToAddr seed) <> ")"

summarize :: ByteString -> String
summarize = BC.unpack . BS.toStrict . take 12 . toLazyByteString . byteStringHex

seedToAddr :: Seed -> Addr
seedToAddr = view _1 . growSeed

type PortId = Word64
type Timeout = Word32

data StaticResponse
  = SR_DROP
  | SR_HOLD Timeout
  | SR_RESP Timeout ByteString
  | SR_DYNO Fan ByteString
  deriving (Show)

data ClientPort = C_PORT
  { them    :: Addr
  , payload :: ByteString -- Pin Bar
  , port    :: Maybe PortId
  }
  deriving (Generic, Show)

instance Serialise ClientPort

instance ToNoun ClientPort where
  toNoun r = mkRow [toNoun r.them, mkPin (toNoun r.payload), toNoun r.port]

data ServerPort = S_PORT
    { from    :: Addr
    , payload :: Fan
    , port    :: PortId
    }

data PortRqst = P_RQST
    { src  :: Addr
    , dst  :: Addr
    , time :: Maybe Timeout
    , body :: ByteString
    }
    deriving (Generic, Show)

instance Serialise PortRqst

newtype Time = TIME Nat
  deriving newtype (ToNoun, Show)

data SeedAddr = SEED_ADDR
  { seed :: Seed
  , addr :: Addr
  }

data RecvResp = RECV_RESP
  { when :: Time       -- Nat
  , body :: ByteString -- Pin Bar
  }
  deriving (Show)

-- TODO make cancellable:
-- [x] SERV
-- [x] RQST
-- [ ] DYNO
-- [ ] HOLD
-- TODO should SEND/RECV/RCLL be cancellable?
data PortRequest
  = SIRE                     -- -> IO (Seed, Addr)
  | SERV !Seed !Fan          -- -> IO Void
  --       Fan ^ is of type (PortRqst -> StaticResponse)
  | RQST !Seed !PortRqst     -- -> IO ClientPort
  | DYNO !Seed               -- -> IO ServerPort
  | HOLD !PortId             -- -> IO ()
  | SEND !PortId !ByteString -- -> IO Time
  | RECV !PortId             -- -> IO RecvResp
  | RCLL !PortId !Int        -- -> IO RecvResp
  deriving (Show)


-- API Instances ---------------------------------------------------------------

instance ToNoun SeedAddr where
    toNoun r = toNoun (r.seed, r.addr)

instance ToNoun RecvResp where
    toNoun r = mkRow [toNoun r.when, mkPin (toNoun r.body)]

instance ToNoun Addr where
    toNoun x = NAT (bytesNat x.bytes)

instance ToNoun Seed where
    toNoun x = NAT (bytesNat x.bytes)

getNat :: Fan -> Maybe Nat
getNat (NAT n) = Just n
getNat _       = Nothing

-- TODO This can be much faster!
getWord256 :: Nat -> Maybe ByteString
getWord256 x =
    let b = natBytes x in
    if (length b == 32) then
        Just b
    else if (length b > 32) then
        Nothing
    else
        Just (b <> BS.replicate (32 - length b) 0)

instance FromNoun Addr where
    fromNoun = getNat >=> fmap ADDR . getWord256

instance FromNoun Seed where
    fromNoun = getNat >=> fmap SEED . getWord256

instance ToNoun PortRqst where
    toNoun pS = toNoun (pS.src, pS.dst, pS.time, pS.body)
instance FromNoun PortRqst where
    fromNoun =
      fromNoun >=> \(src, dst, time, body) -> pure (P_RQST src dst time body)

instance ToNoun StaticResponse where
    toNoun SR_DROP = NAT 0
    toNoun (SR_HOLD tm) = toNoun (0::Word8, tm)
    toNoun (SR_RESP tm payload) = toNoun (1::Word8, tm, payload)
    toNoun (SR_DYNO x y) = toNoun (2::Word8, x, y)
instance FromNoun StaticResponse where
    fromNoun (NAT 0) = Just SR_DROP
    fromNoun noun
      | Just (0::Word8, tm) <- fromNoun noun
      = Just (SR_HOLD tm)
      --
      | Just (1::Word8, tm, payload) <- fromNoun noun
      = Just (SR_RESP tm payload)
      --
      | Just (2::Word8, x, y) <- fromNoun noun
      = Just (SR_DYNO x y)
      --
      | otherwise
      = Nothing

instance ToNoun ServerPort where
  toNoun sP = toNoun (sP.from, sP.payload, sP.port)

decodePortRequest :: Vector Fan -> Maybe PortRequest
decodePortRequest = decode . toList
 where
  f :: ∀a. FromNoun a => Fan -> Maybe a
  f = fromNoun

  decode = \case
    ["sire"]       -> pure SIRE
    ["serv", s, g] -> SERV <$> f s <*> pure g
    ["rqst", s, p] -> RQST <$> f s <*> f p
    ["dyno", s]    -> DYNO <$> f s
    ["hold", i]    -> HOLD <$> f i
    ["send", i, m] -> SEND <$> f i <*> f m
    ["recv", i]    -> RECV <$> f i
    ["rcll", i, c] -> RCLL <$> f i <*> f c
    _              -> Nothing


-- State Types -----------------------------------------------------------------

data Hndl val resp = HNDL
    { val        :: val
    , isCanceled :: STM Bool
    , receive    :: resp -> STM ()
    , janitor    :: Async ()
    }

data RecvType
  = RtRecv
  | RtRcll Int (Dq.Deque ByteString)

data RecvHndl
  = RecvHndl
  { ty      :: RecvType
  , receive :: RecvResp -> STM ()
  }

data PortRecvMode
  = PrmRHs (Dq.Deque RecvHndl)
  | PrmBS ByteString

prmNonNullHs :: PortRecvMode -> Bool
prmNonNullHs (PrmRHs rHs) = not (Dq.null rHs)
prmNonNullHs _ = False

handleReceivedBS
  :: Time
  -> Dq.Deque RecvHndl
  -> ByteString
  -> STM PortRecvMode
handleReceivedBS now rHs portBS = case Dq.uncons rHs of
  Nothing -> error "impossible: received portBS w/ empty rHs"
  Just (rH, rHs') ->
    case rH.ty of
      RtRecv -> do
        rH.receive (RECV_RESP now portBS)
        pure (PrmRHs rHs')
      RtRcll neededBytes chunks ->
        case compare neededBytes (BS.length portBS) of
          EQ -> do
            let recvBS = mconcat (Exts.toList (Dq.snoc portBS chunks))
            rH.receive (RECV_RESP now recvBS)
            pure (PrmRHs rHs')
          LT -> do
            let (neededBS, unneededBS) = BS.splitAt neededBytes portBS
                recvBS = mconcat (Exts.toList (Dq.snoc neededBS chunks))
            rH.receive (RECV_RESP now recvBS)
            pure (PrmBS unneededBS)
          GT -> do
            let neededBytes' = neededBytes - (BS.length portBS)
                chunks' = Dq.snoc portBS chunks
                ty' = RtRcll neededBytes' chunks'
                rH' = rH { ty = ty' }
            pure (PrmRHs (Dq.cons rH' rHs'))


-- | for `sendHndls`, `recvHndls`, `remoteSends`, oldest Hndls are at the start
-- of the Deque. new Hndls should be snoc-ed to the end.
data PortState = PORT_STATE
    { portQueue   :: TBQueue (Hndl ByteString Time)
    -- ^ queue to write "sends" to, which the worker process will forward to
    --   the remote machine.
    , sendHndls   :: TVar (Dq.Deque (Hndl ByteString Time))
    , portMode    :: TVar PortRecvMode
    }

mkPortState :: TBQueue (Hndl ByteString Time) -> STM PortState
mkPortState psQ =
  PORT_STATE psQ <$> newTVar mempty <*> newTVar (PrmRHs mempty)

data HWState = HW_STATE
    { syscallQueue :: TBQueue (SysCall, TVar Cancellable)
    , runningServs :: TVar (Map Addr (Hndl Fan ()))
    -- ^ Addr is target to which RQSTs will be made.
    , pendingRqsts :: TVar (Map Addr (Dq.Deque (Hndl PortRqst ClientPort)))
    -- ^ Addr is target of RQST.
    -- TODO ^ this is not actually used anywhere, we just `writeResponse` on
    --        the syscall.
    , livePorts    :: TVar (Map PortId (TVar PortState))
    , dynoResps    :: TVar (Map Addr (Dq.Deque (Fan, PortId)))
    -- ^ Addr is the same as the Addr of the SERV which outputs SR_DYNO in
    -- order to match with this.
    , pendingDynos :: TVar (Map Addr (Dq.Deque (Hndl () ServerPort)))
    -- ^ Addr is target of RQST, whose static handler must return a SR_DYNO
    -- to match one of these, but has not yet.
    , routerQueue  :: MVar (TBQueue (RouterRequest, MVar RouterResponse))
    , servHostPort :: MVar HostPort
    , routerWorker :: Async ()
    , portWorker   :: Async Void
    , servWorker   :: Async ()
    , hndlrAsyncs  :: TVar [Async ()] -- is this a good pattern?
    , asyncCleaner :: Async Void
    }


-- Clean Canceled Requests from State ------------------------------------------

reapPendingDeque
    :: ( MapValue t ~ (Dq.Deque h)
       , IsMap t
       , HasField "isCanceled" h (STM Bool)
       )
    => TVar t
    -> ContainerKey t
    -> STM ()
reapPendingDeque table key = do
    tab <- readTVar table
    whenJust (lookup key tab) \rs -> do
        newTab <- filterCancelled (Exts.toList rs) <&> \case
                      []  -> deleteMap key tab
                      res -> insertMap key (Exts.fromList res) tab
        writeTVar table newTab

reapDequeTv
  :: HasField "isCanceled" h (STM Bool)
  => TVar (Dq.Deque h)
  -> STM ()
reapDequeTv tv =
  writeTVar tv . Exts.fromList =<< filterCancelled . Exts.toList =<< readTVar tv

filterCancelled
  :: HasField "isCanceled" h (STM Bool)
  => [h]
  -> STM [h]
filterCancelled = filterM (fmap not . (.isCanceled))

jannie :: STM () -> SysCall -> IO (Async ())
jannie reaper syscall = do
    async $ do
        atomically $ do
          isLive <- scsIsLive <$> readTVar syscall.state.var
          if isLive then retry else reaper

mkHndl
  :: ToNoun resp
  => SysCall
  -> STM ()
  -> val
  -> IO (Hndl val resp)
mkHndl syscall reaper val = do
  janitor <- jannie reaper syscall
  pure HNDL
    { val        = val
    , isCanceled = not . scsIsLive <$> readTVar syscall.state.var
    , receive    = void . writeResponse syscall . toNoun
    , janitor    = janitor
    }

--------------------------------------------------------------------------------

-- TODO Caching
seedKeys :: Seed -> (PublicKey, SecretKey)
seedKeys seed =
    case createKeypairFromSeed_ seed.bytes of
        Nothing -> error "seedKeys: impossible"
        Just kp -> kp

addrKey :: Addr -> PublicKey
addrKey addr = PublicKey addr.bytes

keyAddr :: PublicKey -> Addr
keyAddr = ADDR . unPublicKey

growSeed :: Seed -> (Addr, PublicKey, SecretKey)
growSeed seed = (address, pub, sec)
  where
    address             = ADDR pkBytes
    (PublicKey pkBytes) = pub
    (pub, sec)          = seedKeys seed


-- Utilities -------------------------------------------------------------------

-- | Executes the monadic action (`f`) on each element, Just-wrapping the first
--   (if any) element on which `f` returns True.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = pure Nothing
findM f (x:xs) = f x >>= \case { True -> pure (Just x); False -> findM f xs }


-- Executing Requests ----------------------------------------------------------

-- TODO Change this over to IAT time.
getNow :: IO Time
getNow = do
    x <- getPOSIXTime
    let MkFixed picos = nominalDiffTimeToSeconds x
    pure $ TIME (fromIntegral picos `div` 1000)

doSire :: SysCall -> IO ()
doSire syscall = do
    seed <- fmap SEED $ getEntropy 32
    let addr = growSeed seed ^. _1
    let resp = SEED_ADDR seed addr
    atomically (void (writeResponse syscall resp))

runPortWorker :: HWState -> IO Void
runPortWorker st = forever $ do
    (syscall, cnclTv) <- atomically (readTBQueue st.syscallQueue)

    case decodePortRequest syscall.args of
        Nothing             -> atomically (void (writeResponse syscall (NAT 0)))
        Just SIRE           -> doSire syscall
        Just (SERV seed fn) -> -- `doServ` could block on router Identify
          registerHandlerAsync st (doServ st syscall cnclTv seed fn)
        Just (RQST seed pR) -> -- if a port is created, `doRqst` may not return
          registerHandlerAsync st (doRqst st syscall cnclTv seed pR)
        Just (DYNO seed)    -> -- if a port is created, `doDyno` may not return
          registerHandlerAsync st (doDyno st syscall seed)
        Just (HOLD _)       -> error "unimpl"
        Just (SEND pI pM)   -> doSend st syscall pI pM
        Just (RECV pId)     -> doRecv st syscall pId Nothing
        Just (RCLL pId c)   -> doRecv st syscall pId (Just c)

doServ :: HWState -> SysCall -> TVar Cancellable -> Seed -> Fan -> IO ()
doServ st syscall cnclTv seed fn = do
  handleIdentify st seed
  hndl <- mkHndl syscall pass fn
  atomically $ do
    cncl <- readTVar cnclTv
    case cncl of
      Cancelled -> pass
      Live _ -> error "impossible: doServ is already live"
      Pending -> do
        let addr = seedToAddr seed
        mbOldHndl <- M.lookup addr <$> readTVar st.runningServs
        -- notify old serv that it has been replaced
        maybe pass (\h -> h.receive ()) mbOldHndl
        modifyTVar' st.runningServs (M.insert addr hndl)
        writeTVar cnclTv (Live (CANCEL (hndl.receive ())))

doRqst :: HWState -> SysCall -> TVar Cancellable -> Seed -> PortRqst -> IO ()
doRqst st syscall cnclTv _seed pR = do
  hndl <- mkHndl syscall (reapPendingDeque st.pendingRqsts pR.dst) pR
  shouldContinue <- atomically do
    cncl <- readTVar cnclTv
    case cncl of
      Cancelled -> pure False
      Live _ -> traceM "doRqst: impossible: already done?" >> pure False
      Pending -> do
        let inj = Just . (Dq.snoc hndl) . fromMaybe mempty
        modifyTVar' st.pendingRqsts (alterMap inj pR.dst)
        writeTVar cnclTv (Live (CANCEL (void (writeResponse syscall (NAT 0)))))
        pure True
  when shouldContinue $
    handleRqstToRemote st syscall pR

getServOrExpireCancelled :: HWState -> PortRqst -> IO (Maybe (Hndl Fan ()))
getServOrExpireCancelled st pR = atomically $ do
  let addr = pR.dst
  mbServHndl <- M.lookup addr <$> readTVar st.runningServs
  case mbServHndl of
    Nothing -> pure Nothing
    Just hndl -> do
      tst <- hndl.isCanceled
      if tst
         then do
           modifyTVar' st.runningServs (M.delete addr)
           pure Nothing
         else pure (Just hndl)

doDyno :: HWState -> SysCall -> Seed -> IO ()
doDyno st syscall seed = do
  let addr = seedToAddr seed
  shouldRegDyno <- atomically $ do
    mbDRs <- M.lookup addr <$> readTVar st.dynoResps
    case Dq.uncons =<< mbDRs of
      Just ((dR, pId), rest) -> do
        void . writeResponse syscall $ toNoun (S_PORT addr dR pId)
        modifyTVar' st.dynoResps (M.insert addr rest)
        pure False
      -- no entry, or empty list
      _ -> pure True
  when shouldRegDyno $ do
    hndl <- mkHndl syscall (reapPendingDeque st.pendingDynos addr) ()
    let inj = Just . (Dq.snoc hndl) . fromMaybe mempty
    atomically (modifyTVar' st.pendingDynos (alterMap inj addr))

doSend :: HWState -> SysCall -> PortId -> ByteString -> IO ()
doSend st syscall portId portBS = do
  -- INFO we have to do some non-atomic STM reads b/c mkHndl & janitor code
  -- (below) need to be in IO to spawn an async. there is thus possible risk of
  -- race conditions with other syscalls.
  mbPortStateTv <- M.lookup portId <$> readTVarIO st.livePorts
  case mbPortStateTv of
    -- invalid send / nonexistent port
    Nothing -> pass

    -- valid port
    Just portStateTv -> do
      portState <- readTVarIO portStateTv
      let reap = reapDequeTv portState.sendHndls
      sendHndl <- mkHndl syscall reap portBS :: IO (Hndl ByteString Time)
      atomically $ do
        writeTBQueue portState.portQueue sendHndl
        modifyTVar' portState.sendHndls (Dq.snoc sendHndl)

doRecv :: HWState -> SysCall -> PortId -> Maybe Int -> IO ()
doRecv st syscall pId mbRecvAllCount = do
  now <- getNow
  atomically do
    mbPortStateTv <- M.lookup pId <$> readTVar st.livePorts
    case mbPortStateTv of
      -- invalid recv / nonexistent port
      Nothing -> pass

      -- valid port
      Just portStateTv -> do
        portState <- readTVar portStateTv
        let rTy = case mbRecvAllCount of
                    Nothing -> RtRecv
                    Just c  -> RtRcll c mempty
        let receive = void . writeResponse syscall . toNoun
        let recvHndl = RecvHndl rTy receive
        mode <- readTVar portState.portMode
        mode' <- case mode of
                   PrmRHs rHs -> pure (PrmRHs (Dq.snoc recvHndl rHs))
                   PrmBS bs -> handleReceivedBS now (dequeSing recvHndl) bs
        writeTVar portState.portMode mode'

-- External-to-machine networking ----------------------------------------------

data MmPrePortReq
  = MmpRqst PortRqst
  deriving (Generic, Show)

data MmPrePortResp
  = MmpClientPort ClientPort
  | MmpHold Timeout
  deriving (Generic, Show)

instance Serialise MmPrePortReq
instance Serialise MmPrePortResp

resolveCacheTtl :: NominalDiffTime
resolveCacheTtl = secondsToNominalDiffTime 2

identifyCacheTtl :: NominalDiffTime
identifyCacheTtl = secondsToNominalDiffTime 2

rqstToRemoteTimout :: Int
rqstToRemoteTimout = 10 * 10^(6::Int)

resolveTimout :: Int
resolveTimout = 5 * 10 ^ (5::Int)

registerHandlerAsync :: HWState -> IO () -> IO ()
registerHandlerAsync st x = do
  a <- async x
  atomically $ do
    hs <- readTVar st.hndlrAsyncs
    writeTVar st.hndlrAsyncs (a:hs)

runServWorker
  :: HWState
  -> IO ()
runServWorker st = do
  -- block until we have router connection established (perhaps indefinitely)
  _ <- readMVar st.routerQueue
  let hostPortFun host port = putMVar st.servHostPort (host, show port)
  addrInfo <- resolveUntilSuccess hostPortFun
  runTCPServer' addrInfo $ \sock -> do
    req <- recvDeser sock
    case req of
      Right (MmpRqst pR) -> handleRqstFromRemote st sock pR
      _ -> putStrLn ("bad req: " <> tshow req)

handleRqstFromRemote
  :: HWState
  -> Socket
  -> PortRqst
  -> IO ()
handleRqstFromRemote st sock pR = do
  mbServHndl <- getServOrExpireCancelled st pR
  case mbServHndl of
    Nothing -> pure ()
    Just shh -> do
      let staticResp = fromNoun (shh.val %% (toNoun pR))
      case staticResp of
        Nothing -> putStrLn "bad static handler"
        Just SR_DROP -> close sock
        Just (SR_HOLD tm) ->
          sendSer sock (MmpHold tm)
        Just (SR_RESP _timeout payload) -> do
          -- TODO what to do about timeout?
          sendSer sock (MmpClientPort (C_PORT pR.dst payload Nothing))
        Just (SR_DYNO dR dBar) -> do
          takenPorts <- M.keysSet <$> readTVarIO st.livePorts
          pId <- genPortId takenPorts
          --
          dispatchDynoOrRegisterVal st pR.dst dR pR pId
          --
          (psQ, portStateTv) <- atomically $ do
            psQ <- newTBQueue 100
            portStateTv <- newTVar =<< mkPortState psQ
            modifyTVar' st.livePorts (M.insert pId portStateTv)
            pure (psQ, portStateTv)
          --
          -- this indicates to the client that we have created a port to do
          -- ongoing comms over.
          sendSer sock (MmpClientPort (C_PORT pR.dst dBar (Just pId)))
          --
          handlePortSendRecv sock portStateTv psQ

portRecvChunkSize :: Int
portRecvChunkSize = 4096

handlePortSendRecv
  :: Socket
  -> TVar PortState
  -> TBQueue (Hndl ByteString Time)
  -> IO ()
handlePortSendRecv sock portStateTv psQ = forever $ do
  tbqReadAsync <- async $ do
    Left <$> atomically (readTBQueue psQ)
  sockRecvAsync <- async $ do
    _ <- atomically $ do
      shouldRecv <- prmNonNullHs
        <$> (readTVar =<< ((.portMode) <$> readTVar portStateTv))
      if shouldRecv
         then pure ()
         else retry
    bs <- recv sock portRecvChunkSize
    if BS.null bs
       -- TODO peer has closed connection! handle properly! maybe notify any
       -- HOLDs that the port is dead. here we just block forever, which works,
       -- but isn't complete.
       then let x = x in x
       else pure (Right bs)
  (_, res) <- waitAnyCancel [sockRecvAsync, tbqReadAsync]
  case res of
    Left sendHndl -> do
      sendAll sock sendHndl.val
      now <- getNow
      atomically (sendHndl.receive now)
    Right portBS -> do
      now <- getNow
      atomically $ do
        portState <- readTVar portStateTv
        mode <- readTVar portState.portMode
        mode' <- case mode of
                   PrmRHs rHs -> handleReceivedBS now rHs portBS
                   PrmBS _ -> error "impossible: received portBS w/ empty rHs"
        writeTVar portState.portMode mode'

-- if a matching-addr-ed DYNO exists in `pendingDynos`, we `receive` on it. if
-- no match, we register the value in `dynoResps`, such that a later DYNO
-- syscall will get it.
dispatchDynoOrRegisterVal
  :: HWState
  -> Addr
  -> Fan
  -> PortRqst
  -> PortId
  -> IO ()
dispatchDynoOrRegisterVal st addr dR pR pId = do
  shouldRegDynoResp <- atomically $ do
    mbPDs <- M.lookup addr <$> readTVar st.pendingDynos
    case mbPDs of
      Nothing -> pure True
      Just hs -> do
        mbHndl <- findM (\h -> not <$> h.isCanceled) (Exts.toList hs)
        case mbHndl of
          Nothing -> pure True
          Just hndl -> do
            hndl.receive (S_PORT addr dR pId)
            pure False
  when shouldRegDynoResp $ do
    let inj = Just . (Dq.snoc (dR, pId)) . fromMaybe mempty
    atomically (modifyTVar' st.dynoResps (alterMap inj pR.dst))

genPortId :: Set PortId -> IO PortId
genPortId taken = do
  pId <- getStdRandom random :: IO PortId
  if pId `elem` taken
     then genPortId taken
     else pure pId

handleRqstToRemote :: HWState -> SysCall -> PortRqst -> IO ()
handleRqstToRemote st syscall pR = do
  (host, port) <- handleResolve st (addrKey pR.dst)
  runTCPClient host port $ \sock -> do
    sendAsync <- async $ do
      sendSer sock (MmpRqst pR)
      res <- recvDeser sock
      pure (Right res)
    timeoutAsync <- async $ Left <$> threadDelay rqstToRemoteTimout
    (_, res) <- waitAnyCancel [sendAsync, timeoutAsync]
    case res of
      Left () -> handleRqstToRemote st syscall pR
      Right (Right (MmpClientPort cP)) -> do
        case cP.port of
          Nothing -> do
            -- no created port, simple one-and-done RQST
            atomically (void . writeResponse syscall $ toNoun cP)
          Just pId -> do
            -- setup PORT_STATE
            (portStateTv, psQ) <- atomically $ do
              psQ <- newTBQueue 100
              portState <- mkPortState psQ
              portStateTv <- newTVar portState
              modifyTVar' st.livePorts (M.insert pId portStateTv)
              -- return RQST syscall
              void . writeResponse syscall $ toNoun cP
              pure (portStateTv, psQ)
            -- run send/recv handler
            handlePortSendRecv sock portStateTv psQ
      Right (Right (MmpHold _tm)) ->
        -- TODO do proper timeout logic here. we want to retry until `tm` seconds
        -- has elapsed, then stop. probably `writeResponse syscall (NAT 0)` to
        -- indicate failure?
        handleRqstToRemote st syscall pR
      Right resp -> error ("bad resp: " <> show resp)

-- TODO this will just retry forever, which is bad.
handleResolve
  :: HWState
  -> PublicKey
  -> IO HostPort
handleResolve st trgtPk = do
  -- block until we have router connection established (perhaps indefinitely)
  rQ <- readMVar st.routerQueue
  ret <- newEmptyMVar
  atomically (writeTBQueue rQ (Resolve trgtPk, ret))
  resp <- takeMVar ret
  case resp of
    ResolveResp (Just hostPort) -> pure hostPort
    ResolveResp Nothing -> do
      threadDelay resolveTimout
      handleResolve st trgtPk
    _ -> error ("bad resolve resp: " <> show resp)

handleIdentify
  :: HWState
  -> Seed
  -> IO ()
handleIdentify st seed = do
  -- block until we have router connection established (perhaps indefinitely)
  rQ <- readMVar st.routerQueue
  ret <- newEmptyMVar
  let (pk, sk) = seedKeys seed
  atomically (writeTBQueue rQ (IdentifyPropose pk, ret))
  resp <- takeMVar ret
  case resp of
    IdentifyProposeResp chal -> do
      hostPort <- readMVar st.servHostPort
      let sig = dsign sk chal
      ret1 <- newEmptyMVar
      atomically (writeTBQueue rQ (IdentifyChallenge pk sig hostPort, ret1))
      resp1 <- takeMVar ret1
      finalizer resp1
    _ -> finalizer resp
 where
  finalizer resp = case resp of
    IdentifyChallengeResp CsValid -> pure ()
    IdentifyChallengeResp CsInvalid -> do
      putStrLn "handleIdentify: invalid. retrying"
      threadDelay (5 * 10^(5 :: Int))
      handleIdentify st seed
    IdentifyChallengeResp CsMissingChal -> do
      putStrLn "handleIdentify: missing chal. retrying"
      threadDelay (5 * 10^(5 :: Int))
      handleIdentify st seed
    _ -> putStrLn ("handleIdentify: bad resp: " <> tshow resp)

runAsyncCleaner :: TVar [Async ()] -> IO Void
runAsyncCleaner hndlrAsyncs = forever $ do
  threadDelay (5 * 10^(5 :: Int))
  _pair <- atomically $ do
    hs <- readTVar hndlrAsyncs
    filteredHs <- flip filterM hs $ \h -> isNothing <$> pollSTM h
    writeTVar hndlrAsyncs filteredHs
    pure (length hs, length filteredHs)
  -- putStrLn ("runAsyncCleaner: " <> tshow pair)
  pure ()

runRouterWorker
  :: MVar (TBQueue (RouterRequest, MVar RouterResponse))
  -> IO ()
runRouterWorker routerQueue = do
  runTCPClient "127.0.0.1" "4747" $ \sock -> do
    sendSer sock Connect
    resp <- recvDeser sock
    case resp of
      Right ConnectResp -> do
        rQ <- newTBQueueIO 100
        putMVar routerQueue rQ
        resolves <- newTVarIO mempty
        identifies <- newTVarIO mempty
        go rQ resolves identifies sock
      _ -> pure ()
 where
  go
    :: TBQueue (RouterRequest, MVar RouterResponse)
    -> TVar (Map PublicKey (HostPort, UTCTime))
    -> TVar (Map PublicKey UTCTime)
    -> Socket
    -> IO ()
  go rQ resolves identifies sock = forever $ do
    (rReq, rRespMv) <- atomically (readTBQueue rQ)
    now <- getCurrentTime
    shouldSend <- case rReq of
      -- if an unexpired resolution is in the map, wrap the HostPort and return
      -- that as though we had Resolved w/ the router. if lookup fails, send the
      -- request.
      Resolve pk -> do
        mbRslv <- M.lookup pk <$> readTVarIO resolves
        case mbRslv of
          Just (hostPort, expiry) | (now <= expiry) -> do
            putMVar rRespMv (ResolveResp (Just hostPort))
            pure False
          _ -> pure True
      -- if an unexpired identification entry is in the map, respond as though
      -- the challenge process was followed and was successful. this approach
      -- requires the "client" to respond appropriately to IdentifyProposeResp
      -- versus IdentifyChallengeResp.
      IdentifyPropose pk -> do
        mbExpiry <- M.lookup pk <$> readTVarIO identifies
        case mbExpiry of
          Just expiry | (now <= expiry) -> do
            putMVar rRespMv (IdentifyChallengeResp CsValid)
            pure False
          _ -> pure True
      _ -> pure True
    when shouldSend $ do
      sendSer sock rReq
      resp <- recvDeser sock
      case resp of
        Right rResp -> putMVar rRespMv rResp
        _ -> putStrLn ("runRouterWorker: bad router response: " <> tshow resp)

-- Device / SysCall interface --------------------------------------------------

data Cancellable = Pending | Cancelled | Live Cancel

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall = do
  cnclTv <- newTVar Pending
  writeTBQueue st.syscallQueue (syscall, cnclTv)
  pure (CANCEL (handleCancel cnclTv), [])
 where
  handleCancel cnclTv = do
    cncl <- readTVar cnclTv
    case cncl of
      Pending -> writeTVar cnclTv Cancelled
      Cancelled -> pass
      Live x -> x.action

categoryCall :: Vector Fan -> Text
categoryCall args = "%port " <> case decodePortRequest args of
  Nothing         -> "UNKNOWN"
  Just SIRE       -> "%sire"
  Just (SERV _ _) -> "%serv"
  Just (RQST _ _) -> "%rqst"
  Just (DYNO _)   -> "%dyno"
  Just (HOLD _)   -> "%hold"
  Just (SEND _ _) -> "%send"
  Just (RECV _)   -> "%recv"
  Just (RCLL _ _) -> "%rcll"

describeCall :: Vector Fan -> Text
describeCall args = categoryCall args <> trailer
  where
    trailer =
      case decodePortRequest args of
        Just (SERV s _)  -> " " <> tshow s
        Just (RQST s pr) -> " " <> tshow s <> " " <> tshow pr
        Just (DYNO s)    -> " " <> tshow s
        Just (HOLD i)    -> " " <> tshow i
        Just (SEND i m)  -> " " <> tshow i <> " " <> tshow m
        Just (RECV i)    -> " " <> tshow i
        Just (RCLL i c)  -> " " <> tshow i <> " " <> tshow c
        _                -> ""

createHardwarePort :: Acquire Device
createHardwarePort = do
    st <- mkAcquire startup shutdown
    pure DEVICE
        { spin = \_ -> pass -- Don't care which cog makes the calls.
        , stop = \_ -> pass
        , call = runSysCall st
        , category = categoryCall
        , describe = describeCall
        }
  where
    startup :: IO HWState
    startup = do
        syscallQueue <- newTBQueueIO 100
        runningServs <- newTVarIO mempty
        pendingRqsts <- newTVarIO mempty
        livePorts    <- newTVarIO mempty
        dynoResps    <- newTVarIO mempty
        pendingDynos <- newTVarIO mempty
        routerQueue  <- newEmptyMVar
        servHostPort <- newEmptyMVar
        hndlrAsyncs  <- newTVarIO []
        asyncCleaner <- async $ runAsyncCleaner hndlrAsyncs

        mdo let st = HW_STATE{..}
            routerWorker <- async $ runRouterWorker st.routerQueue
            portWorker <- async $ runPortWorker st
            servWorker <- async $ runServWorker st
            pure st

    shutdown :: HWState -> IO ()
    shutdown st = do
        cancel st.portWorker
        cancel st.servWorker
        cancel st.asyncCleaner
        mapM_ cancel =<< readTVarIO st.hndlrAsyncs

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

dequeSing :: a -> Dq.Deque a
dequeSing x = Dq.cons x mempty
