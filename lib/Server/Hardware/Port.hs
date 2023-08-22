-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-----   TODO: Support different HTTP/Telnet ports for different ships.
-----   TODO: ports file exist within the ship data-types, not in `~/.*`.

{-|

    Hardware Device 6: Packet Tranport

    type Seed = Word32 -- Seed used to generate ed25519 key pair
    type Addr = Word32 -- ed25519 public key

    = SIRE []         :: Req (Seed, Addr)
    = RECV [us ls]    :: Seed -> Set Addr -> Req (Time, Addr, Bar)
    = SEND [us th br] :: Seed -> Addr -> Bar -> Req Time

    This currently performs local-to-machine send/recv only.

-}

{-# OPTIONS_GHC -Wall    #-}
{-# OPTIONS_GHC -Werror  #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData       #-}

module Server.Hardware.Port where

import PlunderPrelude

import Data.Acquire
import Fan.Eval
import Fan.Prof                (Flow)
import Server.Hardware.Types

import Crypto.Sign.Ed25519     (PublicKey(..), SecretKey(..),
                                createKeypairFromSeed_)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Fixed              (Fixed(MkFixed))
import Data.Time.Clock         (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (getPOSIXTime)
import GHC.Records             (HasField)
import Fan.Convert             (FromNoun(..), ToNoun(..), fromNoun)
import System.Entropy          (getEntropy)

import qualified Data.ByteString as BS


-- API Types -------------------------------------------------------------------

-- 32 byte public key
newtype Addr = ADDR { bytes :: ByteString }
  deriving newtype (Eq, Ord)

summarize :: ByteString -> String
summarize = show . take 12 . toLazyByteString . byteStringHex

-- 32 byte seed that can be used to generate a keypair (a public key
-- and a secret key).
newtype Seed = SEED { bytes :: ByteString }
  deriving newtype (Eq, Ord)

instance Show Addr where
    show addr = summarize addr.bytes

instance Show Seed where
    show seed = "SEED(" <> show (seedAddr seed) <> ")"

seedAddr :: Seed -> Addr
seedAddr = view _1 . growSeed

data SendRequest = SEND_REQ
    { source  :: Seed
    , target  :: Addr
    , payload :: ByteString
    }

instance Show SendRequest where show x = show (x.source, x.target, x.payload)

data RecvRequest = RECV_REQ
    { identity  :: Seed
    , whitelist :: (Set Addr)
    }

instance Show RecvRequest where show x = show (x.identity, x.whitelist)

newtype Time = TIME Nat
  deriving newtype ToNoun

data SireResponse = SIRE_RESP
    { seed :: Seed
    , addr :: Addr
    }

newtype SendResponse = SEND_RESP
    { when :: Time }
  deriving newtype ToNoun

data RecvResponse = RECV_RESP
    { when :: Time
    , from :: Addr
    , body :: ByteString
    }

data PortRequest
    = SIRE
    | SEND !SendRequest
    | RECV !RecvRequest
  deriving Show


-- API Instances ---------------------------------------------------------------

instance ToNoun SireResponse where
    toNoun r = toNoun (r.seed, r.addr)

instance ToNoun RecvResponse where
    toNoun r = toNoun (r.when, r.from, r.body)

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

decodePortRequest :: Vector Fan -> Maybe PortRequest
decodePortRequest =
    decode . toList
  where
    f :: ∀a. FromNoun a => Fan -> Maybe a
    f = fromNoun

    decode = \case
        ["sire"]       -> pure SIRE
        ["recv",u,w]   -> RECV <$> (RECV_REQ <$> f u <*> f w)
        ["send",u,t,b] -> SEND <$> (SEND_REQ <$> f u <*> f t <*> f b)
        _              -> Nothing


-- State Types -----------------------------------------------------------------

data SendHandle = SEND_HANDLE
    { request    :: SendRequest
    , source     :: Addr
    , isCanceled :: STM Bool
    , ack        :: SendResponse -> STM ()
    , janitor    :: Async ()
    }

data RecvHandle = RECV_HANDLE
    { request    :: RecvRequest
    , isCanceled :: STM Bool
    , recv       :: RecvResponse -> STM ()
    , janitor    :: Async ()
    }

data HWState = HW_STATE
    { queue        :: TBQueue SysCall
    , pendingSends :: TVar (Map Addr [SendHandle])
    , pendingRecvs :: TVar (Map Addr [RecvHandle])
    , portWorker   :: Async Void
    }


-- Clean Canceled Requests from State ------------------------------------------

reapPending
    :: ( MapValue t ~ [h]
       , IsMap t
       , HasField "isCanceled" h (STM Bool)
       )
    => TVar t
    -> ContainerKey t
    -> STM ()
reapPending table key = do
    tab <- readTVar table
    whenJust (lookup key tab) \rs -> do
        newTab <- filterM (fmap not . (.isCanceled)) rs <&> \case
                      []  -> deleteMap key tab
                      res -> insertMap key res tab
        writeTVar table newTab

hack :: IORef Int
hack = unsafePerformIO (newIORef 0)

genHack :: IO Int
genHack = do
    !x <- readIORef hack
    writeIORef hack $! (x+1)
    pure x

jannie :: STM () -> SysCall -> IO (Async ())
jannie reaper syscall = do
    async $ do
        atomically $ do
          isLive <- scsIsLive <$> readTVar syscall.state.var
          if isLive then retry else reaper

--------------------------------------------------------------------------------

-- TODO Caching
seedKeys :: Seed -> (PublicKey, SecretKey)
seedKeys seed =
    case createKeypairFromSeed_ seed.bytes of
        Nothing -> error "seedKeys: impossible"
        Just kp -> kp

addrKey :: Addr -> PublicKey
addrKey addr = PublicKey addr.bytes

growSeed :: Seed -> (Addr, PublicKey, SecretKey)
growSeed seed = (address, pub, sec)
  where
    address             = ADDR pkBytes
    (PublicKey pkBytes) = pub
    (pub, sec)          = seedKeys seed


-- Utilities -------------------------------------------------------------------

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

{-
    This explicitly nulls-out the callback after invoking it.

    As a consuquence, the request will appear to be canceled, which will
    cause it to not match any other requests, and to be cleaned up by
    it's janitor thread.
-}
fillCallback :: ToNoun a => TVar (Maybe (Fan -> STM ())) -> a -> STM ()
fillCallback vCall now = do
    mCb <- readTVar vCall
    whenJust mCb \cb -> do
        cb (toNoun now)
        writeTVar vCall Nothing

mkSendHandle :: HWState -> SysCall -> SendRequest -> IO SendHandle
mkSendHandle st syscall sr = do
    janitor <- jannie (reapPending st.pendingSends sr.target) syscall
    pure SEND_HANDLE
        { request    = sr
        , source     = seedAddr sr.source
        , isCanceled = not . scsIsLive <$> readTVar syscall.state.var
        , ack        = void . writeResponse syscall . toNoun
        , janitor    = janitor
        }

mkRecvHandle :: HWState -> SysCall -> RecvRequest -> Addr -> IO RecvHandle
mkRecvHandle st syscall rr recvAddr = do
    janitor <- jannie (reapPending st.pendingRecvs recvAddr) syscall
    pure RECV_HANDLE
        { request    = rr
        , isCanceled = not . scsIsLive <$> readTVar syscall.state.var
        , recv       = void . writeResponse syscall . toNoun
        , janitor    = janitor
        }

{-
    Given two matching requests, cause both of them to get a response.

    This will also have the effect of nulling out their response
    callbacks, which will:

    1.  Make them appear canceled (so that they wont match any other requests)
    2.  Cause their janitor to be removed them from the state.
-}
marry :: Time -> SendHandle -> RecvHandle -> STM ()
marry now sh rh = do
    sh.ack (SEND_RESP now)
    rh.recv (RECV_RESP now sh.source sh.request.payload)

findMatch :: [SendHandle] -> [RecvHandle] -> STM (Maybe (SendHandle,RecvHandle))
findMatch sends recvs =
    findM (uncurry doesMatch) ((,) <$> sends <*> recvs)

doesMatch :: SendHandle -> RecvHandle -> STM Bool
doesMatch send recv =
    isNothing <$> findM id reasonsWhyNot
  where
    whitelist = recv.request.whitelist
    whitelisted = null whitelist || member send.source whitelist
    reasonsWhyNot =
        [ pure (not whitelisted)
        , recv.isCanceled
        , send.isCanceled
        ]

popMatchingSend
    :: Addr -> Set Addr -> [SendHandle]
    -> STM (Maybe (SendHandle, [SendHandle]))
popMatchingSend recvAddr whitelist = go []
  where
    go !_   []     = pure Nothing
    go !acc (h:hh) = do
        let sentToUs = (h.request.target == recvAddr)
        let weAccept = member recvAddr whitelist
        h.isCanceled >>= \case
            True -> go acc hh
            False ->
                if
                    (sentToUs && weAccept)
                then
                    pure (Just (h, acc ++ hh))
                else
                    go (h:acc) hh

doSend :: HWState -> SysCall -> SendRequest -> IO ()
doSend st syscall sr = do
    now  <- getNow
    hand <- mkSendHandle st syscall sr
    atomically do
        tab <- readTVar st.pendingRecvs
        findMatch [hand] (fromMaybe [] $ lookup sr.target tab) >>= \case
            Just (sh,rh) -> marry now sh rh
            Nothing      -> do
                let inj = Just . (hand:) . fromMaybe []
                modifyTVar' st.pendingSends (alterMap inj sr.target)

doRecv :: HWState -> SysCall -> RecvRequest -> IO ()
doRecv st syscall rr = do
    let recvAddr = (growSeed rr.identity) ^. _1
    now  <- getNow
    hand <- mkRecvHandle st syscall rr recvAddr
    atomically do
        tab <- readTVar st.pendingSends
        findMatch (fromMaybe [] $ lookup recvAddr tab) [hand] >>= \case
            Just (sh,rh) -> marry now sh rh
            Nothing      -> do
                let inj = Just . (hand:) . fromMaybe []
                modifyTVar' st.pendingRecvs (alterMap inj recvAddr)

doSire :: SysCall -> IO ()
doSire syscall = do
    seed <- fmap SEED $ getEntropy 32
    let addr = growSeed seed ^. _1
    let resp = SIRE_RESP seed addr
    atomically (void (writeResponse syscall resp))

runPortWorker :: HWState -> IO ()
runPortWorker st = do
    syscall <- atomically (readTBQueue st.queue)

    case decodePortRequest syscall.args of
        Nothing        -> atomically (void (writeResponse syscall (NAT 0)))
        Just (SEND sr) -> doSend st syscall sr
        Just (RECV rr) -> doRecv st syscall rr
        Just SIRE      -> doSire syscall


-- Device / SysCall interface --------------------------------------------------

runSysCall :: HWState -> SysCall -> STM (Cancel, [Flow])
runSysCall st syscall = do
  writeTBQueue st.queue syscall
  pure (CANCEL (pure ()), [])

categoryCall :: Vector Fan -> Text
categoryCall args = "%port " <> case decodePortRequest args of
  Nothing       -> "UNKNOWN"
  Just SIRE     -> "%sire"
  Just (SEND _) -> "%send"
  Just (RECV _) -> "%recv"

describeCall :: Vector Fan -> Text
describeCall args = categoryCall args <> trailer
  where
    trailer =
      case decodePortRequest args of
        Just (SEND r) -> " " <> tshow r
        Just (RECV r) -> " " <> tshow r
        _             -> ""

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
        pendingSends <- newTVarIO mempty
        pendingRecvs <- newTVarIO mempty
        queue        <- newTBQueueIO 100

        mdo let st = HW_STATE{..}
            portWorker <- async $ forever (runPortWorker st)
            pure st

    shutdown :: HWState -> IO ()
    shutdown st = do
        cancel st.portWorker
