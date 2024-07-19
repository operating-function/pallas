-- | This module implements the `localhost-router`. It is expected to be run
--   in order to enable localhost networking between Plunder ships, whose
--   networking hardware will look for the presence of the router on the
--   pre-defined port on localhost.
module Main (main) where

import           PlunderPrelude

import           Codec.Serialise (deserialise, serialise)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (unless, forever, void)
import           Crypto.Sign.Ed25519 (PublicKey, dverify)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime,
                                  getCurrentTime, secondsToNominalDiffTime)
import           Network.Run.TCP (runTCPServer)
import           Network.Socket
import           System.Entropy (getEntropy)

import           LocalhostRouter
import qualified Pq

--------------------------------------------------------------------------------
-- accessories
--------------------------------------------------------------------------------

chalBytes :: Int
chalBytes = 4

chalMapTTL :: NominalDiffTime
chalMapTTL = secondsToNominalDiffTime 10

addrMapTTL :: NominalDiffTime
addrMapTTL = secondsToNominalDiffTime 600

data MapType
  = MtChal
  | MtAddr
  deriving (Show)

mtToNdt :: MapType -> NominalDiffTime
mtToNdt MtChal = chalMapTTL
mtToNdt MtAddr = addrMapTTL

type TTLTup = (UTCTime, (PublicKey, MapType))

cmpTTLTup :: TTLTup -> TTLTup -> Ordering
cmpTTLTup = compare `on` fst

setExpiry
  :: IORef (Pq.Pq TTLTup)
  -> PublicKey
  -> MapType
  -> IO ()
setExpiry ttlPq pk mt = do
  expiryTime <- addUTCTime (mtToNdt mt) <$> getCurrentTime
  let f = Pq.insert cmpTTLTup (expiryTime, (pk, mt))
  modifyIORef' ttlPq f

expireEntries
  :: IORef (Pq.Pq TTLTup)
  -> IORef (Map PublicKey a)
  -> IORef (Map PublicKey b)
  -> UTCTime
  -> IO ()
expireEntries ttlPq chalMap addrMap now = do
  entries <- atomicModifyIORef' ttlPq (go [])
  forM_ entries $ \(pk, mt) -> do
    case mt of
      MtChal -> modifyIORef' chalMap (M.delete pk)
      MtAddr -> modifyIORef' addrMap (M.delete pk)
  where
    go acc pq = case mbMin of
        Nothing -> (pq, acc)
        Just mn -> if fst mn < now
                      then go (snd mn : acc) pq'
                      else (pq, acc)
      where
        (mbMin, pq') = Pq.delMin cmpTTLTup pq

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

-- successful `IdentifyChallenge` of an already-identified `PublicKey` will
-- overwrite the previous Map entry.
--
-- `IdentifyPropose` of a key for which a previous proposition was made will
-- return the same challenge as was previously generated.

main :: IO ()
main = do
    chalMap <- newIORef (M.empty :: Map PublicKey ByteString)
    addrMap <- newIORef (M.empty :: Map PublicKey HostPort)
    ttlPq   <- newIORef (Pq.empty :: Pq.Pq TTLTup)

    putStrLn "starting janitor..."
    _ <- forkIO $ forever $ do
        threadDelay (5 * 10^6 :: Int)
        expireEntries ttlPq chalMap addrMap =<< getCurrentTime

    putStrLn "starting localhost-router..."
    runTCPServer Nothing "4747" $ \s -> forever $ do
        peerSA <- getPeerName s
        req <- recvDeserInfallible s
        putStrLn ("received `" <> tshow req <> "` from " <> tshow peerSA)
        resp <- case req of

            Connect -> pure ConnectResp

            IdentifyPropose pk -> do
                mbChal <- M.lookup pk <$> readIORef chalMap
                chal <- case mbChal of
                    Nothing -> do
                        chal <- getEntropy chalBytes
                        modifyIORef' chalMap (M.insert pk chal)
                        setExpiry ttlPq pk MtChal
                        pure chal
                    Just chal -> pure chal
                pure (IdentifyProposeResp chal)

            IdentifyChallenge pk sig hp -> do
                mbChal <- M.lookup pk <$> readIORef chalMap
                status <- case mbChal of
                    Nothing -> pure CsMissingChal
                    Just chal ->
                      if dverify pk chal sig
                         then do
                           modifyIORef' chalMap (M.delete pk)
                           modifyIORef' addrMap (M.insert pk hp)
                           -- not sure we actually need to expire these?
                           -- setExpiry ttlPq pk MtAddr
                           pure CsValid
                         else pure CsInvalid
                pure (IdentifyChallengeResp status)

            Resolve pk ->
                ResolveResp . M.lookup pk <$> readIORef addrMap

        putStrLn ("sending `" <> tshow resp <> "` to " <> tshow peerSA)
        sendSer s resp
