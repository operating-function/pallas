-- | This module exists solely to test & debug the `localhost-router`. It is not
--   "essential" and will not be used in typical localhost networking on Plunder
--   ships (as those implement a similar version of the protocol below).
module Main (main) where

import           PlunderPrelude

import           Codec.Serialise (deserialise, serialise)
import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Monad (forever)
import           Crypto.Sign.Ed25519 (PublicKey, createKeypair, dsign)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Network.Run.TCP (runTCPClient)
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

import           LocalhostRouter

sleep = threadDelay (2 * 10^6 :: Int)

verboseSend s req = do
    sendSer s req
    putStrLn ("\nsending: " <> tshow req)

main :: IO ()
main = runTCPClient "127.0.0.1" "4747" $ \s -> forever $ do
    verboseSend s Connect
    resp <- recvDeserInfallible s
    case resp of
        t@ConnectResp -> putStrLn ("got: " <> tshow t)
        other -> error ("error: got: " <> show other)
    sleep

    (pk, sk) <- createKeypair
    verboseSend s $ IdentifyPropose pk
    resp <- recvDeserInfallible s
    chal <- case resp of
        t@(IdentifyProposeResp chal) -> do
            putStrLn ("got: " <> tshow t)
            pure chal
        other -> error ("error: got: " <> show other)
    sleep

    let sig = dsign sk chal
    hostPort <- sockAddrToHostPort <$> getSocketName s
    verboseSend s $ IdentifyChallenge pk sig hostPort
    resp <- recvDeserInfallible s
    status <- case resp of
        t@(IdentifyChallengeResp status) -> do
            putStrLn ("got: " <> tshow t)
            pure status
        other -> error ("error: got: " <> show other)
    sleep

    verboseSend s $ Resolve pk
    resp <- recvDeserInfallible s
    mb_sa <- case resp of
        t@(ResolveResp mb_sa) -> do
            putStrLn ("got: " <> tshow t)
            pure mb_sa
        other -> error ("error: got: " <> show other)
    sleep
