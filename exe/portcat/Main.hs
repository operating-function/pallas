-- | `portcat` is a simple utility which implements the "client/ship" side of
--   the localhost-router protocol. It reads from `stdin` and sends those bytes
--   verbatim to the Addr specified via the command line options.
module Main (main) where


import Options.Applicative

import           Codec.Serialise (deserialise, serialise)
import           Conduit
import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Monad (forever)
import           Crypto.Sign.Ed25519 (PublicKey, createKeypair, dsign)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit.Network (sinkSocket)
import           Network.Run.TCP (runTCPClient)
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import           Text.Read (readEither)
import           System.Exit (exitFailure)

import           LocalhostRouter
import           PlunderPrelude
import           Server.Hardware.Port

--------------------------------------------------------------------------------
-- options
--------------------------------------------------------------------------------

data Options
  = Options
    { oSeed :: Seed
    , oAddr :: Addr
    }

-- | toplevel parser
parseOpts :: IO Options
parseOpts = execParser opts'
  where
    opts' = info (helper <*> opts)
      ( fullDesc
     <> progDesc "pc - portcat"
      )

opts :: Parser Options
opts = Options <$> seedArg <*> addrArg

word256Reader :: (String -> Either String ByteString)
word256Reader str = do
  nat <- annotateLeft ("Nat: bad parse: " <>) $ readEither str
  maybe (Left "getWord256 failed") Right (getWord256 nat)

annotateLeft :: (a -> a) -> Either a b -> Either a b
annotateLeft f (Left err) = Left (f err)
annotateLeft f x          = x

-- TODO fix the help on this
seedArg :: Parser Seed
seedArg = argument
  (SEED <$> eitherReader word256Reader)
  (help "SEED, expressed as a nat")

-- TODO fix the help on this
addrArg :: Parser Addr
addrArg = argument
  (ADDR <$> eitherReader word256Reader)
  (help "ADDR, expressed as a nat")

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- parseOpts
  let ourSeed  = oSeed opts
      trgtAddr = oAddr opts
  hp@(host, port) <- runTCPClient "127.0.0.1" "4747" $ \routerSock -> do
    putStrLn "connecting to localhost router"
    sendSer routerSock Connect
    resp <- recvDeserInfallible routerSock
    case resp of
        t@ConnectResp -> putStrLn ("got: " <> tshow t)
        other -> error ("error: got: " <> show other)
    --
    sendSer routerSock $ Resolve (addrKey trgtAddr)
    resp <- recvDeserInfallible routerSock
    mb_hp <- case resp of
        t@(ResolveResp mb_hp) -> do
            putStrLn ("got: " <> tshow t)
            pure mb_hp
        other -> error ("error: got: " <> show other)

    case mb_hp of
      Nothing -> do
        putStrLn "router does not know of addr. exiting."
        exitFailure
      Just hp -> pure hp
  --
  putStrLn ("router identifies ship as: " <> tshow hp)
  runTCPClient host port $ \shipSock -> do
    let (ourAddr, pk, sk) = growSeed ourSeed
        pR = P_RQST ourAddr trgtAddr Nothing ""
    sendSer shipSock (MmpRqst pR)
    mppr <- recvDeserInfallible shipSock
    cP <- case mppr of
      MmpClientPort cP -> pure cP
      other -> error ("error: got: " <> show other)
    case cP.port of
      Nothing -> putStrLn "no port" >> exitFailure
      Just portId -> do
        putStrLn ("got port: " <> tshow portId)
        putStrLn "reading from stdin and streaming wholesale over port"
        runConduit (stdinC .| sinkSocket shipSock)
