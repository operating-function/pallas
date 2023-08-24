-- | This module contains code borrowed from the "network-run" package, and
--   modified to support dynamic binding of available TCP ports, and
--   instrumentation to get access to whatever port/host was successfully bound.
module Server.Util.TCP
  ( resolveUntilSuccess
  , runTCPServer'
  ) where

import PlunderPrelude

import Control.Concurrent (forkFinally)
import Network.Socket     (AddrInfo, AddrInfoFlag (..), HostName,
                           ServiceName, Socket, SocketOption (..),
                           SocketType (..), accept, addrAddress, addrFlags,
                           addrSocketType, bind, close, defaultHints,
                           getAddrInfo, gracefulClose, listen, openSocket,
                           setCloseOnExecIfNeeded, setSocketOption,
                           withFdSocket)
import System.Random      (getStdRandom, random, randomR)

import qualified Control.Exception    as E

runTCPServer'
  :: AddrInfo
  -> (Socket -> IO ())
  -> IO ()
runTCPServer' addr server = E.bracket (open addr) close loop
 where
  openServerSocket :: AddrInfo -> IO Socket
  openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock
  open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
      listen sock 1024
      return sock
  loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
          \(conn, _peer) ->
              void $ forkFinally (server conn) (const $ gclose conn)

-- `port` could be generated to conflict with an already-bound port. if that
-- happens, we retry.
resolveUntilSuccess hostPortFun = do
  port <- getStdRandom (randomR (6000,8000)) :: IO Int
  let host = "127.0.0.1"
  res <- resolve Stream Nothing (show port) True
  case res of
    Just ai -> do
      hostPortFun host port
      pure ai
    Nothing -> resolveUntilSuccess hostPortFun
 where
  resolve :: SocketType -> Maybe HostName -> ServiceName -> Bool -> IO (Maybe AddrInfo)
  resolve socketType mhost port passive =
    listToMaybe <$> getAddrInfo (Just hints) mhost (Just port)
   where
    hints =
      defaultHints
        { addrSocketType = socketType
        , addrFlags = if passive then [AI_PASSIVE] else []
        }

gclose sock = gracefulClose sock 5000
