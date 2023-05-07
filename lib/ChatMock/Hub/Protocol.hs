-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module ChatMock.Hub.Protocol where

import ChatMock.Hub.Packet
import ClassyPrelude
import Crypto.Sign.Ed25519 (PublicKey, SecretKey, Signature)
import Data.LargeWord      (Word256)

--------------------------------------------------------------------------------

data ListenRequest = LR
    { pubkey :: !PublicKey
    , secret :: !SecretKey
    , whites :: !(Set PublicKey)
    , onRecv :: !(PublicKey -> ByteString -> IO ())
    }

data Connection = CONNECTION
    { send   :: IO ()
    , listen :: ListenRequest -> IO ()
    }

data ClientConnection = CLIENT_CONNECTION
    { send   :: IO ()
    , listen :: ListenRequest -> IO ()
    }

data Server = SERVER
    { listen :: IO ClientConnection }

data AuthPacket = AP
    { addr :: !PublicKey  -- 32 byte public key
    , whyt :: !Word256    -- 32 byte bloom Filter (whitelist).
    , last :: !Word256    -- 32 byte merkle backref to previous packet.
    , sign :: !Signature  -- 64 bytes
    , xtra :: !ByteString
    }

data URL = URL

data ConnectError = CONNECT_ERROR
  deriving (Show, Exception)

serve :: Word -> IO Server
serve = serve

connect :: URL -> IO Connection
connect = connect
