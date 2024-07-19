-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall         #-}
{-# OPTIONS_GHC -Werror       #-}

module ChatMock.Hub.Packet
    ( AuthPacket(..)
    , SendPacket(..)
    , BackCode(..)
    , BackPacket(..)
    , Packet(..)
    )
where

import ClassyPrelude

import Crypto.Sign.Ed25519 (PublicKey, Signature)
import Data.LargeWord      (Word256)

--------------------------------------------------------------------------------

data AuthPacket = AP
    { addr :: !PublicKey  -- 32 byte public key
    , whyt :: !Word256    -- 32 byte bloom Filter (whitelist).
    , last :: !Word256    -- 32 byte merkle backref to previous packet.
    , sign :: !Signature  -- 64 bytes
    , xtra :: !ByteString
    }

data SendPacket = SP
    { from :: !PublicKey  -- 32 bytes
    , dest :: !PublicKey  -- 32 bytes
    , body :: !ByteString -- $n bytes
    , last :: !Word256    -- 64 bytes
    , sign :: !Signature  -- 64 bytes
    , xtra :: !ByteString
    }

data BackCode
    = ALL_GOOD
    | BAD_FORM
    | PAY_MORE
    | NOT_SENT

data BackPacket = BP
    { cod :: !BackCode
    , seq :: !Word32
    }

data Packet
    = BACK !BackPacket
    | HELO !Word256
    | AUTH !AuthPacket
    | SEND !SendPacket
