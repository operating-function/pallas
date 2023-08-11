-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall -Werror #-}

module Hash256
    ( Hash256(..)
    , encodeBtc
    , decodeBtc
    , toHash256
    , buildHash
    , hashToByteString
    , shortHex
    , btcToHash
    , hashToBTC
    )
where

import Data.Hashable
import Fan.Print             (decodeBtc, encodeBtc)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import Numeric               (showHex)
import PlunderPrelude        hiding (Builder)

import Data.ByteString.Builder (Builder, word64LE)

import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Internal as BS

data Hash256 = Hash256 {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
  deriving (Ord, Eq, NFData, Generic)

instance Show Hash256 where
    show = show . hashToBTC

instance IsString Hash256 where
  fromString str =
    let decoded = decodeBtc $ pack str
    in if length decoded /= 32
       then error $ concat [ "Hash256: Bad hash value: ", show str ]
       else toHash256 decoded

instance BA.ByteArrayAccess Hash256 where
  length _ = 32

  withByteArray h fun = do
    allocaBytes 32 $ \pK -> do
      poke pK h
      fun $ castPtr pK

instance Hashable Hash256 where
  hashWithSalt s (Hash256 a _ _ _) = hashWithSalt s a

instance Storable Hash256 where
  sizeOf _ = 32
  alignment _ = 0

  peek inP = do
    let p :: Ptr Word64 = castPtr inP
    a <- peekElemOff p 0
    b <- peekElemOff p 1
    c <- peekElemOff p 2
    d <- peekElemOff p 3
    pure $ Hash256 a b c d

  poke outP (Hash256 a b c d) = do
    let p :: Ptr Word64 = castPtr outP
    pokeElemOff p 0 a
    pokeElemOff p 1 b
    pokeElemOff p 2 c
    pokeElemOff p 3 d


toHash256 :: (BA.ByteArrayAccess ba) => ba -> Hash256
toHash256 ba = unsafePerformIO $ BA.withByteArray ba peek

buildHash :: Hash256 -> Builder
buildHash (Hash256 a b c d) = word64LE a <> word64LE b <> word64LE c
                           <> word64LE d

hashToByteString :: Hash256 -> ByteString
hashToByteString h = unsafePerformIO $ do
  let sz = 32
  fp <- BS.mallocByteString sz
  withForeignPtr fp $ \p -> poke p h
  pure (BS.PS (castForeignPtr fp) 0 sz)

shortHex :: Hash256 -> ByteString
shortHex (Hash256 a _ _ _) = encodeUtf8 $ pack $ showHex a ""

btcToHash :: Text -> Hash256
btcToHash = toHash256 . decodeBtc

hashToBTC :: Hash256 -> Text
hashToBTC = encodeBtc . hashToByteString
