-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

{-|
    Fast conversions between natural numbers and other types (text,
    bytestring, word-vector).

    This assumes a little-endian, 64 bit architecture.
-}

module Natty
    ( Nat
    , bigNatWords
    , bytesNat
    , dropTrailingZeros
    , countTrailingZeros
    , exportBytes
    , natBitWidth
    , natBitWidth#
    , natBytes
    , natUtf8
    , natUtf8Exn
    , natUtf8Lenient
    , natWords
    , slowBytesNat
    , slowNatBytes
    , slowNatWords
    , slowWordsNat
    , takeBitsWord
    , utf8Nat
    , wordBitWidth
    , wordBitWidth#
    , wordsBigNat#
    , wordsNat
    )
where

import Prelude

import Control.Monad.Primitive (primitive_)
import Data.Bits               (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString         (ByteString)
import Data.Vector.Primitive   (Vector(..))
import Data.Word               (Word8)
import GHC.Exts                (Int(..), IsString(..), Ptr(Ptr),
                                sizeofByteArray#)
import GHC.Natural             (Natural(..))
import GHC.Num.BigNat          (BigNat(..), BigNat#)
import GHC.Prim                (Addr#, Int#, Word#, clz#, copyByteArrayToAddr#,
                                int2Word#, minusWord#, plusWord#, timesWord#)
import GHC.Types               (IO(..))
import GHC.Word                (Word(..))
import System.IO.Unsafe        (unsafePerformIO)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS
import qualified Data.ByteString.Unsafe    as BS
import qualified Data.Primitive.ByteArray  as Prim
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Encoding.Error  as T
import qualified Data.Vector.Primitive     as VP
import qualified Foreign.ForeignPtr        as Ptr
import qualified Foreign.ForeignPtr.Unsafe as Ptr
import qualified GHC.Integer.GMP.Internals as G
import qualified GHC.Num.BigNat            as BN
import qualified GHC.Num.Natural           as N

type Nat = Natural


-- Orphan Instances ------------------------------------------------------------

instance IsString Nat where
  fromString = bytesNat . T.encodeUtf8 . T.pack


-- Naive Reference Implementations ---------------------------------------------

-- | Natural number to LSB-ByteString.
slowNatBytes :: Natural -> ByteString
slowNatBytes = BS.pack . go []
 where
  go acc 0 = reverse acc
  go acc n = go (fromIntegral n : acc) (shiftR n 8)

-- | LSB-first ByteString to Natural number.
slowBytesNat :: ByteString -> Natural
slowBytesNat = BS.foldr' go 0
 where
  go :: Word8 -> Natural -> Natural
  go byt acc = shiftL acc 8 .|. fromIntegral byt

-- | LSW-first Word Vector to Natural number.
slowWordsNat :: Vector Word -> Natural
slowWordsNat = VP.foldr' go 0
 where
  go :: Word -> Natural -> Natural
  go wor acc = shiftL acc 64 .|. fromIntegral wor

-- | Natural number to LSW-first Word Vector.
slowNatWords :: Natural -> Vector Word
slowNatWords = VP.fromList . go []
 where
  go acc 0 = reverse acc
  go acc n = go (fromIntegral n : acc) (shiftR n 64)


-- Utils for working with words ------------------------------------------------

wordBitWidth# :: Word# -> Word#
wordBitWidth# w = minusWord# 64## (clz# w)

wordBitWidth :: Word -> Word
wordBitWidth (W# w) = W# (wordBitWidth# w)

bigNatBitWidth# :: BigNat# -> Word#
bigNatBitWidth# nat =
  lswBits `plusWord#` ((int2Word# lastIdx) `timesWord#` 64##)
 where
  !(I# lastIdx) = (I# (BN.bigNatSize# nat)) - 1
  lswBits       = wordBitWidth# (BN.bigNatIndex# nat lastIdx)

natBitWidth# :: Nat -> Word#
natBitWidth# (NatS# gl) = wordBitWidth# gl
natBitWidth# (NatJ# bn) = bigNatBitWidth# (G.unBigNat bn)

natBitWidth :: Num a => Nat -> a
natBitWidth a = fromIntegral (W# (natBitWidth# a))

{-# INLINE takeBitsWord #-}
takeBitsWord :: Int -> Word -> Word
takeBitsWord wid wor = wor .&. (shiftL 1 wid - 1)


-- Fast Versions ---------------------------------------------------------------

-- | Cast a BigNat to a vector without a copy.
bigNatWords# :: BigNat# -> Vector Word
bigNatWords# bn | BN.bigNatIsZero bn = mempty
bigNatWords# bArr =
    Vector 0 (I# (sizeofByteArray# bArr) `div` 8) (Prim.ByteArray bArr)

bigNatWords :: BigNat -> Vector Word
bigNatWords (BN# bn#) = bigNatWords# bn#

-- | Cast a vector to a BigNat. This will not copy.
wordsBigNat# :: Vector Word -> BigNat#
wordsBigNat# v@(Vector off (I# len#) (Prim.ByteArray buf)) =
    case VP.length v of
        0 -> G.unBigNat BN.bigNatZero
        1 -> case VP.unsafeIndex v 0 of
            W# w -> G.unBigNat (G.wordToBigNat w)
        _ -> if off /= 0
            then error "words2Nat: bad-vec"
            else G.unBigNat (BN.bigNatFromWordArray buf (int2Word# len#))
-- TODO Don't crash if given a slice.

-- | Cast a nat to a vector (no copy)
natWords :: Nat -> Vector Word
natWords n = bigNatWords# (N.naturalToBigNat# n)

-- | Cast a vector to a nat (no copy)
-- TODO Can I use bigNatFromWordArray# instead?
wordsNat :: Vector Word -> Nat
wordsNat vec = N.naturalFromBigNat# (wordsBigNat# vec)

wordsBS :: Vector Word -> ByteString
wordsBS = dropTrailingZeros . vecBytes . wordsToBytes

wordsToBytes :: Vector Word -> Vector Word8
wordsToBytes (Vector off sz buf) = Vector (off * 8) (sz * 8) buf

vecBytes :: Vector Word8 -> ByteString
vecBytes (Vector off sz buf) = unsafePerformIO $ do
  fp <- BS.mallocByteString sz
  let !(Ptr a) = Ptr.unsafeForeignPtrToPtr fp -- Safe b/c returning fp
  copyByteArrayToAddr a buf 0 sz
  pure (BS.PS fp off sz)
 where
  unI# :: Int -> Int#
  unI# (I# n#) = n#

  --  Hack to get GHCJS build working, since it has an old version of the
  --  `primitive` library.
  copyByteArrayToAddr dst# (Prim.ByteArray src#) soff siz =
    primitive_ (copyByteArrayToAddr# src# (unI# soff) dst# (unI# siz))

-- | Convert an Nat to a bytestring. O(n), basically just memcopy.
natBytes :: Nat -> ByteString
natBytes = dropTrailingZeros . wordsBS . natWords

exportNatToAddr :: Nat -> Addr# -> IO Word
exportNatToAddr (NatS# w) a       = BN.bigNatToAddr (BN.bigNatFromWord# w) a 0#
exportNatToAddr (NatJ# (BN# n)) a = BN.bigNatToAddr n a 0#

exportNatToByteString :: Nat -> ByteString
exportNatToByteString nat =
    unsafePerformIO $ do
        let sz# = N.naturalSizeInBase# 256## nat
        let szi = fromIntegral (W# sz#)
        fp <- BS.mallocByteString szi
        let !(Ptr a) = Ptr.unsafeForeignPtrToPtr fp
        _ <- exportNatToAddr nat a
        pure (BS.PS fp 0 szi)

exportBytes :: Nat -> ByteString
exportBytes 0 = mempty
exportBytes n = exportNatToByteString n

{-# INLINE dropTrailingZeros #-}
dropTrailingZeros :: ByteString -> ByteString
dropTrailingZeros buf =
    BS.take (BS.length buf - countTrailingZeros buf) buf

{-# INLINE countTrailingZeros #-}
countTrailingZeros :: ByteString -> Int
countTrailingZeros buf =
    go 0 (len - 1)
  where
    len = BS.length buf
    go n i | i < 0                     = n -- TODO Should this be `0`?
           | 0 == BS.unsafeIndex buf i = go (n + 1) (i - 1)
           | otherwise                 = n

{- |
  Convert a bytestring to an Nat. O(n), copies.

  This always uses GMP's `export` routine, since it's portable and faster
  than my hand-rolled implementation.
-}
bytesNat :: ByteString -> Nat
bytesNat =
    \bs -> go (dropTrailingZeros bs)
  where
    go (BS.PS fp 0 sz) = unsafePerformIO $ do
        let !(Ptr a)  = Ptr.unsafeForeignPtrToPtr fp -- TODO Not safe!
        let !(W# sz#) = fromIntegral sz
        !res <- (IO \st ->
                   let !(# st', bn# #) = BN.bigNatFromAddr# sz# a 0# st
                   in (# st', N.naturalFromBigNat# bn# #))
        Ptr.touchForeignPtr fp
        pure res

    -- TODO Avoid this extra copy when given a slice. Should be able to
    -- just offset the raw pointer?
    go bs = bytesNat (BS.copy bs)


-- String/Cord Conversion ------------------------------------------------------

-- | Encode a utf8-encoded nat from text.
utf8Nat :: T.Text -> Nat
utf8Nat = bytesNat . T.encodeUtf8

-- | Interpret an nat as utf8 text.
natUtf8 :: Nat -> Either T.UnicodeException T.Text
natUtf8 = T.decodeUtf8' . natBytes

-- | Interpret an nat as utf8 text, throwing an exception on bad unicode.
natUtf8Exn :: Nat -> T.Text
natUtf8Exn = T.decodeUtf8 . natBytes

-- | Interpret an nat as utf8 text, replacing bad unicode characters.
natUtf8Lenient :: Nat -> T.Text
natUtf8Lenient = T.decodeUtf8With T.lenientDecode . natBytes
