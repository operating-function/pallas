{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE UnboxedTuples #-}

module Nat
    ( natBytes, bytesNat
    , natUtf8, utf8Nat
    , natBitWidth
    , Nat(..)
    , Natural
    , Exo(..)
    , natUtf8Exn
    , natUtf8Lenient
    )
where

import Data.Bits
import Nat.Exo
import Prelude
import System.IO.Unsafe
import Foreign.ForeignPtr
import Foreign.Storable

import Optics        (over, _1)
import ClassyPrelude (ByteString, IsString(..), NFData(..), pack)
import GHC.Exts      (Word(..), Int(..), Word#, addWordC#, timesWord2#)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified GHC.Natural              as GHC
import qualified GHC.Num.Integer          as GHC

--------------------------------------------------------------------------------

type Natural = Nat

data Nat
    = NatS# Word#
    | NatJ# {-# UNPACK #-} !Exo

instance Eq Nat where
    NatS#{} == NatJ#{} = False
    NatJ#{} == NatS#{} = False
    NatS# x == NatS# y = W# x == W# y
    NatJ# x == NatJ# y = x == y

instance Ord Nat where
    compare (NatS# _) (NatJ# _) = LT
    compare (NatJ# _) (NatS# _) = GT
    compare (NatS# x) (NatS# y) = compare (W# x) (W# y)
    compare (NatJ# x) (NatJ# y) = compare x y

instance Show Nat where
    showsPrec p (NatS# w) = showsPrec p (W# w)
    showsPrec p (NatJ# x) = showsPrec p x

instance NFData Nat where
    rnf NatS#{} = ()
    rnf NatJ#{} = ()

instance Real Nat where
    toRational (NatS# w) = toRational (W# w)
    toRational (NatJ# x) = toRational x

instance Integral Nat where
    toInteger (NatS# w) = toInteger (W# w)
    toInteger (NatJ# x) = toInteger x

    divMod x y = quotRem x y

    quotRem (NatS# x) (NatS# y) =
        let (q,r) = quotRem (W# x) (W# y)
        in -- trace (show (("x"::String, W# x), ("y"::String, W# y), ("q"::String, q), ("r"::String, r)))
           (wordNat q, wordNat r)

    quotRem (NatJ# x) (NatJ# y) =
        let (q,r) = quotRem x y
        in (exoNat q, exoNat r)

    quotRem n@(NatS# _) (NatJ# _) = (NatS# 0##, n)

    quotRem (NatJ# x) (NatS# y) =
        -- TODO: So slow
        let (q, r) = quotRem x (wordExo (W# y))
        in (exoNat q, exoNat r)

exoNat :: Exo -> Nat
exoNat = \case
    EXO 0 _  -> NatS# 0##
    EXO 1 fp -> doWord fp
    x        -> NatJ# x
  where
    doWord fp = unsafePerformIO do
        !(W# w) <- withForeignPtr fp peek
        pure (NatS# w)

{-# INLINE wordNat #-}
wordNat :: Word -> Nat
wordNat (W# w) = NatS# w

-- The high-word must be non-zero
word_word_exo :: Word# -> Word# -> Exo
word_word_exo h l = unsafePerformIO do
    newPtr <- mallocForeignPtrBytes 16
    withForeignPtr newPtr \p -> do
        pokeElemOff p 0 (W# l)
        pokeElemOff p 1 (W# h)
        pure (EXO 2 newPtr)


plus_word_word :: Word# -> Word# -> Nat
plus_word_word x y =
    let !(# r, c #) = addWordC# x y in
    case c of
      0# -> NatS# r
      _  -> NatJ# (word_word_exo 1## r)

plus_word_exo :: Word# -> Exo -> Nat
plus_word_exo 0## y = NatJ# y
plus_word_exo x   y = NatJ# (wordExo (W# x) + y)
  -- TODO: Optimize

intNat :: Int -> Nat
intNat = wordNat . toEnum

instance Enum Nat where
    succ (NatS# w) = plus_word_word w 1##
    succ (NatJ# x) = NatJ# (succ x)

    pred (NatS# 0##) = NatS# 0##
    pred (NatS# w)   = wordNat (W# w - 1)
    pred (NatJ# x)   = exoNat (pred x)

    toEnum = intNat

    fromEnum = fromIntegral

plus_nat_nat :: Nat -> Nat -> Nat
plus_nat_nat = \cases
    (NatS# x) (NatS# y) -> plus_word_word x y
    (NatS# w) (NatJ# x) -> plus_word_exo w x
    (NatJ# x) (NatS# w) -> plus_word_exo w x
    (NatJ# x) (NatJ# y) -> NatJ# (x + y)

-- _natExo :: Nat -> Exo
-- _natExo (NatJ# x) = x
-- _natExo (NatS# w) = wordExo (W# w)

times_nat_nat :: Nat -> Nat -> Nat
times_nat_nat = \cases
    (NatS# x) (NatS# y) ->
        case timesWord2# x y of
            (# 0## , w #) -> NatS# w
            (# h,    l #) -> NatJ# (word_word_exo h l)

    (NatS# 0##) NatJ#{}   -> NatS# 0##
    (NatS# 1##) y@NatJ#{} -> y
    (NatS# x)   (NatJ# y) -> NatJ# (times_exo_word y (W# x))

    NatJ#{}   (NatS# 0##) -> NatS# 0##
    y@NatJ#{} (NatS# 1##) -> y
    (NatJ# x) (NatS# y)   -> NatJ# (times_exo_word x (W# y))

    (NatJ# x) (NatJ# y) -> NatJ# (x * y)

minusNat :: Nat -> Nat -> Nat
minusNat = \cases

    -- TODO: Use unboxed representation throughout
    (NatS# x) (NatS# y) ->
        let { wx = W# x ; wy = W# y } in
        if wy >= wx
        then NatS# 0##
        else wordNat (wx - wy)

    -- TODO: Optimize
    (NatS# x) (NatJ# y) -> exoNat (wordExo (W# x) - y)
    (NatJ# x) (NatS# y) -> exoNat (x - wordExo (W# y))
    (NatJ# x) (NatJ# y) -> exoNat (x - y)

instance Num Nat where
    (+) = plus_nat_nat
    (-) = minusNat
    (*) = times_nat_nat

    abs !x = x

    fromInteger (GHC.IN _) =
        error "Nat.fromInteger: underflow"

    fromInteger (GHC.IS i#) =
        let i = I# i# in
        wordNat $
        if i < 0
        then error "Nat.fromInteger: underflow"
        else fromIntegral i

    fromInteger x@GHC.IP{} = exoNat (fromIntegral x)

    negate (NatS# 0##) = NatS# 0##
    negate _           = error "Nat.negate: underflow"

    signum (NatS# 0##) = 0
    signum _           = 1

instance IsString Nat where
    fromString = utf8Nat . pack

natShiftR :: Nat -> Int -> Nat
natShiftR (NatS# w#) i = wordNat (W# w# `shiftR` i)
natShiftR (NatJ# x)  i = exoNat (x `shiftR` i)

natShiftL :: Nat -> Int -> Nat
natShiftL n@(NatS# 0##) _ = n
natShiftL   (NatS# x)   i =
    if fromIntegral (W# (wordBitWidth# x)) + i <= 64
    then wordNat (W# x `shiftL` i)
    else NatJ# (wordExo (W# x) `shiftL` i)

natShiftL (NatJ# x) i =
    NatJ# (x `shiftL` i)

natAnd :: Nat -> Nat -> Nat
natAnd = \cases
    (NatS# x) (NatS# y) -> wordNat (W# x .&. W# y) -- will never grow
    (NatJ# x) (NatJ# y) -> exoNat (x .&. y)        -- May shrink
    (NatS# x) (NatJ# y) -> exoNat (wordExo (W# x) .&. y) -- TODO just use LSW
    (NatJ# x) (NatS# y) -> exoNat (x .&. wordExo (W# y)) -- TODO just use LSW
    -- TODO: Optimize

natIor :: Nat -> Nat -> Nat
natIor = \cases
    (NatS# x) (NatS# y) -> wordNat (W# x .|. W# y) -- Will never grow
    (NatJ# x) (NatJ# y) -> NatJ# (x .|. y)         -- Will never shrink
    (NatS# x) (NatJ# y) -> NatJ# (wordExo (W# x) .|. y)
    (NatJ# x) (NatS# y) -> NatJ# (x .|. wordExo (W# y))
    -- TODO: Optimize

natXor :: Nat -> Nat -> Nat
natXor = \cases
    (NatS# x) (NatS# y) -> wordNat (W# x `xor` W# y) -- will never grow
    (NatJ# x) (NatJ# y) -> exoNat (x `xor` y)        -- May shrink
    (NatS# x) (NatJ# y) -> exoNat (wordExo (W# x) `xor` y)
    (NatJ# x) (NatS# y) -> exoNat (x `xor` wordExo (W# y))
    -- TODO: Optimize

instance Bits Nat where
    isSigned _     = False
    bitSizeMaybe _ = Nothing
    bitSize _      = error "Data.Bits.bitSize(Nat)"

    (.&.)      = natAnd
    (.|.)      = natIor
    xor        = natXor
    complement = \x -> error ("Bits.complement: Natural complement undefined: " <> show x)
    shiftL x i = natShiftL x i
    shiftR x i = natShiftR x i
    rotateL    = natShiftL
    rotateR    = natShiftR

    bit i | i<64      = wordNat (bit i)
    bit i | otherwise = NatJ# (bit i)

    popCount (NatS# x) = popCount (W# x)
    popCount (NatJ# x) = popCount x

    setBit (NatJ# x) i          = NatJ# (setBit x i)
    setBit (NatS# x) i | i < 64 = wordNat (setBit (W# x) i)
    setBit (NatS# x) i          = NatJ# (setBit (wordExo $ W# x) i)

    testBit (NatS# x) i = testBit (W# x) i
    testBit (NatJ# x) i = testBit x i

    clearBit (NatS# x) i = wordNat (clearBit (W# x) i)
    clearBit (NatJ# x) i = exoNat (clearBit x i) -- could get smaller



instance Read Nat where
   readsPrec p s = over _1 naturalNat <$> readsPrec p s

naturalNat :: GHC.Natural -> Nat
naturalNat (GHC.NatS# w)           = NatS# w
naturalNat (GHC.NatJ# (GHC.BN# x)) = NatJ# (bigNatExo x)


--------------------------------------------------------------------------------

natBytes :: Nat -> ByteString
natBytes (NatS# w) = exoBytes (wordExo $ W# w) -- TODO!  Sloooooow
natBytes (NatJ# x) = exoBytes x

bytesNat :: ByteString -> Nat
bytesNat = exoNat . bytesExo

utf8Nat :: T.Text -> Nat
utf8Nat = bytesNat . T.encodeUtf8

natUtf8 :: Nat -> Either T.UnicodeException T.Text
natUtf8 = T.decodeUtf8' . natBytes

-- | Interpret a nat as utf8 text, throwing an exception on bad unicode.
natUtf8Exn :: Nat -> T.Text
natUtf8Exn = T.decodeUtf8 . natBytes

-- | Interpret a nat as utf8 text, replacing bad unicode characters.
natUtf8Lenient :: Nat -> T.Text
natUtf8Lenient = T.decodeUtf8With T.lenientDecode . natBytes

{-# INLINE natBitWidth #-}
natBitWidth :: Integral a => Nat -> a
natBitWidth (NatJ# x) = fromIntegral (exoBitWidth x)
natBitWidth (NatS# w) = let r = wordBitWidth# w in fromIntegral (W# r)
