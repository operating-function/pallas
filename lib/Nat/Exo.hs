{-
    TODO: QuickCheck tests

    TODO: Add another layer to supports direct atoms (like GHC.Natural)
    so that we don't have to go through the indirection when doing basic
    operations on small nats.

    TODO: Do we need anything else?  Is that everything?
-}

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Nat.Exo where

import ClassyPrelude
import Data.Bits
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Int
import GHC.Natural
import GHC.Num.BigNat
import GHC.Num.Integer
import GHC.Word
import GHC.Read

import Optics (over, _1)

import Data.Primitive.ByteArray
import Data.ByteString.Internal (ByteString(BS), nullForeignPtr)
import Foreign.Marshal.Utils    (copyBytes)
import GHC.Exts                 (Ptr(Ptr))
import GHC.Prim                 (Word#, clz#, minusWord#)
import GHC.Real                 (divZeroError)
import GHC.Types                (IO(..))
import System.IO.Unsafe         (unsafePerformIO)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T


-- Types -----------------------------------------------------------------------

type SrcPtr a = Ptr a

-- This does not correspond to the types that GMP declares (GMP
-- declaration corresponds to Foreign.C.Types.CSize), however they should
-- be equivalent on amd64, and this code is not meant to be portable.
type CSize = Word

data Exo = EXO
    { sz  :: {-# UNPACK #-} !CSize
    , ptr :: {-# UNPACK #-} !(ForeignPtr Word)
    }


-- FFI -------------------------------------------------------------------------

foreign import ccall unsafe "__gmpn_add_1" mpn_add_1
    :: Ptr Word -> SrcPtr Word -> CSize -> Word -> IO Word

foreign import ccall unsafe "__gmpn_add" mpn_add
    :: Ptr Word -> SrcPtr Word -> CSize -> SrcPtr Word -> CSize -> IO Word

foreign import ccall unsafe "__gmpn_mul" mpn_mul
    :: Ptr Word -> SrcPtr Word -> CSize -> SrcPtr Word -> CSize -> IO Word

foreign import ccall unsafe "__gmpn_mul_1" mpn_mul_1
    :: Ptr Word -> SrcPtr Word -> CSize -> Word -> IO Word

foreign import ccall unsafe "__gmpn_sub" mpn_sub
    :: Ptr Word -> SrcPtr Word -> CSize -> SrcPtr Word -> CSize -> IO Word

foreign import ccall unsafe "__gmpn_cmp" mpn_cmp
    :: SrcPtr Word -> SrcPtr Word -> CSize -> IO Int32

foreign import ccall unsafe "__gmpn_zero" mpn_zero
    :: Ptr Word -> CSize -> IO ()

foreign import ccall unsafe "__gmpn_copyi" mpn_copyi
    :: Ptr Word -> SrcPtr Word -> CSize -> IO ()

foreign import ccall unsafe "__gmpn_popcount" mpn_popcount
    :: SrcPtr Word -> CSize -> IO Word -- TODO: Is mp_bitcnt_t actually a word?

foreign import ccall unsafe "__gmpn_com" mpn_com
    :: Ptr Word -> SrcPtr Word -> CSize -> IO ()

foreign import ccall unsafe "__gmpn_and_n" mpn_and_n
    :: Ptr Word -> SrcPtr Word -> SrcPtr Word -> CSize -> IO ()

foreign import ccall unsafe "__gmpn_ior_n" mpn_ior_n
    :: Ptr Word -> SrcPtr Word -> SrcPtr Word -> CSize -> IO ()

foreign import ccall unsafe "__gmpn_xor_n" mpn_xor_n
    :: Ptr Word -> SrcPtr Word -> SrcPtr Word -> CSize -> IO ()

{-
    Divide {np, nn} by {dp, dn} and put the quotient at {qp, nn-dn+1}
    and the remainder at {rp, dn}. The quotient is rounded towards 0.

    -   No overlap is permitted between arguments, except that np might
        equal rp.

    -   The dividend size nn must be greater than or equal to divisor
        size dn.

    -   The most significant limb of the divisor must be non-zero.

    -   The qxn operand must be zero.
-}
foreign import ccall unsafe "__gmpn_tdiv_qr" mpn_tdiv_qr
    :: Ptr Word    -- qp (quotent result)
    -> Ptr Word    -- rp (remainder result)
    -> CSize       -- qxn (must be zero, but why)
    -> SrcPtr Word -- np (divident pointer)
    -> CSize       -- nn (dividend size)
    -> SrcPtr Word -- dp (divisior pointer)
    -> CSize       -- dn (divisor size)
    -> IO ()

{-
    mp_limb_t mpn_lshift
        ( mp_limb_t *rp
        , const mp_limb_t *sp
        , mp_size_t n
        , unsigned int count
        )

    Shift {sp, n} left by count bits, and write the result to {rp, n}. The
    bits shifted out at the left are returned in the least significant
    `count` bits of the return value (the rest of the return value
    is zero).

    `count` must be in the range 1 to (mp_bits_per_limb - 1).

    The regions {sp, n} and {rp, n} may overlap, provided rp >= sp.

    This function is written in assembly for most CPUs.
-}
foreign import ccall unsafe "__gmpn_lshift" mpn_lshift
    :: Ptr Word -> SrcPtr sp -> CSize -> Word -> IO Word

{-
    Shift {sp, n} right by count bits, and write the result to {rp, n}.

    The bits shifted out at the right are returned in the most significant
    count bits of the return value (the rest of the return value is zero).

    count must be in the range 1 to mp_bits_per_limb-1.

    The regions {sp, n} and {rp, n} may overlap, provided rp <= sp.

    This function is written in assembly for most CPUs.
-}
foreign import ccall unsafe "__gmpn_rshift" mpn_rshift
    :: Ptr Word -> SrcPtr sp -> CSize -> Word -> IO Word


--------------------------------------------------------------------------------

plus_word :: Word -> Exo -> Exo
plus_word 0 x              = x
plus_word w (EXO 0 _)      = wordExo w
plus_word w (EXO xSz xPtr) = unsafePerformIO do
    newPtr <- mallocForeignPtrBytes $ fromIntegral ((xSz + 1) * 8)
    withForeignPtr newPtr \new ->
      withForeignPtr xPtr \buf -> do
        carry <- mpn_add_1 new buf xSz w
        pokeElemOff new (fromIntegral xSz) carry
        let newSize = if carry==0 then xSz else xSz+1
        pure (EXO newSize newPtr)

zeroExo :: Exo
zeroExo = EXO 0 (castForeignPtr nullForeignPtr)

wordExo :: Word -> Exo
wordExo 0 = zeroExo
wordExo w = unsafePerformIO do
    newPtr <- mallocForeignPtrBytes 8
    withForeignPtr newPtr (\p -> poke p w)
    pure (EXO 1 newPtr)

wordWordExo :: Word -> Word -> Exo
wordWordExo 0 0 = zeroExo
wordWordExo 0 x = wordExo x
wordWordExo n x = unsafePerformIO do
    newPtr <- mallocForeignPtrBytes 16
    withForeignPtr newPtr \p -> do
        poke p x
        pokeElemOff p 1 n
        pure (EXO 2 newPtr)

plus :: Exo -> Exo -> Exo
plus (EXO 0 _)      y              = y
plus x              (EXO 0 _)      = x
plus x@(EXO xSz xP) y@(EXO ySz yP) =
    if xSz < ySz then
        plus y x
    else unsafePerformIO $
         withForeignPtr xP \xBuf ->
         withForeignPtr yP \yBuf -> do
             newPtr <- mallocForeignPtrBytes $ fromIntegral ((xSz + 1) * 8)
             withForeignPtr newPtr \new -> do
                 carry <- mpn_add new xBuf xSz yBuf ySz
                 pokeElemOff new (fromIntegral xSz) carry
                 let newSize = if carry==0 then xSz else xSz+1
                 pure (EXO newSize newPtr)

bigNatExo :: BigNat# -> Exo
bigNatExo bn =
    case I# (bigNatSize# bn) of
        0 -> zeroExo
        n -> unsafePerformIO do
            newPtr <- mallocForeignPtrBytes (n*8)
            withForeignPtr newPtr \new -> do
                for_ [0 .. n-1] \i@(I# i#) -> do
                    let w = bigNatIndex# bn i#
                    pokeElemOff new i (W# w)
            pure (EXO (fromIntegral n) (castForeignPtr newPtr))

-- Returns 0 when given a negative number
intExo :: Int -> Exo
intExo i | i <= 0 = zeroExo
intExo i          = wordExo (fromIntegral i)

naturalExo :: Natural -> Exo
naturalExo (NatS# w)  = wordExo (W# w)
naturalExo (NatJ# bn) = bigNatExo (unBigNat bn)

exoNatural :: Exo -> Natural
exoNatural (EXO 0  _) = 0
exoNatural (EXO 1 fp) = fromIntegral $ unsafePerformIO $ withForeignPtr fp peek
exoNatural (EXO n fp) =
    unsafePerformIO $
    withForeignPtr fp \p -> do
        arr <- newByteArray (fromIntegral n * 8)
        copyPtrToMutableByteArray arr 0 p (fromIntegral n)
        ByteArray ba <- unsafeFreezeByteArray arr
        pure (NatJ# (BN# ba))


-- Returns 0 if given a negative number.
integerExo :: Integer -> Exo
integerExo (IP i) = bigNatExo i
integerExo (IN _) = zeroExo
integerExo (IS i) = intExo (I# i)

-- Construct an exo, ignoring pointer if zero (using a null pointer instead).
exo :: CSize -> ForeignPtr Word -> Exo
exo 0 _ = zeroExo
exo n p = EXO n p

-- Decreases the size until the most-significant word is nonZero.
trim :: CSize -> Ptr Word -> IO CSize
trim = go . fromIntegral
  where
    go 0 _ = pure 0
    go n b = do
        msw <- peekElemOff b (n-1)
        case msw of
           0 -> go (n-1) b
           _ -> pure (fromIntegral n)

minusExo :: Exo -> Exo -> Exo
minusExo x (EXO 0 _) = x
minusExo (EXO 0 _) _ = zeroExo
minusExo x y         =
    unsafePerformIO do
        newPtr <- mallocForeignPtrBytes (fromIntegral (x.sz * 8))
        withForeignPtr newPtr \ptr ->
            withForeignPtr x.ptr \xBuf ->
            withForeignPtr y.ptr \yBuf -> do
                borrow  <- mpn_sub ptr xBuf x.sz yBuf y.sz
                let newSz = (if borrow == 0 then x.sz else x.sz - 1)
                newSize <- trim newSz ptr
                pure (exo newSize newPtr)

times_exo_exo :: Exo -> Exo -> Exo
times_exo_exo _ (EXO 0 _) = zeroExo
times_exo_exo (EXO 0 _) _ = zeroExo
times_exo_exo x y         = unsafePerformIO do
    let maxSize = x.sz + y.sz
    newPtr <- mallocForeignPtrBytes $ fromIntegral (maxSize * 8)
    withForeignPtr newPtr \buf ->
        withForeignPtr x.ptr \xBuf ->
        withForeignPtr y.ptr \yBuf -> do
            _        <- mpn_mul buf xBuf x.sz yBuf y.sz
            newSize' <- trim maxSize buf
            pure (exo newSize' newPtr)

times_exo_word :: Exo -> Word -> Exo
times_exo_word _ 0         = zeroExo
times_exo_word x 1         = x
times_exo_word (EXO 0 _) _ = zeroExo
times_exo_word x y =
    unsafePerformIO $
    let maxWid = (x.sz + 1) in
    withForeignPtr x.ptr \xBuf ->
    makeExo maxWid \buf -> do
        extra <- mpn_mul_1 buf xBuf x.sz y
        if extra == 0 then do
            pure x.sz
        else do
            pokeElemOff buf (fromIntegral (maxWid - 1) :: Int) extra
            pure maxWid

-- You'll need to read the docs of `mpn_tdiv_qr` alongside this code in
-- order to understand it.
exoQuotRem :: Exo -> Exo -> (Exo, Exo)
exoQuotRem (EXO 0 _) _ = (zeroExo, zeroExo)
exoQuotRem _ (EXO 0 _) = divZeroError
exoQuotRem n@(EXO nn nfp) (EXO dn dfp) | nn < dn   = (zeroExo, n)
                                       | otherwise = unsafePerformIO do
    let qxn = 0      -- That's what the docs say?  *shrug*
    let qn = nn-dn+1 -- TODO: is oper precedence same as docs?
    let rn = dn
    qfp <- mallocForeignPtrBytes $ fromIntegral (qn * 8)
    rfp <- mallocForeignPtrBytes $ fromIntegral (rn * 8)
    id $
        withForeignPtr nfp \np ->
        withForeignPtr dfp \dp ->
        withForeignPtr qfp \qp ->
        withForeignPtr rfp \rp -> do
            mpn_tdiv_qr qp rp qxn np nn dp dn
            qs <- trim qn qp
            rs <- trim rn rp
            pure (exo qs qfp, exo rs rfp)

exoShiftL :: Exo -> Word -> Exo
exoShiftL x              0 = x
exoShiftL (EXO 0 _)      _ = zeroExo
exoShiftL (EXO xSz xFP) i = unsafePerformIO do
    let (w,n) = quotRem i 64
    let extra = if n == 0 then 0 else 1
    let rWid = xSz + extra + w
    rFP <- mallocForeignPtrBytes $ fromIntegral (rWid * 8)
    id $ withForeignPtr xFP \xBuf ->
         withForeignPtr rFP \rBuf -> do
             -- The `w` least-significant-words are all zero.
             let target = rBuf `plusPtr` fromIntegral (w*8)
             mpn_zero rBuf w
             if n == 0 then do
                 mpn_copyi target xBuf xSz
                 pure (EXO rWid rFP)
             else do
                 overflow <- mpn_lshift target xBuf xSz (fromIntegral n)
                 pokeElemOff target (fromIntegral xSz) overflow
                 let newSize = if overflow==0 then rWid-1 else rWid
                 pure (EXO newSize rFP)

makeExo :: CSize -> (Ptr Word -> IO CSize) -> IO Exo
makeExo maxPossibleSize act = do
    if maxPossibleSize == 0 then pure zeroExo else do
        fp <- mallocForeignPtrBytes $ fromIntegral (maxPossibleSize * 8)
        actualSize <- withForeignPtr fp act
        pure if actualSize == 0 then zeroExo else EXO actualSize fp

exoShiftR :: Exo -> Word -> Exo
exoShiftR x              0 = x
exoShiftR (EXO 0 _)      _ = zeroExo
exoShiftR (EXO xSz xPtr) i = unsafePerformIO do
    let (nWords, nBits) = quotRem i 64
    if nWords >= xSz then
        pure zeroExo
    else
        let rSz = xSz - nWords in
        withForeignPtr xPtr \xBuf ->
        makeExo rSz \buf -> do
        let src = xBuf `plusPtr` (fromIntegral nWords * 8)
        if (nBits == 0) then
            mpn_copyi buf src rSz
        else do
            _erased <- mpn_rshift buf src rSz nBits
            pure ()
        trim rSz buf

exoPopCount :: Exo -> Int
exoPopCount (EXO 0  _ ) = 0::Int
exoPopCount (EXO sz fp) =
    unsafePerformIO $
    withForeignPtr fp \p -> do
    bits <- mpn_popcount p sz
    pure (fromIntegral bits)

-- TODO: This is probably wrong.  Data.Bits.complement says "reverse
-- all bits in the argument" whereas mpn_com seems to take the bitwise
-- complement of each word.  However, "of each word" is an implementation
-- detail, not a thing that exists for natural numbers.
exoCom :: Exo -> Exo
exoCom (EXO xS xFP) =
    unsafePerformIO $
    makeExo xS \rp ->
    withForeignPtr xFP \xp -> do
        mpn_com rp xp xS
        pure xS

{-
    If one is longer than the other, then we can just ignore the
    most-significant-words (last words) of the longer one.  This is true
    because the corresponding bits of the smaller nat will all be zero
    and (1 && 0) is 0.
-}
exoAnd :: Exo -> Exo -> Exo
exoAnd (EXO 0 _) _ = zeroExo
exoAnd _ (EXO 0 _) = zeroExo
exoAnd (EXO xS xFP) (EXO yS yFP) =
    unsafePerformIO $
    makeExo rS \rp ->
    withForeignPtr xFP \xp ->
    withForeignPtr yFP \yp ->
    mpn_and_n rp xp yp rS $> rS
  where
    rS = min xS yS

{-
    If one is longer than the other, then we can just copy all of the
    most-significant-words from the longer one.

    We don't need to trim the result because the high-bit of the largest
    input will always be set on the output.
-}
exoIor :: Exo -> Exo -> Exo
exoIor (EXO 0 _)    x            = x
exoIor x            (EXO 0 _)    = x
exoIor (EXO xS xFP) (EXO yS yFP) =
    unsafePerformIO $
    makeExo rS \rp ->
    withForeignPtr xFP \xp ->
    withForeignPtr yFP \yp -> do
        mpn_ior_n rp xp yp mS
        when (xS /= yS) do
            let numExtra = fromIntegral (rS - mS)
            let offset   = fromIntegral (mS * 8)
            let exTar    = rp `plusPtr` offset
            let exSrc    = (if xS > yS then xp else yp) `plusPtr` offset
            mpn_copyi exTar exSrc numExtra
        pure rS
  where
    mS = min xS yS
    rS = max xS yS

{-
    If one is longer than the other, then we can just copy all of the
    most-significant-words from the longer one.

    We do need to trim the result if the two lengths are the same,
    because the high-bits can get canceled out.
-}
exoXor :: Exo -> Exo -> Exo
exoXor (EXO 0 _)    x            = x
exoXor x            (EXO 0 _)    = x
exoXor (EXO xS xFP) (EXO yS yFP) =
    unsafePerformIO $
    makeExo rS \rp ->
    withForeignPtr xFP \xp ->
    withForeignPtr yFP \yp -> do
        mpn_xor_n rp xp yp mS
        if (xS /= yS) then do
            let numExtra = fromIntegral (rS - mS)
            let offset   = fromIntegral (mS * 8)
            let exTar    = rp `plusPtr` offset
            let exSrc    = (if xS > yS then xp else yp) `plusPtr` offset
            mpn_copyi exTar exSrc numExtra
            pure rS -- high bits are copied so the result is always full size.
        else
            trim rS rp -- high-bits could be canceled out.
  where
    mS = min xS yS
    rS = max xS yS

exoTestBit :: Exo -> Int -> Bool
exoTestBit (EXO 0 _)    _ = False
exoTestBit (EXO xS xFP) i =
    let (w,n) = quotRem i 64 in
    if w >= fromIntegral xS then False else
    unsafePerformIO $
    withForeignPtr xFP \xp -> do
    sector <- peekElemOff xp w
    pure (testBit (sector::Word) (n::Int))

exoBex :: Int -> Exo
exoBex i = unsafePerformIO do
    let (w,b) = quotRem i 64
    let rs    = fromIntegral w + 1
    makeExo rs \rp -> do
        mpn_zero rp rs
        pokeElemOff rp w (bit b)
        pure rs

exoSetBit :: Exo -> Int -> Exo
exoSetBit (EXO xS xFP) i = unsafePerformIO do
    let (w,b) = quotRem i 64
    let rS    = max xS (fromIntegral w + 1)
    withForeignPtr xFP \xp ->
        makeExo rS \rp -> do
            unless (rS == xS) do mpn_zero rp rS
            mpn_copyi rp xp xS
            z <- peekElemOff rp w
            pokeElemOff rp w (setBit z b)
            pure rS

exoClearBit :: Exo -> Int -> Exo
exoClearBit (EXO xS xFP) i = unsafePerformIO do
    let (w,b) = quotRem i 64
    let rS    = max xS (fromIntegral w + 1)
    withForeignPtr xFP \xp ->
        makeExo rS \rp -> do
            unless (rS == xS) do mpn_zero rp rS
            mpn_copyi rp xp xS
            z <- peekElemOff rp w
            pokeElemOff rp w (clearBit z b)
            trim rS rp

exoBigNat :: Exo -> BigNat
exoBigNat (EXO 0 _)   = bigNatZero
exoBigNat (EXO sz fp) =
    unsafePerformIO
        $ withForeignPtr fp \(Ptr a) ->
          IO \st ->
            let !(W# sz#)       = fromIntegral (sz * 8)
                !(# st', bn# #) = bigNatFromAddr# sz# a 0# st
            in (# st', BN# bn# #)

wordBitWidth# :: Word# -> Word#
wordBitWidth# w = minusWord# 64## (clz# w)

wordBitWidth :: Word -> Word
wordBitWidth (W# w) = W# (wordBitWidth# w)

exoBitWidth :: Exo -> Word
exoBitWidth (EXO 0 _)  = 0
exoBitWidth (EXO s fp) =
    unsafePerformIO $
    withForeignPtr fp \p -> do
        wrd <- peekElemOff p (fromIntegral (s-1))
        pure (64 * (s-1) + wordBitWidth wrd)


--------------------------------------------------------------------------------

{-
    Decodes a bar-encoded nat.  The high bit must be set on the byte
    following the final byte.  If the high byte is not 1, then the
    encoding is not valid.

    This routine does not copy, it re-uses the memory for the Exo.
-}
exoBar :: Exo -> Maybe ByteString
exoBar (EXO 0 _ ) = Nothing
exoBar (EXO s fp) =
    unsafePerformIO $
    withForeignPtr fp \p -> do
        let final = fromIntegral (s-1)
        wrd <- peekElemOff p final
        case barTrailingBytes wrd of
            Nothing -> pure Nothing
            Just tb -> pure
                     $ Just
                     $ BS (castForeignPtr fp) (tb + 8 * final)

barTrailingBytes :: Word -> Maybe Int
barTrailingBytes word = do
    case quotRem (countLeadingZeros word + 1) 8 of
        (q,0) -> Just (8-q)
        _     -> Nothing

numSignificantBytes :: Word -> Int
numSignificantBytes word = 8 - quot (countLeadingZeros word) 8

{-
    Converts an exo to a byte-string.

    The length of the result is the number of significant bytes in the
    input, so the final byte is never 0.

    This routine does not copy, it re-uses the memory for the Exo.
-}
exoBytes :: Exo -> ByteString
exoBytes (EXO 0 _ ) = mempty
exoBytes (EXO s fp) =
    unsafePerformIO $
    withForeignPtr fp \p -> do
        let final = fromIntegral (s-1)
        wrd <- peekElemOff p final
        let fz = numSignificantBytes wrd
        let sz = fz + (8 * final)
        pure $ BS (castForeignPtr fp) sz

exoUtf8 :: Exo -> Either T.UnicodeException T.Text
exoUtf8 = T.decodeUtf8' . exoBytes

-- | Interpret an exo as utf8 text, throwing an exception on bad unicode.
exoUtf8Exn :: Exo -> T.Text
exoUtf8Exn = T.decodeUtf8 . exoBytes

-- | Interpret an exo as utf8 text, replacing bad unicode characters.
exoUtf8Lenient :: Exo -> T.Text
exoUtf8Lenient = T.decodeUtf8With T.lenientDecode . exoBytes


-- Convert Bytes to Exos -------------------------------------------------------

trim' :: Exo -> IO Exo
trim' (EXO sz fp) =
    withForeignPtr fp \p -> do
        sz' <- trim sz p
        pure (EXO sz' fp)

{-
    This converts a bytestring to an exo.

    If the length of the bytestring is a multiple of 8, then this re-uses
    the origional buffer, otherwise we copy and create a new buffer.
-}
bytesExo :: ByteString -> Exo
bytesExo (BS bFp bSz) =
    if bSz == 0 then zeroExo else
    case quotRem bSz 8 of
        (q, 0) -> unsafePerformIO $ trim' $ EXO (fromIntegral q) fp
        (q, _) -> let rxs = fromIntegral q + 1 in
                  unsafePerformIO $
                  withForeignPtr fp \bsPtr ->
                  makeExo rxs \p -> do
                      mpn_zero p rxs
                      copyBytes p bsPtr bSz
                      trim rxs p
  where
    fp = castForeignPtr bFp

utf8Exo :: T.Text -> Exo
utf8Exo = bytesExo . T.encodeUtf8

{-
    This converts from a bytestring to a pad-encoded exo.

    This pad-encoding requires that a one-byte be added to the end,
    so this always needs to perform a copy.
-}
barExo :: ByteString -> Exo
barExo (BS bFp bSz) =
    unsafePerformIO $
    withForeignPtr fp \bsPtr ->
    makeExo rS \p -> do
        mpn_zero p rS
        copyBytes p bsPtr bSz
        pokeByteOff p bSz (1 :: Word8)
        pure rS
  where
    fp = castForeignPtr bFp
    rS = let (w,r) = quotRem (bSz + 1) 8 in fromIntegral (w + min r 1)


-- Instances -------------------------------------------------------------------

instance Show Exo where
    show = show . exoNatural

instance Num Exo where
    (+) = plus
    (-) = minusExo
    (*) = times_exo_exo

    abs          = id
    signum x     = if x.sz==0 then 0 else 1
    fromInteger  = integerExo
    negate EXO{} = zeroExo

instance Eq Exo where
    (==) (EXO 0 _)   (EXO 0 _)     = True
    (==) (EXO xSz xP) (EXO ySz yP) =
        if xSz /= ySz then
            False
        else
            unsafePerformIO $
            withForeignPtr xP \xBuf ->
            withForeignPtr yP \yBuf -> do
                res <- mpn_cmp xBuf yBuf xSz
                pure (res == 0)

instance Ord Exo where
    compare (EXO 0 _)    (EXO 0 _)    = EQ
    compare (EXO xSz xP) (EXO ySz yP) =
        case compare xSz ySz of
            LT -> LT
            GT -> GT
            EQ -> unsafePerformIO $
                  withForeignPtr xP \xBuf ->
                  withForeignPtr yP \yBuf -> do
                      res <- mpn_cmp xBuf yBuf xSz
                      pure (compare res 0)

instance Real Exo where
    toRational x = toRational (toInteger x)

instance Integral Exo where
    quotRem = exoQuotRem
    divMod  = exoQuotRem

    toInteger x@(EXO w p) =
        case w of
            0 -> 0
            1 -> unsafePerformIO $ withForeignPtr p $ fmap toInteger . peek
            _ -> let !(BN# bn#) = exoBigNat x
                 in IP bn#

instance Enum Exo where
    succ x   = plus_word 1 x
    pred x   = minusExo x 1
    toEnum   = intExo
    fromEnum = fromIntegral

instance NFData Exo where
    rnf EXO{} = ()

instance Bits Exo where
    isSigned _     = False
    bitSizeMaybe _ = Nothing
    bitSize _      = error "Data.Bits.bitSize(Exo)"

    (.&.)      = exoAnd
    (.|.)      = exoIor
    xor        = exoXor
    complement = error "Bits.complement: Natural complement undefined"
    shiftL x i = exoShiftL x (fromIntegral i)
    shiftR x i = exoShiftR x (fromIntegral i)
    rotateL    = shiftL
    rotateR    = shiftR
    testBit    = exoTestBit
    setBit     = exoSetBit
    clearBit   = exoClearBit
    bit        = exoBex
    popCount   = exoPopCount

instance IsString Exo where
    fromString = utf8Exo . pack

instance Read Exo where
    readsPrec p str = over _1 naturalExo <$> readsPrec p str
