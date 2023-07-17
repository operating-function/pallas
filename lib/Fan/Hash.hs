module Fan.Hash (fanHash) where

import Fan.Types
import Fan.Eval (lawName, lawArgs, lawBody)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import PlunderPrelude hiding ((^))

import Foreign.Storable (peek)
import GHC.Int          (Int(..))
import GHC.Natural      (Natural(..))
import GHC.Word         (Word(..))
import Jelly.Types      (toHash256)

import qualified Data.ByteArray            as BA
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Unsafe    as BS
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Fan.Prof                  as Prof
import qualified GHC.Integer.GMP.Internals as G
import qualified GHC.Num.BigNat            as BN


-- Foreign Imports -------------------------------------------------------------

newtype Hasher = HASHER { ptr :: Ptr Void }

foreign import ccall unsafe "blake3_hasher_new"
    c_blake3_hasher_new :: IO Hasher

foreign import ccall unsafe "blake3_hasher_update"
    c_blake3_hasher_update :: Hasher -> Ptr Void -> CSize -> IO ()

foreign import ccall unsafe "blake3_hasher_update_byte"
    c_blake3_hasher_update_byte :: Hasher -> Word8 -> IO ()

foreign import ccall unsafe "blake3_hasher_update_word"
    c_blake3_hasher_update_word :: Hasher -> Word64 -> IO ()

foreign import ccall unsafe "blake3_hasher_finalize"
    c_blake3_hasher_finalize :: Hasher -> Ptr Void -> CSize -> IO ()


--------------------------------------------------------------------------------

c_blake3_hasher_update_bs :: Hasher -> ByteString -> IO ()
c_blake3_hasher_update_bs h bs =
    BS.unsafeUseAsCStringLen bs \(buf, cs) -> do
        c_blake3_hasher_update h (castPtr buf) (fromIntegral cs)

(^) :: SmallArray a -> Int -> a
(^) = indexSmallArray

withHasher :: (Hasher -> IO a) -> IO a
withHasher act = do
    bracket c_blake3_hasher_new release act
  where
    release h = free h.ptr

fanHash :: Fan -> Hash256
fanHash top =
    unsafePerformIO $
    Prof.withSimpleTracingEvent "newhash" "pin" $
    withHasher \h -> do
        go h top
        allocaBytes 32 \buf -> do
            c_blake3_hasher_finalize h buf 32
            peek (castPtr buf)
  where
    go :: Hasher -> Fan -> IO ()
    go h = \case
        -- <p> (0 p.hash)
        PIN p -> do
            c_blake3_hasher_update_byte h 0
            BA.withByteArray p.hash \ptr ->
                c_blake3_hasher_update h (castPtr ptr) 32

        -- {n a b} -> (1 n a b)
        FUN l -> do
            c_blake3_hasher_update_byte h 1
            doNat h l.name.nat
            doNat h l.args
            go h l.body

        -- (f x y z) -> (2 3 f x y z)
        klo@KLO{} -> do
            loop 0 [] klo
          where
            loop :: Int -> [Fan] -> Fan -> IO ()
            loop n xs = \case
               KLO _ e ->
                   loop (n + (sizeofSmallArray e - 1))
                        (drop 1 (toList e) <> xs)
                        (e^0)
               f -> do
                   c_blake3_hasher_update_byte h 2
                   c_blake3_hasher_update_word h (fromIntegral n)
                   go h f
                   traverse_ (go h) xs

        NAT n -> do
            c_blake3_hasher_update_byte h 3
            doNat h n

        BAR bs -> do
            c_blake3_hasher_update_byte h 4
            c_blake3_hasher_update_word h (fromIntegral $ length bs)
            BS.unsafeUseAsCStringLen bs \(ptr, sz) ->
                c_blake3_hasher_update h (castPtr ptr) (fromIntegral sz)

        COw n -> do
            c_blake3_hasher_update_byte h 5
            doNat h n

        ROW v -> do
            c_blake3_hasher_update_byte h 6
            c_blake3_hasher_update_word h (fromIntegral $ length v)
            traverse_ (go h) v

        CAB v -> do
            c_blake3_hasher_update_byte h 7
            c_blake3_hasher_update_word h (fromIntegral $ S.size v)
            traverse_ (go h) (S.toAscList v)

        TAb v -> do
            c_blake3_hasher_update_byte h 8
            c_blake3_hasher_update_word h (fromIntegral $ M.size v)
            traverse_ (go h) (M.keys v)
            traverse_ (go h) (M.elems v)

        v@(REX r) -> do -- It's a law
            c_blake3_hasher_update_byte h 1
            doNat h (lawName v)
            doNat h (lawArgs v)
            go h (lawBody v)


    doNat h = \case

        NatS# w# -> do
            let w = W# w#
            if w == 0 then do
                c_blake3_hasher_update_word h 0
            else do
                c_blake3_hasher_update_word h 1
                c_blake3_hasher_update_word h (fromIntegral w)

        NatJ# bn -> do
            let bn#   = G.unBigNat bn
            let sz    = BN.bigNatSize bn#
            let szInt = fromIntegral sz :: Int
            c_blake3_hasher_update_word h (fromIntegral sz)
            for_ [0 .. szInt-1] \(I# i) ->
                c_blake3_hasher_update_word h
                    $ fromIntegral
                    $ BN.bigNatIndex bn# i
