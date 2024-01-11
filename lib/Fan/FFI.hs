{-# LANGUAGE EmptyDataDecls #-}

module Fan.FFI where

import Foreign.Ptr
import Foreign.C.Types
import GHC.Word
import Prelude

data Hasher

foreign import ccall unsafe "jet_blake3"
    c_jet_blake3
        :: Ptr Word8 -- 32-byte buffer
        -> CSize     -- input size
        -> Ptr Word8 -- head
        -> IO ()

foreign import ccall unsafe "blake3_hasher_new"
    c_jet_blake3_hasher_new :: IO (Ptr Hasher)

foreign import ccall unsafe "jet_blake3_hasher_update"
    c_jet_blake3_hasher_update :: Ptr Hasher -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "jet_blake3_hasher_finalize"
    c_jet_blake3_hasher_finalize :: Ptr Hasher -> Ptr Word8 -> IO ()

foreign import ccall unsafe "jet_revmemcmp"
    c_revmemcmp :: Ptr a -> Ptr b -> CSize -> IO CInt
