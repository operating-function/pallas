module Fan.FFI where

import Foreign.Ptr
import Foreign.C.Types
import GHC.Word
import Prelude

foreign import ccall unsafe "jet_blake3"
    c_jet_blake3
        :: Ptr Word8 -- 32-byte buffer
        -> CSize     -- input size
        -> Ptr Word8 -- head
        -> IO ()

foreign import ccall unsafe "jet_revmemcmp"
    c_revmemcmp :: Ptr a -> Ptr b -> CSize -> IO CInt
