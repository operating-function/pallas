-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot #-}

module Jelly.Fast.FFI where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Prelude


-- Types -----------------------------------------------------------------------

data JellyCtx

type Ctx = Ptr JellyCtx

type CNode = Word32


-- Foreign Functions -----------------------------------------------------------

foreign import ccall unsafe "jelly_make"
    c_make :: IO Ctx

foreign import ccall unsafe "jelly_wipe"
    c_wipe :: Ctx -> IO ()

foreign import ccall unsafe "jelly_free"
    c_free :: Ctx -> IO ()

foreign import ccall unsafe "jelly_done"
    c_done :: Ctx -> IO ()

foreign import ccall unsafe "jelly_dbug"
    c_dbug :: Ctx -> IO ()

foreign import ccall unsafe "jelly_show"
    c_show :: Ctx -> IO ()

foreign import ccall unsafe "jelly_pin"
    c_pin :: Ctx -> Ptr CBool -> Ptr Word64 -> IO CNode

foreign import ccall unsafe "jelly_bar"
    c_bar :: Ctx -> CSize -> Ptr Word8 -> IO CNode

foreign import ccall unsafe "jelly_nat"
    c_nat :: Ctx -> CSize -> Ptr Word8 -> IO CNode

foreign import ccall unsafe "jelly_cons"
    c_cons :: Ctx -> CNode -> CNode -> IO CNode

foreign import ccall unsafe "jelly_word"
    c_word :: Ctx -> Word64 -> IO CNode

foreign import ccall unsafe "jelly_head_size"
    c_head_size :: Ctx -> IO CSize

foreign import ccall unsafe "jelly_body_size"
    c_body_size :: Ctx -> IO CSize

foreign import ccall unsafe "jelly_save_head"
    c_save_head :: Ctx -> CSize -> Ptr Word8 -> IO ()

foreign import ccall unsafe "jelly_save_body"
    c_save_body :: Ctx -> CSize -> Ptr Word8 -> IO ()

foreign import ccall unsafe "jelly_load_head"
    c_load_head :: Ctx -> CSize -> Ptr Word8 -> IO ()

foreign import ccall unsafe "jelly_load_body"
    c_load_body :: Ctx -> CSize -> Ptr Word8 -> IO ()

foreign import ccall unsafe "jet_blake3"
    c_jet_blake3
        :: Ptr Word8 -- 32-byte buffer
        -> CSize     -- input size
        -> Ptr Word8 -- head
        -> IO ()

foreign import ccall unsafe "jet_revmemcmp"
    c_revmemcmp :: Ptr a -> Ptr b -> CSize -> IO CInt
