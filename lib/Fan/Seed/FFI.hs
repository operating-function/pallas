-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot #-}

module Fan.Seed.FFI where

import PlunderPrelude (bracket)
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


-- Utils -----------------------------------------------------------------------

{-# INLINE withContext #-}
withContext :: (Ctx -> IO a) -> IO a
withContext = bracket c_make c_free


-- Foreign Functions -----------------------------------------------------------

foreign import ccall unsafe "seed_make"
    c_make :: IO Ctx

foreign import ccall unsafe "seed_hole"
    c_hole :: Ctx -> IO ()

foreign import ccall unsafe "seed_wipe"
    c_wipe :: Ctx -> IO ()

foreign import ccall unsafe "seed_free"
    c_free :: Ctx -> IO ()

foreign import ccall unsafe "seed_done"
    c_done :: Ctx -> IO ()

foreign import ccall unsafe "seed_dbug"
    c_dbug :: Ctx -> IO ()

foreign import ccall unsafe "seed_show"
    c_show :: Ctx -> IO ()

foreign import ccall unsafe "seed_barnat"
    c_barnat :: Ctx -> CSize -> Ptr Word8 -> IO CNode

foreign import ccall unsafe "seed_nat"
    c_nat :: Ctx -> CSize -> Ptr Word8 -> IO CNode

foreign import ccall unsafe "seed_cons"
    c_cons :: Ctx -> CNode -> CNode -> IO CNode

foreign import ccall unsafe "seed_touch"
    c_touch :: Ctx -> CNode -> IO ()

foreign import ccall unsafe "seed_word"
    c_word :: Ctx -> Word64 -> IO CNode

foreign import ccall unsafe "seed_size"
    c_size :: Ctx -> IO CSize

foreign import ccall unsafe "seed_save"
    c_save :: Ctx -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "seed_load"
    c_load :: Ctx -> CSize -> Ptr Word8 -> IO ()
