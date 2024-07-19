-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.LmdbStore.MdbValue
    ( MdbValue(..)
    )
where

import Database.LMDB.Raw
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Hash256
import PlunderPrelude

import qualified Data.ByteArray  as BA
import qualified Data.ByteString as BS


-- Class -----------------------------------------------------------------------

class MdbValue a where
    withMdbVal :: a -> (MDB_val -> IO b)     -> IO b
    loadMdbVal :: MDB_val -> IO a

    {-# INLINE withMdbValPtr #-}
    withMdbValPtr :: a -> (Ptr MDB_val -> IO b) -> IO b
    withMdbValPtr val act =
        withMdbVal val \mdb ->
            allocaBytes (sizeOf mdb) \p -> do
                poke p mdb
                act p

    {-# INLINE peekMdbVal #-}
    peekMdbVal :: Ptr MDB_val -> IO a
    peekMdbVal ptr = peek ptr >>= loadMdbVal @a


-- Instances -------------------------------------------------------------------

instance MdbValue ByteString where
    {-# INLINE loadMdbVal #-}
    loadMdbVal (MDB_val sz ptr) =
        BS.packCStringLen (castPtr ptr, fromIntegral sz)

    {-# INLINE withMdbVal #-}
    withMdbVal bs act =
        BA.withByteArray bs \ptr ->
            act (MDB_val (fromIntegral $ length bs) ptr)

instance MdbValue Hash256 where
    {-# INLINE withMdbVal #-}
    withMdbVal h act =
        allocaBytes 32 \pK -> do
            poke pK h
            act (MDB_val 32 (castPtr pK))

    {-# INLINE loadMdbVal #-}
    loadMdbVal (MDB_val sz ptr) =
        if sz /= 32
        then error ("Invalid hash value: length=" <> show sz)
        else peek (castPtr ptr)

instance MdbValue Word64 where
    {-# INLINE withMdbVal #-}
    withMdbVal w act =
        allocaBytes 8 \p -> do
            poke p w
            act (MDB_val 8 (castPtr p))

    {-# INLINE loadMdbVal #-}
    loadMdbVal (MDB_val sz ptr) =
        if sz /= 8
        then error ("Invalid w64 value: length=" <> show sz)
        else peek (castPtr ptr)
