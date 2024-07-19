-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{- |
    `PinMetadata` is the information about each pin that we keep in LMDB.

    This is enough information to do garbage collection and to do basic
    queries like "total pin size", etc.

    TODO Once we have single-file pin-loom, also store the "loom offset".

    TODO Consider using `flat`, since we are encoding many small words
    as W64.  What does the size/speed trade-off look like in practice?
-}
module Server.LmdbStore.PinMetadata
    ( PinMetadata
    , PinMetadata_(..)
    , PinMetadataLoadExn
    )
where

import Database.LMDB.Raw
import Fan.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr
import PlunderPrelude
import Server.LmdbStore.MdbValue

import qualified Data.Vector.Storable as VS


-- Pin Metadata ----------------------------------------------------------------

{- |
    This is the information about each pin that we keep in LMDB.  This is
    enough information to do garbage collection, and to calculate the
    "true size" of a pin.

    `edges` is the pin edge-list, encoded as an array of LMDB rowIds.

    TODO: Also store the "loom offset" here, so that we can find it in
    the file.

    `edges` is polymorphic in PinMetadata_ because, when we first
    construct the pin metadata object, the new pins don't have row-ids
    assigned yet, so we omit the edge-list and replace it later.
-}
data PinMetadata_ a = PIN_META
    { hash  :: Hash256
    , size  :: Word64
    , edges :: a
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type PinMetadata = PinMetadata_ (VS.Vector Word64)

data PinMetadataLoadExn = PIN_METADATA_LOAD_EXN
    { bufferLength  :: Int
    , failureReason :: Text
    }
  deriving (Eq, Ord, Show, Exception)

instance MdbValue PinMetadata where
    withMdbVal meta act = do
        let wid = 40 + (8 * length meta.edges)

        allocaBytes wid \p -> do

            poke (castPtr p) meta.hash
            pokeByteOff (castPtr p) 32 meta.size

            let mdbEdgePtr :: Ptr Word64 = castPtr p `plusPtr` 40

            let edgeListBytes = wid - 40

            VS.unsafeWith meta.edges \inputEdgeBuf ->
                copyBytes (castPtr mdbEdgePtr)
                          (castPtr inputEdgeBuf)
                          edgeListBytes

            act (MDB_val (fromIntegral wid) (castPtr p))

    loadMdbVal (MDB_val sz ptr) = do
        let badLen = throwIO . PIN_METADATA_LOAD_EXN (fromIntegral sz)

        when (sz < 40) do
            badLen "size too small for header"

        hax :: Hash256 <- peek (castPtr ptr)
        siz :: Word64  <- peekByteOff (castPtr ptr) 32

        let edgeListBytes = sz - 40
        let edgeListSize  = edgeListBytes `div` 8
        let edgeListPtr   = castPtr ptr `plusPtr` 40 :: Ptr Word64

        when (0 /= (edgeListBytes `mod` 8)) do
            badLen "not word array"

        vecBuf <- mallocPlainForeignPtrAlignedBytes
                      (fromIntegral edgeListBytes)
                      8

        withForeignPtr vecBuf \newVecPtr -> do
            copyBytes (castPtr newVecPtr)
                      (castPtr edgeListPtr)
                      (fromIntegral edgeListBytes)

        let edges :: VS.Vector Word64 =
                VS.unsafeFromForeignPtr0 vecBuf (fromIntegral edgeListSize)

        pure PIN_META{hash=hax,size=siz,edges}
