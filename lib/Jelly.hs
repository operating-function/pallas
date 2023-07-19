-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NoFieldSelectors #-}

module Jelly
    ( loadBodySlow
    , loadDepsSlow
    , mkBlob
    , splitBlob
    , saveFast
    , saveSlow
    , withContext
    , Ctx
    , Node(..)
    , encodeBtc
    , decodeBtc
    )
where

import Jelly.Fast.Save
import PlunderPrelude  hiding ((%))

import Jelly.Fast.FFI  (Ctx)
import Jelly.Reference (IsJelly(..), Node(..), loadBody, loadDeps, mkBlob,
                        splitBlob)
import Jelly.Types     (decodeBtc, encodeBtc, Hash256)

import qualified Fan.Prof        as Prof
import qualified Jelly.Fast.FFI  as FFI
import qualified Jelly.Reference


--------------------------------------------------------------------------------

{-# INLINE loadDepsSlow #-}
loadDepsSlow :: ByteString -> Either Text (Vector Hash256)
loadDepsSlow !blob =
    unsafePerformIO do
        Prof.withSimpleTracingEvent "load_deps" "pages" do
            pure (Jelly.Reference.loadDeps blob)

{-# INLINE loadBodySlow #-}
loadBodySlow :: ∀a. (Show a, IsJelly a) => Vector a -> ByteString -> Either Text a
loadBodySlow !pinz !bodBytes =
    unsafePerformIO do
        Prof.withSimpleTracingEvent "load_body" "pages" do
            pure (Jelly.Reference.loadBody pinz bodBytes)

{-# INLINE saveFast #-}
saveFast :: ∀a. FFI.Ctx -> Node a -> IO (Vector a, ByteString, ByteString)
saveFast !ctx !top =
    Prof.withSimpleTracingEvent "save" "pages" do
        Jelly.Fast.Save.save ctx top

{-# INLINE saveSlow #-}
saveSlow :: ∀a. Show a => Node a -> IO (Vector a, ByteString, ByteString)
saveSlow !top =
    Prof.withSimpleTracingEvent "save_slow" "pages" do
        let res@(!_, !_, !_) = Jelly.Reference.save top
        pure res
