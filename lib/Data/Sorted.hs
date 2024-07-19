-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- | Maps and Sets as Sorted Vectors

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Data.Sorted (module X, ArraySet, ArrayMap) where

import Data.Sorted.Row   as X hiding ((!))
import Data.Sorted.Set   as X
import Data.Sorted.Tab   as X
import Data.Sorted.Types as X (Tab)

import qualified Data.Sorted.Types

--------------------------------------------------------------------------------

type ArrayMap k v = Data.Sorted.Types.Tab k v

type ArraySet a = Data.Sorted.Types.Set a
