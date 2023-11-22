-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Types
    ( RuneShape(..)
    , TextShape(..)
    , GRex(..)
    , Leaf
    , Rex
    )
where

import PlunderPrelude

-- TODO: Rename to NodeShape
-- TODO: Rename branches to mirror Fan repr
data RuneShape = OPEN | NEST | INFX | PREF | SHUT
  deriving (Eq, Ord, Show, Generic, NFData)
  deriving anyclass (Hashable)

-- TODO Rename to LeafShape
-- TODO: Rename branches to mirror Fan repr
data TextShape
    = WORD  --  foo
    | TEXT  --  {foo} or }_}_
    | LINE  --  } foo
  deriving (Eq, Ord, Show, Generic, NFData, Enum)

type Leaf = (TextShape, Text)

data GRex v
    = N RuneShape Text [GRex v] (Maybe (GRex v))
    | T TextShape Text (Maybe (GRex v))
    | C v
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

type Rex = GRex Void

--------------------------------------------------------------------------------

instance Hashable TextShape where

instance (Hashable v, Eq v) => Hashable (GRex v) where

--------------------------------------------------------------------------------

-- All methods default to Foldable methods, which is what we want.
instance MonoFoldable (GRex a) where
type instance Element (GRex a) = a
