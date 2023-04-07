{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Types
    ( RuneShape(..)
    , TextShape(..)
    , GRex(..)
    , Leaf
    , Rex
    , rexIdent
    )
where

import PlunderPrelude

-- TODO: Rename to NodeShape
-- TODO: Rename branches to mirror Fan repr
data RuneShape
    = OPEN
    | NEST_PREFIX
    | NEST_INFIX
    | SHUT_PREFIX
    | SHUT_INFIX
  deriving (Eq, Ord, Show, Generic, NFData)
  deriving anyclass (Hashable)

-- TODO Rename to LeafShape
-- TODO: Rename branches to mirror Fan repr
data TextShape
    = BARE_WORD  --  foo
    | THIC_CORD  --  "foo"
    | THIN_CORD  --  'foo'
    | CURL_CORD  --  {foo}
    | THIC_LINE  --  """foo
    | THIN_LINE  --  '''foo
  deriving (Eq, Ord, Show, Generic, NFData, Enum)

type Leaf = (TextShape, Text)

data GRex v
    = N !Nat RuneShape Text [GRex v] (Maybe (GRex v))
    | T !Nat TextShape Text (Maybe (GRex v))
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

--------------------------------------------------------------------------------

rexIdent :: GRex a -> Nat
rexIdent (N k _ _ _ _) = k
rexIdent (T k _ _ _)   = k
rexIdent (C _)         = 0
