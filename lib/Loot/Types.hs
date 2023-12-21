-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-|
    Types for concrete syntax tress (`XCmd`, `XExp`, etc) and abstract
    syntax trees (`Cmd`, `Exp`, etc).

    @"Loot.Syntax"@ parses `Rex` into concrete syntax trees, and
    @"Loot.Sugar"@ desugars those into abstract syntax trees.

    In particular, desugaring `XBod` to `Bod` resolves bindings
    (`XVAR`) to either a local argument index (`BVAR`) or a free variable
    (`BCNS`).
-}
module Loot.Types
    ( Symb
    , XTag(..)
    , xtagTag
    , Tag(..)
    , Cmd(..)
    , Bind(..)
    , Rul(..)
    , Bod(..)
    , Val(..)
    , XCmd(..)
    , XBind(..)
    , XBindHead(..)
    , XLaw(..)
    , XBod(..)
    , XVal(..)
    , LawName(..)
    , LootRex(..)
    , Word256
    )
where

import PlunderPrelude

import Fan (LawName(..))
import Rex (RuneShape, TextShape)

----------------------------------------
-- Identifier with Explicit Tag Value --
----------------------------------------

-- |The type of identifiers in `Loot` and `Sire`.
type Symb = Nat

-- |The binder for a function, which has both an identifier and an
-- actual value-level tag.  These two things are often the same, but
-- aren't always.
data Tag = TAG
    { tagIdn :: !Symb
    , tagNam :: !LawName
    }
  deriving (Eq, Ord, Show, Generic, NFData)

------------
-- Values --
------------

-- |A Loot value.  This has a one-to-one mapping to Plunder values.
data Val a
    = REF a
    | NAT Nat
    | APP (Val a) (Val a)
    | LAW LawName Nat (Bod a)
    | ROW (Vector (Val a))
    | ROX (LootRex (Val a))
    | COW Nat
    | TAB [(Val a, Val a)]
    | SET [Val a]
    | BAR ByteString
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data LootRex a
    = NODE RuneShape Text [LootRex a] (Maybe (LootRex a))
    | LEAF TextShape Text (Maybe (LootRex a))
    | EMBD a
    | EVIL a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

instance Num (Val a) where
    fromInteger = NAT . fromInteger
    (+)    = error "HACK"
    (*)    = error "HACK"
    abs    = error "HACK"
    signum = error "HACK"
    negate = error "HACK"

-- |A concrete syntax tree for `Val`s.  (TODO: This used to just be
-- `(Val Symb)`, can it return to being that?)
data XVal
    = XVREF Symb
    | XVNAT Nat
    | XVAPP XVal XVal
    | XVLAW XLaw
    | XVROW (Vector XVal)
    | XVCOW Nat
    | XVTAB [(XVal, XVal)]
    | XVSET [XVal]
    | XVBAR ByteString
    | XVROX (LootRex XVal)
  deriving (Eq, Ord, Show, Generic, NFData)

-----------
-- Rules --
-----------

data Rul a = RUL
    { rulName :: LawName
    , rulArgs :: Nat
    , rulBody :: Bod a
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data XLaw
    = XLAW
        { xlName :: XTag
        , xlArgs :: NonEmpty Symb
        , xlBody :: XBod
        }
    | XLAM
        { xlArgs :: NonEmpty Symb
        , xlBody :: XBod
        }
  deriving (Eq, Ord, Show, Generic, NFData)

data XBod
    = XVAR Symb
    | XCNS XVal
    | XBAD XVal
    | XRAW XVal
    | XAPP XBod XBod
    | XLET Symb XBod XBod
  deriving (Eq, Ord, Show, Generic, NFData)

---------------
-- Functions --
---------------

{-
    - `a`      -> XTAG "a" NONE
    - `$0`     -> XTAG 0   NONE
    - `$" "`   -> XTAG " " NONE
    - `a$32`   -> XTAG "a" (SOME 32)
    - `0$32`   -> XTAG 0   (SOME 32)
    - `"a"$b`  -> XTAG "a" (SOME "b")
    - `_`      -> XTAG "_" NONE
    - `_$32`   -> XTAG "_" (SOME 32)
-}
data XTag = XTAG
    { xtagIdn :: !Symb
    , xtagOpt :: !(Maybe Symb)
    }
  deriving (Eq, Ord, Show, Generic, NFData)

xtagTag :: XTag -> Symb
xtagTag (XTAG _ (Just s)) = s
xtagTag (XTAG s Nothing)  = s

type Word256 = ByteString -- Always 256 bytes

data Bod a
    = BVAR Nat
    | BCNS (Val a)
    | BAPP (Bod a) (Bod a)
    | BLET (Bod a) (Bod a)
    | BBAD (Val a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-- All methods default to Foldable methods, which is what we want.
instance MonoFoldable (Bod a) where
type instance Element (Bod a) = a

-------------------
-- REPL Commands --
-------------------

data Cmd z v a
    = OUTPUT (Val a)
    | DUMPIT (Val a)
    | ASSERT [(XVal, Val a)]
    | DEFINE [Bind a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data Bind a
    = BIND_VL a (Val a)
    | BIND_PN a (Val a)
    | BIND_PL a (Rul a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

data XCmd z
    = XOUTPUT XVal
    | XDUMPIT XVal
    | XASSERT [XVal]
    | XDEFINE [XBind]

data XBindHead
    = XHEAD_VL Symb
    | XHEAD_PN Symb
    | XHEAD_PL XTag (NonEmpty Symb)

data XBind
  = XBIND_VL Symb XVal
  | XBIND_PN Symb XVal
  | XBIND_PL XLaw
