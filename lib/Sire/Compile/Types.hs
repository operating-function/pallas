{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.Compile.Types
    ( Inliner
    , Refr(..)
    , Global(..)
    , Expr
    , Func
    , showSymb
    )
where

import Loot.Backend
import PlunderPrelude
import Rex
import Sire.Types

import Loot.Syntax (symbRex)

import qualified Fan as F


-- Utils -----------------------------------------------------------------------

showSymb :: Symb -> Text
showSymb =
    let ?rexColors = NoColors
    in rexLine . symbRex


-- Types -----------------------------------------------------------------------

type Expr = Exp Refr Global
type Func = Fun Refr Global

type Inliner = Maybe Func

data Global = G
    { val     :: Fan
    , inliner :: Inliner
    }
  deriving (Generic, Eq, NFData)

instance Show Global where
  show (G (F.NAT n) _) = "(AT " <> show n <> ")"
  show (G pln _)       = "(G " <> show (F.valName pln) <> ")"

data Refr = REFR
    { name :: !Symb
    , key  :: !Int
    }
  deriving (Generic, NFData)

instance Eq  Refr where (==)    x y = (==)    x.key y.key
instance Ord Refr where compare x y = compare x.key y.key

instance Show Refr where
    show r = show (unpack (showSymb r.name) <> "_" <> show r.key)
    -- TODO Doesn't handle all cases correctly
