-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.Types
    ( Fan(..)
    , Any
    , PrimopCrash(..)
    , Nat
    , Pin(..)
    , Law(..)
    , LawName(..)
    , Rex
    , setExec
    , toNat
    , Prog(..)
    , Run(..)
    , Hash256
    , JetEdgeCase(..)
    , RtsConfig(..)
    )
where

import PlunderPrelude hiding (hash, (^))
import Data.Sorted

import Hash256     (Hash256)
import Rex         (GRex)

import qualified Data.Vector.Storable as SV

-- Types -----------------------------------------------------------------------

data JetEdgeCase = IGNORE | WARN | CRASH
  deriving (Eq, Ord, Read, Show)

data RtsConfig = RTS_CONFIG
    { onJetFallback :: !JetEdgeCase
    , onJetMismatch :: !JetEdgeCase
    }

data PrimopCrash = PRIMOP_CRASH { errCode :: !Nat, errVal :: !Fan }

{-
    Note that `code` is lazy.  We don't "compile" it until it is
    first run.
-}
data Law = L
    { name :: !LawName
    , args :: !Nat
    , body :: !Fan
    , code :: Prog
    }

-- All data already forced upon construction.
instance NFData Law where rnf = \L{} -> ()

newtype LawName = LN { nat :: Nat }
  deriving newtype (Eq, Ord, NFData)

{-
    -   `hash` is the BLAKE3 hash of the concatenation of the jelly head
        and jelly body.

    -   `refs` is all the pins that we reference, in noun-traversal order.
        This corresponds directly to the pin length and order used in Jelly
        save.

     TODO Use on-heap data for hash and blob (ShortByteString, or similar).

     TODO Evaluate the performance impact of making `.hash` strict.
-}
data Pin = P
    { refs :: Vector Pin -- Edge-list
    , hash :: Hash256    -- Cryptographic Hash
    , args :: !Nat
    , item :: !Fan
    , exec :: SmallArray Fan -> Fan
    }

-- All data already forced upon construction.
instance NFData Pin where rnf = \P{} -> ()

setExec :: (SmallArray Fan -> Fan) -> Pin -> Pin
setExec x (P n h a i _) = P n h a i x

data Fan
    = NAT !Nat
    | PIN !Pin
    | FUN !Law
    | KLO !Int {-# UNPACK #-} !(SmallArray Fan)
    | BAR {-# UNPACK #-} !ByteString
    | ROW {-# UNPACK #-} !(Array Fan)
    | TAb {-# UNPACK #-} !(ArrayMap Fan Fan)
    | SET {-# UNPACK #-} !(ArraySet Fan)
    | COw !Nat
  deriving (Generic, NFData)

type Any = Fan

instance Num Fan where
    fromInteger n = NAT (fromIntegral n)
    x+y           = NAT (toNat x + toNat y)
    x*y           = NAT (toNat x * toNat y)
    abs x         = x
    negate _      = NAT 0
    signum x      = case toNat x of { 0 -> NAT 0; _ -> NAT 1 }

toNat :: Fan -> Nat
toNat (NAT n) = n
toNat _       = 0

instance IsString Fan where
   fromString = NAT . fromString

type Rex = GRex Fan


-- Internal DSL used to execute Laws -------------------------------------------

data Prog = PROG
    { arity  :: !Int
    , varsSz :: !Int
    , prgrm  :: !Run
    }

{-
   GHC Will use pointer-tagging for the first 6 constructors.
-}
data Run
    = CNS !Fan
    | ARG {-# UNPACK #-} !Int
    | VAR {-# UNPACK #-} !Int
    | LET {-# UNPACK #-} !Int !Run !Run
    | IFZ !Run !Run !Run
    | IF_ !Run !Run !Run

    | EXE                !(SmallArray Fan -> Fan)  --  Precalculated Executor
          {-# UNPACK #-} !Int                      --  Precalculated Frame Size
                         !Fan                      --  Function (or closure)
          {-# UNPACK #-} !(SmallArray Run)         --  Arguments

    | SWI      !Run !Run !(SmallArray Run)
    | JMP      !Run !Run !(Tab Fan Run)
    | JMP_WORD !Run !Run !(SV.Vector Word) !(SmallArray Run)

    | LETREC !(SmallArray (Int, Run)) !Run

    | SEQ !Run !Run

    | REC {-# UNPACK #-} !(SmallArray Run)      -- Saturated self-application

    | KAL {-# UNPACK #-} !(SmallArray Run)      -- Dynamic Application

    | PAR {-# UNPACK #-} !Int              -- Closure arity (avoid recalculate)
          {-# UNPACK #-} !(SmallArray Run) -- Unsaturated Application

    | TRK !Run !Run

    | MK_ROW !(Vector Run)

    | MK_TAB !(Tab Fan Run)

    -- Inlined operations with fallback from the pin.
    | OP2 !String (Fan -> Fan -> Fan) !Run !Run

   -- Broken Spine (thunked sub-spine)
    | LAZ !Prog !(SmallArray Run)
