-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.Util
    ( mkCow
    , a2, a3, a4
    , kloWalk, kloList, kloArgs
    , mkRow, tabValsRow, fromBit
    , natToArity, trueArity, natArity, evalArity
    )
where


import PlunderPrelude hiding (hash, (^))
import Fan.Types
import Data.Sorted

--------------------------------------------------------------------------------

{-# INLINE mkCow #-}
mkCow :: Nat -> Fan
mkCow 0 = ROW mempty
mkCow n = COw n

--------------------------------------------------------------------------------

a2 :: a -> a -> SmallArray a
a2 p q = createSmallArray 2 p \a -> do
    writeSmallArray a 1 q

a3 :: a -> a -> a -> SmallArray a
a3 p q r = createSmallArray 3 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r

a4 :: a -> a -> a -> a -> SmallArray a
a4 p q r s = createSmallArray 4 p \a -> do
    writeSmallArray a 1 q
    writeSmallArray a 2 r
    writeSmallArray a 3 s

--------------------------------------------------------------------------------

kloWalk :: Fan -> [Fan]
kloWalk (KLO _ xs) = go (sizeofSmallArray xs - 1)
                       where
                         go !0 = kloWalk (xs.!0)
                         go !i = (xs.!i) : go (i-1)
kloWalk v          = [v]

kloList :: Fan -> [Fan]
kloList = reverse . kloWalk

{-
    `kloArgs` returns a list of the arguments of a closure in traversal order.

    For example:

        kloArgs (0 1 2) -> [2,1]
        kloArgs 0       -> []
        kloArgs [3 4]   -> [3,4]
-}
kloArgs :: Fan -> [Fan]
kloArgs = exceptHead . kloWalk
  where
    exceptHead []     = error "impossible"
    exceptHead [_]    = []
    exceptHead (x:xs) = x : exceptHead xs

--------------------------------------------------------------------------------

mkRow :: [Fan] -> Fan
mkRow = ROW . arrayFromList

tabValsRow :: Tab Fan Fan -> Fan
tabValsRow tab = ROW (tabElemsArray tab)

fromBit :: Bool -> Fan
fromBit True  = NAT 1
fromBit False = NAT 0

--------------------------------------------------------------------------------

natToArity :: Nat -> Int
natToArity !n =
    if n>fromIntegral(maxBound::Int)
    then maxBound
    else fromIntegral n

-- Vector and Table constructors are given an evalArity of one less than
-- there actual arity so that they will be transformed into vectors/tables
-- once saturated.
--
-- Thus, to get the true arity, we need to actually do the scan and see
-- what's at the head.
trueArity :: Fan -> Nat
trueArity = \case
    COw 0          -> error "Should be jet matched as V0"
    COw n          -> succ $ fromIntegral n
    SET{}          -> 2
    KLO _ xs       -> trueArity (xs.!0) - fromIntegral (sizeofSmallArray xs - 1)
    FUN l          -> l.args
    NAT n          -> natArity n
    PIN p          -> p.args
    ROW _          -> 1
    TAb _          -> 1
    BAR _          -> 1

{-# INLINE natArity #-}
natArity :: Nat -> Nat
natArity (NatS# 0##) = 3 -- FUN
natArity (NatS# 1##) = 5 -- CAS
natArity (NatS# 2##) = 3 -- DEC
natArity _           = 1 -- INC, PIN, etc

evalArity :: Fan -> Int
evalArity (FUN l) = natToArity l.args
evalArity (NAT n) = case n of NatS# 0## -> 3
                              NatS# 1## -> 5
                              NatS# 2## -> 3
                              _         -> 1
evalArity (KLO r _) = r
evalArity (PIN p)   = natToArity p.args
evalArity (SET _)   = 1
evalArity (COw 0)   = error "Should be jet matched as V0"
evalArity (COw n)   = natToArity n
evalArity (ROW _)   = 1
evalArity (TAb _)   = 1
evalArity (BAR _)   = 1
