-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- | Maps and Sets as Sorted Vectors

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fprof-auto  #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Sorted.Search
    ( bsearch
    , bsearch#
    , bsearch_
    , bsearchIndex
    , bsearchPostIndex
    , bfind_
    , bfind
    , bfind#
    )
where

import Data.Primitive.Array
import Prelude

import GHC.Exts (Int(..), Int#, (>=#), (+#), uncheckedIShiftRL#)
import GHC.Exts (Array#, indexArray#, sizeofArray#)


-- Searching -------------------------------------------------------------------

bsearch_
    :: (a -> a -> Ordering)
    -> a
    -> Array# a
    -> Int#
    -> Int#
    -> (# Int#, Int# #) -- (Int, Bool)
bsearch_ cmp key row low end =
    case low >=# end of
      0# ->
          let i = (low +# end) `uncheckedIShiftRL#` 1# in
          case indexArray# row i of
            (# res #) ->
              case cmp key res of
                  LT -> bsearch_ cmp key row low i
                  EQ -> (# i, 1# #)
                  GT -> bsearch_ cmp key row (i +# 1#) end
      _  ->  (# low, 0# #)


{-# INLINE bsearch# #-}
bsearch# :: Ord a => a -> Array# a -> (# Int#, Int# #)
bsearch# key row = bsearch_ compare key row 0# (sizeofArray# row)

{-# INLINE bsearch #-}
bsearch :: Ord a => a -> Array a -> (Int, Bool)
bsearch key (Array row) =
    let
        !(# i#, found# #) = bsearch_ compare key row 0# (sizeofArray# row)
        i = I# i#
        bit = case found# of 0# -> False; _ -> True
    in
        (i, bit)

{-# INLINE bsearchIndex #-}
bsearchIndex :: Ord a => a -> Array a -> Int
bsearchIndex key (Array row) =
    let !(# i#, _ #) = bsearch_ compare key row 0# (sizeofArray# row)
    in I# i#

{-# INLINE bsearchPostIndex #-}
bsearchPostIndex :: Ord a => a -> Array a -> Int
bsearchPostIndex key (Array row) =
    let !(# i#, found# #) = bsearch_ compare key row 0# (sizeofArray# row)
    in I# (i# +# found#)

bfind_
    :: (a -> Bool)
    -> Array# a
    -> Int#
    -> Int#
    -> Int#
bfind_ f row low end =
    case low >=# end of
        1# -> low
        _  -> let i = (low +# end) `uncheckedIShiftRL#` 1# in
              let (# v #) = indexArray# row i in
              case f v of
                  True  -> bfind_ f row (i +# 1#) end
                  False -> bfind_ f row low   i

{-# INLINE bfind# #-}
bfind# :: (a -> Bool) -> Array# a -> Int#
bfind# f row = bfind_ f row 0# (sizeofArray# row)

-- Assuming that the predicate is monotone, find the first element where
-- the predicate is not true.
--
-- TODO: (This is not quite accurate, find a better way to say this).
{-# INLINE bfind #-}
bfind :: (a -> Bool) -> Array a -> Int
bfind f (Array row) = I# (bfind_ f row 0# (sizeofArray# row))
