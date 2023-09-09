-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- | Maps and Sets as Sorted Vectors

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Sorted.Set
    ( ssetFromList
    , ssetToAscArray
    , ssetToAscList
    , ssetToDescList
    , ssetToArray
    , ssetSplitAt
    , ssetSpanAntitone
    , ssetIntersection
    , ssetDifference
    , ssetSingleton
    , ssetInsert
    , ssetLookupMin
    , ssetLookupMax
    , ssetDelete
    , ssetSize
    , ssetUnion
    , ssetIsEmpty
    , ssetFromDistinctAscList
    , ssetFindMax
    , ssetFindMin
    , ssetMember
    , ssetTake
    , ssetDrop
    )
where

import Control.Monad.ST
import Data.Bits
import Data.Containers
import Data.Foldable
import Data.MonoTraversable
import Data.Primitive.Array
import Data.Sorted.Row
import Data.Sorted.Search
import Data.Sorted.Types
import Prelude

import Data.Coerce  (coerce)
import Data.Functor (($>))
import GHC.Exts     (Array#, Int(..), Int#, sizeofArray#, (+#))

-- import qualified Fan.Prof as Prof


--------------------------------------------------------------------------------

{-# INLINE emptySet #-}
emptySet :: Set k
emptySet = SET mempty

{-# INLINE ssetSingleton #-}
ssetSingleton :: k -> Set k
ssetSingleton k = SET (rowSingleton k)

-- x must be less than y, otherwise the resulting set will be invalid.
{-# INLINE ssetUnsafeDuo #-}
ssetUnsafeDuo :: k -> k -> Set k
ssetUnsafeDuo x y = coerce (rowDuo x y)
  -- coerce here avoids the fmap.  SET is a newtype for row, so there
  -- is a type-safe cast between the two.

-- Collect list into an mutable array.  Sort it, remove duplicates.
--
-- TODO: Avoid the copies by loading into a mutable array, sorting the
-- mutable array, and then doing an unsafe freeze of that.
ssetFromList :: Ord k => [k] -> Set k
ssetFromList kList =
    -- Prof.withSimpleTracingEventPure "ssetFromList" "sorted" $
    SET $ rowSortUniqBy compare $ arrayFromList kList

{-# INLINE ssetInsert# #-}
ssetInsert# :: Ord a => a -> Array# a -> Int# -> Array# a
ssetInsert# k ks# wid# =
    let !(# i, found #) = bsearch# compare k ks# 0# wid# in
    case found of
        0# -> rowUnsafeInsert# i k ks# wid#
        _  -> ks#

{-# INLINE ssetInsert #-}
ssetInsert :: Ord k => k -> Set k -> Set k
ssetInsert k (SET (Array ks#)) =
    -- Prof.withSimpleTracingEventPure "setInsert" "sorted" $
    SET (Array (ssetInsert# k ks# (sizeofArray# ks#)))

{-# INLINE ssetDelete #-}
ssetDelete :: Ord k => k -> Set k -> Set k
ssetDelete k set@(SET ks@(Array ks#)) =
    -- Prof.withSimpleTracingEventPure "setDelete" "sorted" $
    let !(# i, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#) in
    case found of
        0# -> set
        _  -> SET (rowDelete (I# i) ks)

{-# INLINE ssetLookupMin #-}
ssetLookupMin :: Set a -> Maybe a
ssetLookupMin (SET ks) | null ks = Nothing
ssetLookupMin (SET ks)           = Just (ks!0)

{-# INLINE ssetLookupMax #-}
ssetLookupMax :: Set a -> Maybe a
ssetLookupMax (SET ks) =
    if wid==0 then Nothing else Just (ks ! (wid-1))
  where
    !wid = sizeofArray ks

{-# INLINE ssetToAscArray #-}
ssetToAscArray :: Set k -> Array k
ssetToAscArray (SET a) = a

{-# INLINE ssetToAscList #-}
ssetToAscList :: Set k -> [k]
ssetToAscList (SET a) = toList a

{-# INLINE ssetToArray #-}
ssetToArray :: Set k -> Array k
ssetToArray (SET a) = a

{-# INLINE ssetToDescList #-}
ssetToDescList :: Set k -> [k]
ssetToDescList (SET a) = go (length a - 1)
  where
    go i | i<0 = []
    go i       = (a!i) : go (i-1)

{-# INLINE ssetSize #-}
ssetSize :: Set k -> Int
ssetSize (SET a) = sizeofArray a

-- O(n+m) given input sets of size n and m.
--
-- We special-case sets of size zero and one.
ssetUnion :: Ord k => Set k -> Set k -> Set k
ssetUnion x@(SET xs) y@(SET ys) =
    case (sizeofArray xs, sizeofArray ys) of
        ( 0,  _  ) -> y
        ( _,  0  ) -> x
        ( 1,  1  ) -> let xv = (xs!0)
                          yv = (ys!0)
                      in case compare xv yv of
                          EQ -> x
                          LT -> ssetUnsafeDuo xv yv
                          GT -> ssetUnsafeDuo yv xv
        ( 1,  _  ) -> ssetInsert (xs!0) y
        ( _,  1  ) -> ssetInsert (ys!0) x
        ( xw, yw ) -> ssetUnionGeneric x xw y yw

-- This assumes that neither of the inputs are empty.
--
-- TODO: Skip the shrinking optimization if the sizes are small.
ssetUnionGeneric :: Ord a => Set a -> Int -> Set a -> Int -> Set a
ssetUnionGeneric (SET xs) !xWid (SET ys) !yWid =
    -- Prof.withSimpleTracingEventPure "setUnionGeneric" "sorted" $
    let
        xSmallest = xs ! 0
        xLargest  = xs ! (xWid-1)
        ySmallest = ys ! 0
        yLargest  = ys ! (yWid-1)
    in

    -- If there is no overlap, then the union is just array concatenation.
    if xSmallest > yLargest then SET (ys <> xs) else
    if ySmallest > xLargest then SET (xs <> ys) else

    -- Find the overlapping range of the sets so we can walk merely the
    -- parts we know overlap
    let
        (xMin, yMin, beforeCount, initialArray) =
            case compare xSmallest ySmallest of
                EQ -> (0, 0, 0, xs)
                GT -> let yMn = bsearchIndex xSmallest ys
                      in (0, yMn, yMn, ys)
                LT -> let xMn = bsearchIndex ySmallest xs
                      in (xMn, 0, xMn, xs)

        (xMax, yMax, afterStart, afterCount, finalArray) =
            case compare xLargest yLargest of
                EQ -> (xWid, yWid, 0, 0, xs)
                GT -> let xMx = bsearchPostIndex yLargest xs
                      in (xMx, yWid, xMx, xWid - xMx, xs)
                LT -> let yMx = bsearchPostIndex xLargest ys
                      in (xWid, yMx, yMx, yWid - yMx, ys)
        xOverlapWidth = xMax - xMin
        yOverlapWidth = yMax - yMin
    in
        coerce $
        if (yOverlapWidth > xOverlapWidth) then
            ssetUnionGenericSwapped
                ys yMin yMax yOverlapWidth
                xs xMin xMax xOverlapWidth
                initialArray beforeCount
                finalArray afterStart afterCount
        else
            ssetUnionGenericSwapped
                xs xMin xMax xOverlapWidth
                ys yMin yMax yOverlapWidth
                initialArray beforeCount
                finalArray afterStart afterCount

-- TODO: Too many arguments, find a way to rejigger this!
ssetUnionGenericSwapped
    :: Ord a
    => Array a -> Int -> Int -> Int
    -> Array a -> Int -> Int -> Int
    -> Array a -> Int
    -> Array a -> Int -> Int
    -> Array a
ssetUnionGenericSwapped
        xs xMin xMax xOverlapWidth
        ys yMin yMax yOverlapWidth
        initialArray beforeCount
        finalArray finalStart finalCount = runST do

    let maxOverlapWidth = xOverlapWidth + yOverlapWidth
    buf <- newArray maxOverlapWidth (error "setUnion: uninitialized")

    let go o iLow iEnd jLow jEnd = do
            let iRemain = iEnd - iLow
            let jRemain = jEnd - jLow
            case (iRemain, jRemain) of
                (0, 0) -> pure o
                (0, _) -> copyArray buf o ys jLow jRemain $> (o + jRemain)
                (_, 0) -> copyArray buf o xs iLow iRemain $> (o + iRemain)
                (1, 1) -> do
                    let x = xs!iLow
                    let y = ys!jLow
                    case compare x y of
                       LT -> writeArray buf o x >> writeArray buf (o+1) y >> pure (o+2)
                       EQ -> writeArray buf o x >> pure (o+1)
                       GT -> writeArray buf o y >> writeArray buf (o+1) x >> pure (o+2)
                (1, _) -> do
                    let x = xs!iLow
                    case bsearch_ x ys jLow jEnd of
                        (# _, 1# #) -> do
                            copyArray buf o ys jLow jRemain $> (o+jRemain)
                        (# jMid#, _ #) -> do
                            let jMid   = I# jMid#
                            let nBelo  = jMid - jLow
                            let nAbove = jEnd - jMid
                            copyArray buf o ys jLow nBelo
                            writeArray buf (o+nBelo) x
                            copyArray buf (o+nBelo+1) ys jMid nAbove
                            pure (o+nBelo+1+nAbove)
                (_, 1) -> do
                    let y = ys!jLow
                    case bsearch_ y xs iLow iEnd of
                        (# _, 1# #) -> do
                            copyArray buf o xs iLow iRemain $> (o+iRemain)
                        (# iMid#, _ #) -> do
                            let iMid   = I# iMid#
                            let nBelo  = iMid - iLow
                            let nAbove = iEnd - iMid
                            copyArray buf o xs iLow nBelo
                            writeArray buf (o+nBelo) y
                            copyArray buf (o+nBelo+1) xs iMid nAbove
                            pure (o+nBelo+1+nAbove)

                (_, _) -> do
                    -- Get the middle value for the left-set.
                    let iMid               = (iLow + iEnd) `shiftR` 1
                    let iMidVal            = xs ! iMid
                    let !(# jMid, found #) = bsearch_ iMidVal ys jLow jEnd

                    -- Recurse to the left of the split on both.
                    o2 <- go o iLow iMid jLow (I# jMid)

                    -- Always write out the pivot value.
                    writeArray buf o2 iMidVal
                    let o3 = o2+1

                    -- Skip over the pivot on recursion if it matched.
                    let iMid' = iMid + 1
                    let jMid' = I# (jMid +# found)

                    -- Recurse to the right of the split on both.
                    go o3 iMid' iEnd jMid' jEnd

    overlapCount <- go 0 xMin xMax yMin yMax
    overlap      <- unsafeFreezeArray buf

    if (overlapCount == maxOverlapWidth && finalCount+beforeCount == 0)
    then do
        pure overlap
    else do
        let totalCount = beforeCount + overlapCount + finalCount
        res <- newArray totalCount (error "setUnion: uninitialized")
        copyArray res 0                          initialArray 0          beforeCount
        copyArray res beforeCount                overlap      0          overlapCount
        copyArray res (beforeCount+overlapCount) finalArray   finalStart finalCount
        unsafeFreezeArray res

{-# INLINE ssetIsEmpty #-}
ssetIsEmpty :: Set k -> Bool
ssetIsEmpty (SET a) = null a

-- TODO: Should we check this invariant?
{-# INLINE ssetFromDistinctAscList #-}
ssetFromDistinctAscList :: [k] -> Set k
ssetFromDistinctAscList ksList = SET (arrayFromList ksList)

{-# INLINE ssetFindMax #-}
ssetFindMax :: Set k -> k
ssetFindMax (SET s) =
    case sizeofArray s of
        0 -> error "setFindMax: empty set"
        n -> s ! (n-1)

{-# INLINE ssetFindMin #-}
ssetFindMin :: Set k -> k
ssetFindMin (SET s) =
    if null s
    then error "setFindMin: empty setE"
    else s!0

-- Do a search, return True if if found something, otherwise False.
{-# INLINE ssetMember #-}
ssetMember :: Ord k => k -> Set k -> Bool
ssetMember k (SET (Array ks#)) =
    case bsearch# compare k ks# 0# (sizeofArray# ks#) of
        (# _, 0# #) -> False
        (# _, _  #) -> True

-- This doesn't affect the order invariants, so we just run the operation
-- directly against the underlying array.
{-# INLINE ssetTake #-}
ssetTake :: Int -> Set k -> Set k
ssetTake i (SET ks) =
    -- Prof.withSimpleTracingEventPure "setTake" "sorted" $
    SET (rowTake i ks)

-- This doesn't affect the order invariants, so we just run the operation
-- directly against the underlying array.
{-# INLINE ssetDrop #-}
ssetDrop :: Int -> Set k -> Set k
ssetDrop i (SET ks) =
    -- Prof.withSimpleTracingEventPure "setDrop" "sorted" $
    SET (rowDrop i ks)

-- Just split the underlying array, set invariants are not at risk.
{-# INLINE ssetSplitAt #-}
ssetSplitAt :: Int -> Set k -> (Set k, Set k)
ssetSplitAt i (SET ks) = (SET (rowTake i ks), SET (rowDrop i ks))

-- O(n) set intersection.  Special cases for (size=0 and size=1)
ssetIntersection :: Ord a => Set a -> Set a -> Set a
ssetIntersection x@(SET xs) y@(SET ys) =
    -- Prof.withSimpleTracingEventPure "setIntersection" "sorted" $
    case (sizeofArray xs, sizeofArray ys) of
        ( 0,    _    ) -> mempty
        ( _,    0    ) -> mempty
        ( 1,    1    ) -> if xs!0 == ys!0 then x else mempty
        ( 1,    _    ) -> if ssetMember (xs!0) y then x else mempty
        ( _,    1    ) -> if ssetMember (ys!0) x then y else mempty
        ( xWid, yWid ) ->
            let
                xSmallest = xs ! 0
                yLargest  = ys ! (yWid-1)
                xLargest  = xs ! (xWid-1)
                ySmallest = ys ! 0
            in

            -- If no overlap is possible, return empty.
            if xSmallest > yLargest then mempty else
            if ySmallest > xLargest then mempty else

            -- figure out which subregions overlap.
            let
                (xMin, yMin) =
                    case compare xSmallest ySmallest of
                        EQ -> (0, 0)
                        GT -> (0, bsearchIndex xSmallest ys)
                        LT -> (bsearchIndex ySmallest xs, 0)

                (xMax, yMax) =
                    case compare xLargest yLargest of
                        EQ -> (xWid, yWid)
                        GT -> (bsearchPostIndex yLargest xs, yWid)
                        LT -> (xWid, bsearchPostIndex xLargest ys)

                xSz = (xMax - xMin)
                ySz = (yMax - yMin)
            in
                -- Run the intersection algorithm against the overlapping
                -- region, with the smaller region as the left-hand-side.
                coerce $
                if xSz > ySz
                then ssetIntersectionGeneric ys yMin yMax ySz xs xMin xMax xSz
                else ssetIntersectionGeneric xs xMin xMax xSz ys yMin yMax ySz

{-
    Performs intersection by divide-and-conquor on two non-empty slices
    for two sorted arrays.  Returns a sorted, unique array containing
    the intersection.
-}
ssetIntersectionGeneric
    :: Ord a
    => Array a -> Int -> Int -> Int
    -> Array a -> Int -> Int -> Int
    -> Array a
ssetIntersectionGeneric xs !xMin !xMax !xSz ys !yMin !yMax !ySz = runST do
    let rWid = min ySz xSz

    buf <- newArray rWid (error "setIntersection: uninitialized")

    let go o iLow iEnd jLow jEnd = do
              case (iEnd - iLow, jEnd - jLow) of
                  (0, _) -> pure o
                  (_, 0) -> pure o
                  (1, 1) ->
                      let xVal = xs!iLow; yVal = ys!jLow in
                      if xVal ==yVal
                      then writeArray buf o xVal >> pure (o+1)
                      else pure o

                  (1, _) ->
                      let xVal = xs!iLow in
                      case bsearch_ xVal ys jLow jEnd of
                          (# _, 0# #) -> pure o
                          (# _, _  #) -> writeArray buf o xVal >> pure (o+1)

                  (_, 1) ->
                      let yVal = ys!jLow in
                      case bsearch_ yVal xs iLow iEnd of
                          (# _, 0# #) -> pure o
                          (# _, _  #) -> writeArray buf o yVal >> pure (o+1)

                  (_, _) ->

                      -- Get the middle value for the left-set.
                      let iMid               = (iLow + iEnd) `shiftR` 1
                          iMidVal            = xs ! iMid
                          !(# jMid, found #) = bsearch_ iMidVal ys jLow jEnd
                      in do
                              -- Recurse to the left of the split on both.
                              o2 <- go o iLow iMid jLow (I# jMid)

                              -- Write out the pivot value, if it exists
                              -- in both arrays.
                              o3 <- case found of
                                        0# -> pure o2
                                        _  -> do writeArray buf o2 iMidVal
                                                 pure (o2+1)

                              -- Skip over the pivot if it matched.
                              let iMid' = iMid + 1
                              let jMid' = I# (jMid +# found)

                              -- Recurse to the right of the split on both.
                              go o3  iMid' iEnd jMid' jEnd

    used <- go 0 xMin xMax yMin yMax

    if used == rWid
    then unsafeFreezeArray buf
    else freezeArray buf 0 used

-- O(n) set difference.
ssetDifference :: Ord a => Set a -> Set a -> Set a
ssetDifference (SET xs) (SET ys) = runST do
    let xWid = sizeofArray xs
    let yWid = sizeofArray ys
    buf <- newArray xWid (error "setDifference: uninitialized")
    let go o i j =
            if i >= xWid then pure o else
            if j >= yWid then do
                let extra = xWid - i
                copyArray buf o xs i extra
                pure (o + extra)
            else do
                let x = xs ! i
                let y = ys ! j
                case compare x y of
                    LT -> writeArray buf o x >> go (o+1) (i+1) j
                    EQ -> go o (i+1) (j+1)
                    GT -> go o i (j+1)
    used <- go 0 0 0
    if used == xWid
    then SET <$> unsafeFreezeArray buf
    else SET <$> freezeArray buf 0 used

-- Assuming that the predicate is monotone, find the point where the
-- predicate stops holding, and split the set there.
{-# INLINE ssetSpanAntitone #-}
ssetSpanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
ssetSpanAntitone f (SET ks) =
    let numTrue = bfind f ks in
    ( SET $ rowTake numTrue ks
    , SET $ rowDrop numTrue ks
    )

--------------------------------------------------------------------------------
-- TODO: Optimize and verify these instances

instance MonoFoldable (Set a) where

type instance Element (Set a) = a

-- Assert that append (<>) never produces something smaller.
instance GrowingAppend (Set k) where

instance Ord k => SetContainer (Set k) where
    type ContainerKey (Set k) = k
    member = ssetMember
    notMember k s = not (ssetMember k s)
    union = ssetUnion
    difference = ssetDifference
    intersection = ssetIntersection
    keys = ssetToAscList
    {-# INLINE member #-}
    {-# INLINE notMember #-}
    {-# INLINE union #-}
    {-# INLINE difference #-}
    {-# INLINE intersection #-}
    {-# INLINE keys #-}

instance Ord a => Semigroup (Set a) where
    (<>) = ssetUnion
    {-# INLINE (<>) #-}

instance Ord a => Monoid (Set a) where
    mempty = emptySet
    {-# INLINE mempty #-}

instance Ord k => IsSet (Set k) where
    insertSet = ssetInsert
    deleteSet = ssetDelete
    singletonSet = ssetSingleton
    setFromList = ssetFromList
    setToList = ssetToAscList
    {-# INLINE insertSet #-}
    {-# INLINE deleteSet #-}
    {-# INLINE singletonSet #-}
    {-# INLINE setFromList #-}
    {-# INLINE setToList #-}
