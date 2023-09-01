-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- | Maps and Sets as Sorted Vectors

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    , rowGenerate
    , bsearch
    , bsearch_
    , bfind_
    , bfind
    )
where

import Control.Monad.ST
import Data.Containers
import Data.Foldable
import Data.MonoTraversable
import Data.Primitive.Array
import Data.Sorted.Row
import Data.Sorted.Types
import Prelude

import Data.Bits (shiftR)


-- Searching -------------------------------------------------------------------

-- TODO: Manually unbox into (# Int# , Int# #) instead.
-- TODO: Is `shiftR` actually better than `div`?  Check the compiler output.
bsearch_
    :: (a -> a -> Ordering)
    -> a
    -> Array a
    -> Int
    -> Int
    -> (Int, Bool)
bsearch_ cmp key row low end =
    if low >= end then (low, False) else
    let i = (low + end) `shiftR` 1 in
    case cmp key (row ! i) of
        LT -> bsearch_ cmp key row low i
        EQ -> (i, True)
        GT -> bsearch_ cmp key row (i+1) end

-- TODO: Manually unbox into (# Int# , Int# #) instead.
{-# INLINE bsearch #-}
bsearch :: Ord a => a -> Array a -> (Int, Bool)
bsearch key row = bsearch_ compare key row 0 (sizeofArray row)

-- TODO: Manually unbox into (# Int# , Int# #) instead.
-- TODO: Is `shiftR` actually better than `div`?  Check the compiler output.
bfind_
    :: (a -> Bool)
    -> Array a
    -> Int
    -> Int
    -> Int
bfind_ f row low end =
    if low >= end then low else
    let i = (low + end) `shiftR` 1 in
    case f (row ! i) of
        True  -> bfind_ f row (i+1) end
        False -> bfind_ f row low   i

-- Assuming that the predicate is monotone, find the first element where
-- the predicate is not true.
--
-- TODO: (This is not quite accurate, find a better way to say this).
{-# INLINE bfind #-}
bfind :: (a -> Bool) -> Array a -> Int
bfind f row = bfind_ f row 0 (sizeofArray row)


--------------------------------------------------------------------------------

{-# INLINE emptySet #-}
emptySet :: Set k
emptySet = SET mempty

{-# INLINE ssetSingleton #-}
ssetSingleton :: k -> Set k
ssetSingleton k = SET (rowSingleton k)

-- x must be less than y, otherwise the resulting set will be invalid.
ssetUnsafeDuo :: k -> k -> Set k
ssetUnsafeDuo x y = runST do
    res <- newArray 2 x
    writeArray res 1 y
    SET <$> unsafeFreezeArray res

-- Collect list into an mutable array.  Sort it, remove duplicates.
--
-- TODO: Avoid the copies by loading into a mutable array, sorting the
-- mutable array, and then doing an unsafe freeze of that.
ssetFromList :: Ord k => [k] -> Set k
ssetFromList kList =
    SET $ rowSortUniqBy compare $ arrayFromList kList

ssetInsert :: Ord k => k -> Set k -> Set k
ssetInsert k set@(SET ks) =
    let (i, found) = bsearch k ks in
    if found then set else SET (rowInsert i k ks)

ssetDelete :: Ord k => k -> Set k -> Set k
ssetDelete k set@(SET ks) =
    let (i, found) = bsearch k ks in
    if found then SET (rowDelete i ks) else set

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
--
-- TODO: Make sure that GHC optimizes away this pattern match.
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

ssetUnionGeneric :: Ord k => Set k -> Int -> Set k -> Int -> Set k
ssetUnionGeneric (SET xs) !xWid (SET ys) !yWid = runST do
    let !rWid = xWid + yWid

    buf <- newArray rWid (error "ssetUnion: uninitialized")

    let go o i j = do
            let xRemain = xWid - i
            let yRemain = yWid - j
            case (xRemain, yRemain) of
                (0, 0) -> pure o
                (0, _) -> do
                    copyArray buf o ys j yRemain
                    pure (o + yRemain)
                (_, 0) -> do
                    copyArray buf o xs i xRemain
                    pure (o + xRemain)
                (_, _) -> do
                    let x = xs ! i
                    let y = ys ! j
                    case compare x y of
                        EQ -> writeArray buf o x >> go (o+1) (i+1) (j+1)
                        LT -> writeArray buf o x >> go (o+1) (i+1) j
                        GT -> writeArray buf o y >> go (o+1) i     (j+1)

    written <- go 0 0 0

    SET <$> if written == rWid
            then unsafeFreezeArray buf
            else freezeArray buf 0 written

{-# INLINE ssetIsEmpty #-}
ssetIsEmpty :: Set k -> Bool
ssetIsEmpty (SET a) = null a

-- TODO: Should we check this invariant?
ssetFromDistinctAscList :: [k] -> Set k
ssetFromDistinctAscList ksList = SET (arrayFromList ksList)

ssetFindMax :: Set k -> k
ssetFindMax (SET s) =
    case sizeofArray s of
        0 -> error "setFindMax: empty set"
        n -> s ! (n-1)

ssetFindMin :: Set k -> k
ssetFindMin (SET s) =
    if null s
    then error "setFindMin: empty setE"
    else s!0

-- Do a search, return True if if found something, otherwise False.
ssetMember :: Ord k => k -> Set k -> Bool
ssetMember k (SET ks) = snd (bsearch k ks)

-- This doesn't affect the order invariants, so we just run the operation
-- directly against the underlying array.
ssetTake :: Int -> Set k -> Set k
ssetTake i (SET ks) = SET (rowTake i ks)

-- This doesn't affect the order invariants, so we just run the operation
-- directly against the underlying array.
ssetDrop :: Int -> Set k -> Set k
ssetDrop i (SET ks) = SET (rowDrop i ks)

-- Just split the underlying array, set invariants are not at risk.
ssetSplitAt :: Int -> Set k -> (Set k, Set k)
ssetSplitAt i (SET ks) = (SET (rowTake i ks), SET (rowDrop i ks))

-- O(n) set intersection.
ssetIntersection :: Ord a => Set a -> Set a -> Set a
ssetIntersection (SET xs) _ | null xs = mempty
ssetIntersection _ (SET ys) | null ys = mempty
ssetIntersection (SET xs) (SET ys) = runST do
    let xWid = sizeofArray xs
    let yWid = sizeofArray ys
    let rWid = min xWid yWid
    buf <- newArray rWid (error "setIntersection: uninitialized")
    let go o i j = do
            if i >= xWid || j >= yWid then pure o else do
                let x = xs ! i
                let y = ys ! j
                case compare x y of
                    EQ -> writeArray buf o x >> go (o+1) (i+1) (j+1)
                    LT -> go o (i+1) j
                    GT -> go o i (j+1)
    used <- go 0 0 0
    if used == rWid
    then SET <$> unsafeFreezeArray buf
    else SET <$> freezeArray buf 0 used

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
ssetSpanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
ssetSpanAntitone f (SET ks) =
    ( SET $ rowTake numTrue ks
    , SET $ rowDrop numTrue ks
    )
  where
    numTrue = bfind f ks

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
