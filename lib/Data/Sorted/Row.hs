-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- TODO: Try skipping ST and do these things directly in terms of runtime
-- primitives.  Or at least validate that the optimizer does that for us.

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Sorted.Row
    ( (!)
    , arrayFromListRevN
    , arrayFromListRev
    , rowGenerate
    , rowPut
    , rowUnsafePut
    , rowZipWith
    , rowSlice
    , rowSingleton
    , rowDuo
    , rowSortUniqBy
    , rowUnsafeDelete
    , rowDelete
    , rowInsert
    , rowUnsafeInsert#
    , rowTake
    , rowDrop
    , rowReverse
    , rowCons
    , rowSnoc
    , rowFilter
    )
where

import Control.Monad.ST
import Control.Monad.Zip
import Data.Sorted.Types
import PlunderPrelude    hiding ((!))

import Data.Vector.Mutable (MVector(..))
import GHC.Exts            (Int(..), Int#, (==#), (+#), (-#))
import GHC.Exts            (Array#, sizeofArray#, unsafeFreezeArray#, copyArray#)
import GHC.Exts            (newArray#)
import GHC.Exts            (runRW#)

import qualified Data.Vector.Algorithms.Merge as VA

--------------------------------------------------------------------------------

-- No bounds checking (CRASH!  Not exception)
(!) :: Array a -> Int -> a
(!) = indexArray

--------------------------------------------------------------------------------

-- | Create an array from a list of a known length. If the length
-- of the list does not match the given length, this throws an exception.
arrayFromListRevN :: Int -> [a] -> Array a
arrayFromListRevN 0 _ = mempty
arrayFromListRevN n l =
  createArray n (error "fromListN: uninitialized element") \buf ->
      let
          go (-1) []      = pure ()
          go _    []      = error "fromListRevN: list is smaller than specified"
          go (-1) (_:_)   = error "fromListRevN: list is longer than specified"
          go ix   (x:xs)  = writeArray buf ix x >> go (ix-1) xs
      in
          go (n-1) l

{-# INLINE arrayFromListRev #-}
arrayFromListRev :: [a] -> Array a
arrayFromListRev xs = arrayFromListRevN (length xs) xs

{-# INLINE rowGenerate #-}
rowGenerate :: Int -> (Int -> a) -> Row a
rowGenerate sz f = runArray do
    res <- newArray sz (error "rowGenerate: uninitialized")
    let go i | i>= sz = pure res
        go i          = writeArray res i (f i) >> go (i+1)
    go 0

{-# INLINE rowSingleton #-}
rowSingleton :: a -> Row a
rowSingleton = pure

rowDuo :: a -> a -> Row a
rowDuo x y = runArray do
    res <- newArray 2 x
    writeArray res 1 y
    pure res

{-# INLINE rowZipWith #-}
rowZipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
rowZipWith = mzipWith

{-# INLINE rowCons# #-}
rowCons# :: Int# -> a -> Array# a -> Array# a
rowCons# wid x xs =
    runRW# \s1 ->
        let !(# s2, res #) = newArray# (wid +# 1#) x s1 in
        let !s3            = copyArray# xs 0# res 1# wid s2 in
        let !(# _, fin #)  = unsafeFreezeArray# res s3 in
        fin

{-# INLINE rowSnoc# #-}
rowSnoc# :: Int# -> Array# a -> a -> Array# a
rowSnoc# wid xs x =
    runRW# \s1 ->
        let !(# s2, res #) = newArray# (wid +# 1#) x s1 in
        let !s3            = copyArray# xs 0# res 0# wid s2 in
        let !(# _, fin #)  = unsafeFreezeArray# res s3 in
        fin

{-# INLINE rowUnsafeInsertCenter# #-}
rowUnsafeInsertCenter# :: Int# -> Int# -> Array# a -> a -> Array# a
rowUnsafeInsertCenter# i wid xs x =
    runRW# \s1 ->
    let !(# s2, res #) = newArray# (wid +# 1#) x s1 in
    let !s3            = copyArray# xs 0# res 0# i s2 in
    let !s4            = copyArray# xs i res (i +# 1#) (wid -# i) s3 in
    let !(# _, fin #)  = unsafeFreezeArray# res s4 in
    fin

{-# INLINE rowCons #-}
rowCons :: a -> Array a -> Array a
rowCons x (Array xs) = Array (rowCons# (sizeofArray# xs) x xs)

{-# INLINE rowSnoc #-}
rowSnoc :: Array a -> a -> Array a
rowSnoc (Array xs) x = Array (rowSnoc# (sizeofArray# xs) xs x)

rowReverse :: Array a -> Array a
rowReverse xs =
    let !wid = sizeofArray xs in
    if wid == 0 then mempty else
    runArray do
        !res <- newArray wid (error "rowReverse: uninitialized")
        let go i | i >= wid = pure res
            go i            = writeArray res i (xs ! (wid - (i+1))) >> go (i+1)
        go 0

rowIntersperse :: a -> Array a -> Array a
rowIntersperse x xs =
    let !wid = sizeofArray xs in
    if wid == 0 then mempty else
    runArray do
        res <- newArray (wid*2 - 1) x
        let go i =
                if i >= wid then pure res else do
                    writeArray res (i*2) (xs!i)
                    go (i+1)
        go 0

{-
    Find the first element of an error where the condition holds.
-}
rowFind :: (a -> Bool) -> Array a -> Maybe a
rowFind f a =
    let !wid = sizeofArray a
        go i = if i >= wid then Nothing else
               let x = a!i in
               if f x then Just x else go (i+1)
    in go 0

rowFindIndex :: (a -> Bool) -> Array a -> Maybe Int
rowFindIndex f a =
    let !wid = sizeofArray a
        go i = if i >= wid then Nothing else
               if f (a!i) then Just i else go (i+1)
    in go 0


{-
    This works by creating a temporary mutable array, and filling it
    only with items that pass the filter.  At the end, we copy the items
    that passed the filter into an immutable array.

    This means that 
-}
rowFilter :: (a -> Bool) -> Array a -> Array a
rowFilter f xs =
    let !wid = sizeofArray xs in
    if wid==0 then xs else
    runST do
        buf <- newArray wid (error "rowFilter: uninitialized")
        let go i outIx =
                if i >= wid then
                    if i == outIx
                    then pure xs
                    else freezeArray buf 0 outIx
                else
                    let !x = xs!i in
                    if f x
                    then writeArray buf outIx x >> go (i+1) (outIx+1)
                    else go (i+1) outIx
        go 0 0

-- Ignores out-of-bounds writes.  The output will always have the same
-- size as the input.
rowPut :: Int -> a -> Array a -> Array a
rowPut i x xs =
    let !wid = sizeofArray xs in
    if i >= wid || i < 0 then
        xs
    else runArray do
        res <- thawArray xs 0 wid
        writeArray res i x
        pure res

rowUnsafePut :: Int -> a -> Array a -> Array a
rowUnsafePut i x xs = runArray do
    let !wid = sizeofArray xs
    res <- thawArray xs 0 wid
    writeArray res i x
    pure res

-- This is allocate a new array, and is O(n), not O(1).
rowTake :: Int -> Array a -> Array a
rowTake 0 _ = mempty
rowTake n xs =
    let !wid = sizeofArray xs in
    if n >= wid then xs else cloneArray xs 0 n

-- This allocates a new array, not a slice.  O(n), not O(1).
rowDrop :: Int -> Array a -> Array a
rowDrop 0 xs = xs
rowDrop n xs = if siz <= 0 then mempty else cloneArray xs n siz
  where
    !wid = sizeofArray xs
    !siz = wid - n

-- | This is allocate a new array, and is O(n), not O(1).
rowSlice :: Int -> Int -> Array a -> Array a
rowSlice off sz xs =
    let
        !wid = sizeofArray xs
    in if wid < (off + sz) || off<0 || sz<0 then
        error "rowSlice: out of bounds"
    else if sz==0 then
        mempty
    else if sz==wid then
        xs
    else
        cloneArray xs off sz

rowDropEnd :: Int -> Array a -> Array a
rowDropEnd n xs = rowTake (sizeofArray xs - n) xs

rowDelete :: Int -> Array a -> Array a
rowDelete i xs =
    if i >= sizeofArray xs || i < 0 then xs else rowUnsafeDelete i xs

-- Does no bounds checking
rowUnsafeDelete :: Int -> Array a -> Array a
rowUnsafeDelete i xs =
    let !wid = sizeofArray xs in
    if i == 0 then do
        cloneArray xs 1 (wid-1)
    else if i+1 == wid then do
        cloneArray xs 0 (wid-1)
    else runArray do
        res <- newArray (wid-1) (error "rowUnsafeDelete: uninitialized")
        copyArray res 0 xs 0     i
        copyArray res i xs (i+1) (wid - (i+1))
        pure res

{-# INLINE rowUnsafeInsert# #-}
rowUnsafeInsert# :: Int# -> a -> Array# a -> Int# -> Array# a
rowUnsafeInsert# i x xs wid =
    case i of
    0# -> rowCons# wid x xs
    _  -> case i ==# wid of
              1# -> rowSnoc# wid xs x
              _  -> rowUnsafeInsertCenter# i wid xs x

rowInsert :: Int -> a -> Array a -> Array a
rowInsert i@(I# i#) x (Array xs) =
    let !wid = sizeofArray# xs in
    if i > (I# wid) || i < 0
    then error "rowInsert: out of bounds"
    else Array (rowUnsafeInsert# i# x xs wid)

rowSortBy :: (a -> a -> Ordering) -> Array a -> Array a
rowSortBy cmp xs = runST do
    let !wid = sizeofArray xs
    buf <- thawArray xs 0 wid
    VA.sortBy cmp (MVector 0 wid buf)
    unsafeFreezeArray buf

rowSafeIndex :: Array a -> Int -> Maybe a
rowSafeIndex a i = if i >= sizeofArray a || i<0 then Nothing else Just (a!i)

rowIndexEx :: Array a -> Int -> a
rowIndexEx a i =
    if i >= sizeofArray a || i<0
    then error "rowIndexEx: out of bounds"
    else a!i

rowSortUniqBy :: (a -> a -> Ordering) -> Array a -> Array a
rowSortUniqBy cmp xs = runST do
    let !wid = sizeofArray xs
    buf <- thawArray xs 0 wid
    MVector _ _ rBuf <- VA.sortUniqBy cmp (MVector 0 wid buf)
    unsafeFreezeArray rBuf
    -- sortUniqBy does it's own resize+copy internally if some elements
    -- were duplicated, so this should be safe.  A proper implementation
    -- does not go through this Vector package, but this should work
    -- for now.

rowTailEx :: Array a -> Array a
rowTailEx row =
    if null row then error "rowTailEx: empty" else rowDrop 1 row

rowTailMay :: Array a -> Maybe (Array a)
rowTailMay row =
    if null row then Nothing else Just (rowDrop 1 row)

rowInitEx :: Array a -> Array a
rowInitEx row =
    case sizeofArray row of
        0 -> error "rowInitEx: empty"
        n -> rowTake (n-1) row

rowInitMay :: Array a -> Maybe (Array a)
rowInitMay row =
    case sizeofArray row of
        0 -> Nothing
        n -> Just (rowTake (n-1) row)

rowUncons :: Array a -> Maybe (a, Array a)
rowUncons row =
    if null row then Nothing else
    Just (row!0, rowDrop 1 row)

rowUnSnoc :: Array a -> Maybe (Array a, a)
rowUnSnoc row =
    case sizeofArray row of
        0 -> Nothing
        n -> Just (rowTake (n-1) row, row ! (n-1))

rowReplicate :: Int -> a -> Array a
rowReplicate sz x = runArray (newArray sz x)

rowSplitAt :: Int -> Array a -> (Array a, Array a)
rowSplitAt i xs = (rowTake i xs, rowDrop i xs)

rowTakeWhile :: (a -> Bool) -> Array a -> Array a
rowTakeWhile f xs =
    case rowFindIndex (not . f) xs of
        Nothing -> xs
        Just ix -> rowTake ix xs

rowDropWhile :: (a -> Bool) -> Array a -> Array a
rowDropWhile f xs =
    case rowFindIndex (not . f) xs of
        Nothing -> mempty
        Just ix -> rowDrop ix xs

--------------------------------------------------------------------------------

instance MonoFunctor (Array a) where
    omap = fmap
    {-# INLINE omap #-}

instance MonoTraversable (Array a) where
    otraverse = traverse
    omapM = traverse
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}

instance MonoPointed (Array a) where
    opoint = pure
    {-# INLINE opoint #-}

-- Assert that append (<>) never produces something smaller (this class
-- how no methods).
instance GrowingAppend (Array a) where

-- This implements all methods.
instance SemiSequence (Array a) where
    type Index (Array a) = Int
    intersperse = rowIntersperse
    reverse = rowReverse
    find = rowFind
    sortBy = rowSortBy
    cons = rowCons
    snoc = rowSnoc

instance IsSequence (Array a) where
    fromList = arrayFromList
    lengthIndex = sizeofArray
    take = rowTake
    unsafeTake = rowTake
    drop = rowDrop
    unsafeDrop = rowDrop
    unsafeIndex = (!)
    indexEx = rowIndexEx
    index = rowSafeIndex
    tailEx = rowTailEx
    unsafeTail = rowTailEx
    tailMay = rowTailMay
    initEx = rowInitEx
    unsafeInit = rowInitEx
    initMay = rowInitMay
    uncons = rowUncons
    unsnoc = rowUnSnoc
    replicate = rowReplicate
    splitAt = rowSplitAt
    filter = rowFilter
    dropWhile = rowDropWhile
    takeWhile = rowTakeWhile
    dropEnd = rowDropEnd
    unsafeSplitAt = rowSplitAt

    -- The default implementations are slow.  We should implement these
    -- but they are not currently used.
    replicateM = error "TODO: rowReplicateM"
    break = error "TODO: rowBreak"
    span = error "TODO: rowSpan"
    filterM = error "TODO: rowFilterM"
    groupBy = error "TODO: rowGroupBy"
    groupAllOn = error "TODO: rowGroupAllOn"
    subsequences = error "TODO: rowSubsequences"
    permutations = error "TODO: rowPermutations"
    splitWhen = error "TODO: rowSplitWhen"
    partition = error "TODO: rowPartition"

    {-# INLINE fromList #-}
    {-# INLINE break #-}
    {-# INLINE span #-}
    {-# INLINE dropWhile #-}
    {-# INLINE takeWhile #-}
    {-# INLINE splitAt #-}
    {-# INLINE unsafeSplitAt #-}
    {-# INLINE take #-}
    {-# INLINE unsafeTake #-}
    {-# INLINE drop #-}
    {-# INLINE unsafeDrop #-}
    {-# INLINE partition #-}
    {-# INLINE uncons #-}
    {-# INLINE unsnoc #-}
    {-# INLINE filter #-}
    {-# INLINE filterM #-}
    {-# INLINE replicate #-}
    {-# INLINE replicateM #-}
    {-# INLINE groupBy #-}
    {-# INLINE groupAllOn #-}
    {-# INLINE subsequences #-}
    {-# INLINE permutations #-}
    {-# INLINE tailEx #-}
    {-# INLINE initEx #-}
    {-# INLINE unsafeTail #-}
    {-# INLINE unsafeInit #-}
    {-# INLINE index #-}
    {-# INLINE indexEx #-}
    {-# INLINE unsafeIndex #-}
    {-# INLINE splitWhen #-}
