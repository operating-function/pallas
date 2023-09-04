-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

-- | Maps and Sets as Sorted Vectors

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Sorted.Tab
    ( mkTab
    , tabSingleton
    , tabInsert
    , tabLookup
    , tabSize
    , tabElemAt
    , tabSplit
    , tabSplitAt
    , tabSpanAntitone
    , tabMap
    , tabMapWithKey
    , tabUnion
    , tabUnionWith
    , tabIntersection
    , tabDifference
    , tabLookupMin
    , tabLookupMax
    , tabAlter
    , tabDelete
    , tabMember
    , tabElemsList
    , tabElemsArray
    , tabFoldlWithKey'
    , tabFilterWithKey
    , tabKeysSet
    , tabKeysList
    , tabKeysArray
    , tabToAscPairsList
    , tabToDescPairsList
    )
where

import Prelude

import Control.Monad.ST
import Data.Containers
import Data.Foldable
import Data.MonoTraversable
import Data.Primitive.Array
import Data.Sorted.Row
import Data.Sorted.Search
import Data.Sorted.Set
import Data.Sorted.Types

import PlunderPrelude (on)
import GHC.Exts       (Int(..), indexArray#, sizeofArray#, (+#))

-- import qualified Fan.Prof as Prof


-- Searching -------------------------------------------------------------------

{-# INLINE emptyTab #-}
emptyTab :: Tab k v
emptyTab = TAB mempty mempty

{-# INLINE tabSingleton #-}
tabSingleton :: k -> v -> Tab k v
tabSingleton k v = TAB (rowSingleton k) (rowSingleton v)

-- The first key MUST be strictly smaller than the second.
{-# INLINE tabUnsafeDuo #-}
tabUnsafeDuo :: k -> v -> k -> v -> Tab k v
tabUnsafeDuo xk xv yk yv =
    TAB (rowDuo xk yk) (rowDuo xv yv)

-- If found, update the values array at the found index.  Otherwise insert
-- the key and value at the found-index of the relevent arrays.
tabInsert :: Ord k => k -> v -> Tab k v -> Tab k v
tabInsert k v (TAB ks@(Array ks#) vs) =
    -- Prof.withSimpleTracingEventPure "tabInsert" "sorted" $
    let !(# i#, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#) in
    let i = I# i# in
    case found of
        1# -> TAB ks (rowUnsafePut i v vs)
        _  -> TAB (rowInsert i k ks) (rowInsert i v vs)

-- If found, merge the two values with (merge newVal oldVal).  Otherwise
-- insert the key and value at the found-index of the relevant arrays.
tabInsertWith :: Ord k => (v -> v -> v) -> k -> v -> Tab k v -> Tab k v
tabInsertWith merge k v (TAB ks@(Array ks#) vs) =
    let !(# i#, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#) in
    let i = I# i# in
    case found of
        1# -> TAB ks $ rowUnsafePut i (merge v (vs!i)) vs
        _  -> TAB (rowInsert i k ks) (rowInsert i v vs)

-- Do a search on the keys set, if we found a match, return the matching
-- value in the values array.
tabLookup :: Ord k => k -> Tab k v -> Maybe v
tabLookup k (TAB (Array ks#) (Array vs#)) =
    -- Prof.withSimpleTracingEventPure "tabLookup" "sorted" $
    let !(# i, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#) in
    case found of
        0# -> Nothing
        _  -> case indexArray# vs# i of
                  (# res #) -> Just res

{-# INLINE tabSize #-}
tabSize :: Tab k v -> Int
tabSize (TAB ks _) = sizeofArray ks

{-# INLINE tabElemAt #-}
tabElemAt :: Int -> Tab k v -> (k, v)
tabElemAt i (TAB ks vs) =
    if i > length ks
    then error "tabElemAt: out-of-bounds"
    else (ks!i, vs!i)

{-# INLINE tabSplitAt #-}
tabSplitAt :: Int -> Tab k v -> (Tab k v, Tab k v)
tabSplitAt i (TAB ks vs) =
    ( TAB (rowTake i ks) (rowTake i vs)
    , TAB (rowDrop i ks) (rowDrop i vs)
    )

-- Find index, call split (TODO: What behavior on found vs not-found?
-- Avoid off-by-one-errors)
{-# INLINE tabSplit #-}
tabSplit :: Ord k => k -> Tab k v -> (Tab k v, Tab k v)
tabSplit k (TAB ks@(Array ks#) vs) =
    let !(# i#, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#)
        i = I# i#
        j = I# (i# +# found)
    in 
        ( TAB (rowTake i ks) (rowTake i vs)
        , TAB (rowDrop j ks) (rowDrop j vs)
        )

{-# INLINE tabSpanAntitone #-}
tabSpanAntitone :: (k -> Bool) -> Tab k v -> (Tab k v, Tab k v)
tabSpanAntitone f (TAB ks vs) =
    ( TAB (rowTake numTrue ks) (rowTake numTrue vs)
    , TAB (rowDrop numTrue ks) (rowDrop numTrue vs)
    )
  where
    numTrue = bfind f ks

{-# INLINE tabMapWithKey #-}
tabMapWithKey :: (k -> v -> a) -> Tab k v -> Tab k a
tabMapWithKey f (TAB ks vs) = TAB ks (rowZipWith f ks vs)

{-# INLINE tabMap #-}
tabMap :: (a -> b) -> Tab k a -> Tab k b
tabMap f (TAB k v) = TAB k (f <$> v)

{-# INLINE tabUnion #-}
tabUnion :: Ord k => Tab k v -> Tab k v -> Tab k v
tabUnion = tabUnionWith const

-- O(n) union
tabUnionWith :: Ord k => (v -> v -> v) -> Tab k v -> Tab k v -> Tab k v
tabUnionWith merge x@(TAB xKeys xVals) y@(TAB yKeys yVals) =
    -- Prof.withSimpleTracingEventPure "tabUnion" "sorted" $
    case (sizeofArray xKeys, sizeofArray yKeys) of
        ( 0,  _  ) -> y
        ( _,  0  ) -> x
        ( 1,  1  ) -> let xk = xKeys!0
                          yk = yKeys!0
                          xv = xVals!0
                          yv = yVals!0
                      in case compare xk yk of
                          LT -> tabUnsafeDuo xk xv yk yv
                          GT -> tabUnsafeDuo yk yv xk xv
                          EQ -> tabSingleton xk (merge xv yv)
        ( 1,  _  ) -> tabInsertWith merge        (xKeys!0) (xVals!0) y
        ( _,  1  ) -> tabInsertWith (flip merge) (yKeys!0) (yVals!0) x
        ( xw, yw ) -> tabUnionWithGeneric merge x xw y yw

tabUnionWithGeneric
    :: Ord k => (v -> v -> v) -> Tab k v -> Int -> Tab k v -> Int -> Tab k v
tabUnionWithGeneric merge (TAB xKeys xVals) !xWid (TAB yKeys yVals) !yWid =
  runST do
    let rWid = xWid + yWid

    valsBuf <- newArray rWid (error "ssetUnion: uninitialized")
    keysBuf <- newArray rWid (error "ssetUnion: uninitialized")

    let go o i j = do
            let xRemain = xWid - i
            let yRemain = yWid - j
            case (xRemain, yRemain) of
                (0, 0) -> pure o
                (0, _) -> do
                    copyArray keysBuf o yKeys j yRemain
                    copyArray valsBuf o yVals j yRemain
                    pure (o + yRemain)
                (_, 0) -> do
                    copyArray keysBuf o xKeys i xRemain
                    copyArray valsBuf o xVals i xRemain
                    pure (o + xRemain)
                (_, _) -> do
                    let x = xKeys ! i
                    let y = yKeys ! j
                    case compare x y of
                        EQ -> do writeArray keysBuf o x
                                 writeArray valsBuf o (merge (xVals!i) (yVals!j))
                                 go (o+1) (i+1) (j+1)
                        LT -> do writeArray keysBuf o x
                                 writeArray valsBuf o (xVals!i)
                                 go (o+1) (i+1) j
                        GT -> do writeArray keysBuf o y
                                 writeArray valsBuf o (yVals!j)
                                 go (o+1) i     (j+1)

    written <- go 0 0 0

    if written == rWid
    then TAB <$> unsafeFreezeArray keysBuf
             <*> unsafeFreezeArray valsBuf
    else TAB <$> freezeArray keysBuf 0 written
             <*> freezeArray valsBuf 0 written

-- O(n) tab difference
tabDifference :: Ord k => Tab k v -> Tab k v -> Tab k v
tabDifference x (TAB yKeys _) | null yKeys = x
tabDifference (TAB xKeys _) _ | null xKeys = mempty
tabDifference (TAB xKeys xVals) (TAB yKeys yVals) =
    -- Prof.withSimpleTracingEventPure "tabDifference" "sorted" $
    runST do
    let xWid = sizeofArray xKeys
    let yWid = sizeofArray yVals
    rKeys <- newArray xWid (error "tabDifference: uninitialized")
    rVals <- newArray xWid (error "tabDifference: uninitialized")
    let go o i j =
            if i >= xWid then pure o else
            if j >= yWid then do
                let extra = xWid - i
                copyArray rKeys o xKeys i extra
                copyArray rVals o xVals i extra
                pure (o + extra)
            else do
                let x = xKeys ! i
                let y = yKeys ! j
                case compare x y of
                    LT -> do
                        writeArray rKeys o x
                        writeArray rVals o (xVals!i)
                        go (o+1) (i+1) j
                    EQ -> go o (i+1) (j+1)
                    GT -> go o i (j+1)
    used <- go 0 0 0
    if used == 0 then
        pure mempty
    else if used == xWid then
        TAB <$> unsafeFreezeArray rKeys <*> unsafeFreezeArray rVals
    else
        TAB <$> freezeArray rKeys 0 used <*> freezeArray rVals 0 used

{-# INLINE tabIntersection #-}
tabIntersection :: Ord k => Tab k v -> Tab k v -> Tab k v
tabIntersection x y = tabIntersectionWith const x y

tabIntersectionWith :: Ord k => (v -> v -> v) -> Tab k v -> Tab k v -> Tab k v
tabIntersectionWith f x@(TAB xKeys xVals) y@(TAB yKeys yVals) =
    -- Prof.withSimpleTracingEventPure "tabIntersectionWith" "sorted" $
    case (sizeofArray xKeys, sizeofArray yKeys) of
        ( 0,  _  ) -> mempty
        ( _,  0  ) -> mempty
        ( 1,  1  ) -> let xk = xKeys!0
                      in if xk == (yKeys!0)
                         then tabSingleton xk (f (xVals!0) (yVals!0))
                         else mempty
        ( 1,  _  ) -> let xk = xKeys!0 in
                      case tabLookup xk y of
                          Nothing -> mempty
                          Just yv -> tabSingleton xk (f (xVals!0) yv)
        ( _,  1  ) -> let yk = yKeys!0 in
                      case tabLookup yk x of
                          Nothing -> mempty
                          Just xv -> tabSingleton yk (f xv (yVals!0))
        ( xw, yw ) -> tabIntersectionWithGeneric f x xw y yw

tabIntersectionWithGeneric
    :: Ord k => (v -> v -> v) -> Tab k v -> Int -> Tab k v -> Int -> Tab k v
tabIntersectionWithGeneric merge (TAB xKeys xVals) xWid (TAB yKeys yVals) yWid =
  runST do
    let rWid = min xWid yWid
    rKeys <- newArray rWid (error "setIntersection: uninitialized")
    rVals <- newArray rWid (error "setIntersection: uninitialized")
    let go o i j =
            if i >= xWid || j >= yWid then pure o else do
                let x = xKeys ! i
                let y = yKeys ! j
                case compare x y of
                    EQ -> do
                        writeArray rKeys o x
                        writeArray rVals o $ merge (xVals!i) (yVals!j)
                        go (o+1) (i+1) (j+1)
                    LT -> go o (i+1) j
                    GT -> go o i (j+1)
    used <- go 0 0 0
    if used == rWid
    then TAB <$> unsafeFreezeArray rKeys <*> unsafeFreezeArray rVals
    else TAB <$> freezeArray rKeys 0 used <*> freezeArray rVals 0 used

{-# INLINE tabLookupMin #-}
tabLookupMin :: Tab k v -> Maybe (k, v)
tabLookupMin (TAB k v) =
    if null k then Nothing else Just (k!0, v!0)

{-# INLINE tabLookupMax #-}
tabLookupMax :: Tab k v -> Maybe (k, v)
tabLookupMax (TAB k v) =
    case sizeofArray k of
        0 -> Nothing
        n -> let !i = n-1 in Just (k!i, v!i)

tabAlter :: Ord k => (Maybe v -> Maybe v) -> k -> Tab k v -> Tab k v
tabAlter f k tab@(TAB ks@(Array ks#) vs) =
    -- Prof.withSimpleTracingEventPure "tabAlter" "sorted" $
    let !(# i#, found #) = bsearch# compare k ks# 0# (sizeofArray# ks#) in
    let i = I# i# in
    case found of
        1# ->
            case f (Just (vs!i)) of
                Nothing -> TAB (rowUnsafeDelete i ks) (rowUnsafeDelete i vs)
                Just v  -> TAB ks                     (rowUnsafePut i v vs)
        _ ->
            case f Nothing of
                Nothing -> tab -- no change
                Just v  -> TAB (rowInsert i k ks) (rowInsert i v vs)

tabDelete :: Ord k => k -> Tab k v -> Tab k v
tabDelete k tab@(TAB ks@(Array ks#) vs) =
    -- Prof.withSimpleTracingEventPure "tabDelete" "sorted" $
    case bsearch# compare k ks# 0# (sizeofArray# ks#) of
        (# _,  0# #) -> tab
        (# i#, _  #) -> TAB ks' vs'
            where i = I# i#
                  !ks' = rowUnsafeDelete i ks
                  !vs' = rowUnsafeDelete i vs

{-# INLINE tabMember #-}
tabMember :: Ord k => k -> Tab k v -> Bool
tabMember k (TAB ks _) = ssetMember k (SET ks)

{-# INLINE tabKeysSet #-}
tabKeysSet :: Tab k v -> Set k
tabKeysSet (TAB k _) = SET k

{-# INLINE tabKeysArray #-}
tabKeysArray :: Tab k v -> Array k
tabKeysArray (TAB k _) = k

{-# INLINE tabKeysList #-}
tabKeysList :: Tab k v -> [k]
tabKeysList (TAB k _) = toList k

{-# INLINE tabFoldlWithKey' #-}
tabFoldlWithKey' :: (a -> k -> v -> a) -> a -> Tab k v -> a
tabFoldlWithKey' f !x (TAB ks vs) =
    -- Prof.withSimpleTracingEventPure "tabFoldWithKey" "sorted" do
    go 0 x
  where
    !wid = sizeofArray ks

    go i !acc | i >= wid  = acc
    go i !acc | otherwise = go (i+1) $ f acc (ks!i) (vs!i)

tabFilterWithKey :: (k -> v -> Bool) -> Tab k v -> Tab k v
tabFilterWithKey f tab@(TAB ks vs) =
  -- Prof.withSimpleTracingEventPure "tabFilterWithKey" "sorted" $
  runST do
    let !wid = sizeofArray ks
    keysBuf <- newArray wid (error "tabFilterWithKey: uninitialized")
    valsBuf <- newArray wid (error "tabFilterWithKey: uninitialized")
    let go o i | i >= wid  = pure o
        go o i | otherwise = do
            let key = ks!i
            let val = vs!i
            if f key val then do
                writeArray keysBuf o key
                writeArray valsBuf o val
                go (o+1) (i+1)
            else do
                go o (i+1)
    written <- go 0 0
    if written == wid then pure tab else
        TAB <$> freezeArray keysBuf 0 written
            <*> freezeArray valsBuf 0 written

{-# INLINE tabElemsArray #-}
tabElemsArray :: Tab k v -> Array v
tabElemsArray (TAB _ v) = v

{-# INLINE tabElemsList #-}
tabElemsList :: Tab k v -> [v]
tabElemsList (TAB _ v) = toList v

{-
    Creates a table from a list of key-value pairs.  If keys appear
    multiple times, values later in the list are used.

    Implementation:

    - Collect the list of pairs into an array.

    - Do a stable sort on the array (comparing only the keys), removing
      duplicates.

    - Split the resulting array out into a keys array and a values array.

    Note that rowSortUniqBy chooses earlier values, not later values.
    We resolve this by filling the array in reverse.

    TODO: We can skip a copy by directly creating a mutable array,
    and doing the sort on that, in place.
-}
tabFromPairsList :: Ord k => [(k,v)] -> Tab k v
tabFromPairsList pairs =
    -- Prof.withSimpleTracingEventPure "tabFromPairsList" "sorted" $
    let buf = rowSortUniqBy (on compare fst) $ arrayFromListRev pairs in
    TAB (fst <$> buf) (snd <$> buf)

{-# INLINE tabToAscPairsList #-}
tabToAscPairsList :: Tab k v -> [(k,v)]
tabToAscPairsList (TAB k v) = go 0
  where
    !len = sizeofArray k
    go i | i >= len = []
    go i            = (k!i, v!i) : go (i+1)

{-# INLINE tabToDescPairsList #-}
tabToDescPairsList :: Tab k v -> [(k,v)]
tabToDescPairsList (TAB k v) = go (length k - 1)
  where
    go i | i < 0 = []
    go i         = (k!i, v!i) : go (i-1)

{-# INLINE mkTab #-}
mkTab :: Set k -> Array v -> Tab k v
mkTab (SET k) v =
    if sizeofArray k /= sizeofArray v
    then error "mkTab: keys must have same size as values"
    else TAB k v

--------------------------------------------------------------------------------
-- TODO: Optimize and verify these instances

instance Foldable (Tab k) where
    foldMap f (TAB _ v) = foldMap f v
    {-# INLINE foldMap #-}

instance MonoFunctor (Tab k v) where
    omap = fmap
    {-# INLINE omap #-}

-- TODO: Explicit instances
instance MonoFoldable (Tab k v) where

instance MonoTraversable (Tab k v) where
    otraverse = traverse
    omapM = traverse
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}

type instance Element (Tab k v) = v

instance Ord k => Semigroup (Tab k v) where
    (<>) = tabUnion
    {-# INLINE (<>) #-}

instance Ord k => Monoid (Tab k v) where
    mempty = emptyTab
    {-# INLINE mempty #-}

-- Assert that append (<>) never produces something smaller (this class
-- how no methods).
instance GrowingAppend (Tab k v) where

{-
    This explicitly implements all methods

    TODO: Better implementation of `unions`!
-}
instance Ord k => SetContainer (Tab k v) where
    type ContainerKey (Tab k v) = k
    member = tabMember
    notMember k t = not (tabMember k t)
    union = tabUnion
    difference = tabDifference
    intersection = tabIntersection
    keys = tabKeysList
    unions = ofoldl' tabUnion mempty
    {-# INLINE member #-}
    {-# INLINE notMember #-}
    {-# INLINE union #-}
    {-# INLINE unions #-}
    {-# INLINE difference #-}
    {-# INLINE intersection #-}
    {-# INLINE keys #-}

instance Functor (Tab k) where
    fmap = tabMap
    {-# INLINE fmap #-}

-- TODO: implement way more methods.
instance Traversable (Tab k) where
    traverse f (TAB ks vs) = TAB ks <$> traverse f vs
    sequenceA (TAB ks vs)  = TAB ks <$> sequenceA vs

-- TODO: Implement many more methods.
instance Ord k => IsMap (Tab k v) where
    type MapValue (Tab k v) = v
    lookup = tabLookup
    insertMap = tabInsert
    deleteMap = tabDelete
    singletonMap = tabSingleton
    mapFromList = tabFromPairsList
    mapToList = tabToAscPairsList
