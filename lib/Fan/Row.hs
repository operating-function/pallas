-- TODO: Skip ST and do these things directly in terms of runtime
-- primitives.

module Fan.Row where

import PlunderPrelude
import Control.Monad.ST

type Row a = Array a

{-# INLINE generateRow #-}
generateRow :: Int -> (Int -> a) -> Row a
generateRow sz f = runArray do
    res <- newArray sz (error "undefined")
    let go i | i>= sz = pure res
        go i          = writeArray res i (f i) >> go (i+1)
    go 0

{-# INLINE rowSingleton #-}
rowSingleton :: a -> Row a
rowSingleton x = runArray (newArray 1 x)

rowZipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
rowZipWith f x y =
    generateRow (min (length x) (length y)) \i ->
        f (x!i) (y!i)

rowCons :: a -> Array a -> Array a
rowCons x xs = runArray do
    let !wid = 1 + sizeofArray xs
    !res <- newArray wid x
    copyArray res 1 xs 0 (wid-1)
    pure res

rowSnoc :: Array a -> a -> Array a
rowSnoc xs x = runArray do
    let !wid = 1 + sizeofArray xs
    !res <- newArray wid x
    copyArray res 0 xs 0 (wid-1)
    pure res

rowReverse :: Array a -> Array a
rowReverse xs = runArray do
    let !wid = sizeofArray xs
    !res <- newArray wid (error "undefined")
    let go i | i >= wid = pure res
        go i            = writeArray res i (xs ! (wid - (i+1))) >> go (i+1)
    go 0

rowFilter :: (a -> Bool) -> Array a -> Array a
rowFilter f = arrayFromList . filter f .  toList
  -- TODO: How to make this faster?
  -- There is no shrink, but could use a temporary array of the same
  -- size as the input, and then copy that into a smaller array?

-- DOES NOT DO BOUNDS CHECKING
rowPut :: Int -> a -> Array a -> Array a
rowPut ix x xs = runArray do
    let !wid = sizeofArray xs
    res <- newArray wid x
    copyArray res 0 xs 0 wid
    writeArray res ix x
    pure res

-- This is allocate a new array, and is O(n), not O(1).
rowTake :: Int -> Array a -> Array a
rowTake 0 _ = mempty
rowTake n xs =
    let !wid = sizeofArray xs in
    if n >= wid then xs else
        runArray do
            res <- newArray n (error "undefined")
            copyArray res 0 xs 0 n
            pure res

-- This is allocate a new array, and is O(n), not O(1).
rowDrop :: Int -> Array a -> Array a
rowDrop 0 xs = xs
rowDrop n xs =
    let !wid = sizeofArray xs
        !siz = wid - n
    in
    if siz <= 0 then mempty
    else runArray do
        res <- newArray siz (error "undefined")
        copyArray res 0 xs n siz
        pure res


-- instance GrowingAppend (Array a) where

{-
instance SemiSequence (Array a) where
    type Index (Array a) = Int
    reverse = rowReverse
    cons = rowCons
    snoc = rowSnoc
    intersperse = error "TODO"
    find = error "TODO"
    sortBy = error "TODO"

instance IsSequence (Array a) where
-}

-- instance MonoTraversable (Array a) where

-- instance MonoPointed (Array a) where

-- instance MonoFunctor (Array a) where
