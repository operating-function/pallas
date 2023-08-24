-- | `Pq` implements a priority queue datastructure & associated functions.
--   Notably, it does not use a typeclass interface for the "ordering relation"
--   on elements. Instead, the various Pq functions take a `cmp` function
--   argument which expresses the ordering relation.
module Pq where

import Prelude hiding (min)

data Pq a
  = Lf
  | Br a (Pq a) (Pq a)
  deriving Show

type Cmp a = a -> a -> Ordering

testPq :: Pq Int
testPq = fromList compare [1, 9, 3, 2, 4, 7]

-- adapted from https://stackoverflow.com/a/19442407
ppPq :: Show a => Pq a -> String
ppPq (Lf) = ""
ppPq t@(Br _ _ _) = unlines (go t)
 where
  go (Br w l r) =
    show w
      : (pad "+- " "|  " (go l))
        ++ (pad "`- " "   " (go r))
  go Lf = []
  --
  pad first rest = zipWith (++) (first : repeat rest)

insert
  :: Cmp a
  -> a
  -> Pq a
  -> Pq a
insert cmp = go
 where
  go w Lf = Br w Lf Lf
  go w (Br v t1 t2) =
    case cmp w v of
      GT -> Br v (go w t2) t1
      _  -> Br w (go v t2) t1

leftRem
  :: Cmp a
  -> Pq a
  -> (Maybe a, Pq a)
leftRem _ t@Lf = (Nothing, t)
leftRem _   (Br w Lf Lf) = (Just w, Lf)
leftRem cmp (Br w t1 t2) = (mbW, Br w t2 t)
 where
   (mbW, t) = leftRem cmp t1

siftDown
  :: Cmp a
  -> a
  -> Pq a
  -> Pq a
  -> Pq a
siftDown cmp = go
 where
  go w Lf Lf = Br w Lf Lf
  go w (t@(Br v Lf Lf)) Lf =
    case cmp w v of
      GT -> Br v (Br w Lf Lf) Lf
      _  -> Br w t Lf
  go w (t1@(Br v1 p1 q1)) (t2@(Br v2 p2 q2)) =
    case (GT == cmp w v1, GT == cmp w v2) of
      (False, False) -> Br w t1 t2
      _ ->
        case cmp v1 v2 of
          GT -> Br v2 t1 (go w p2 q2)
          _  -> Br v1 (go w p1 q1) t2
  go x t1 t2 = go x t2 t1

delMin
  :: Cmp a
  -> Pq a
  -> (Maybe a, Pq a)
delMin _ Lf = (Nothing, Lf)
delMin cmp (Br v t1 t2) =
  case mbW of
    Just w -> (Just v, siftDown cmp w t2 t)
    Nothing -> (Just v, Lf)
 where
  (mbW, t) = leftRem cmp t1

min
  :: Pq a
  -> Maybe a
min Lf = Nothing
min (Br w _ _) = Just w

empty :: Pq a
empty = Lf

null :: Pq a -> Bool
null Lf = True
null _  = False

heapify
  :: forall a
   . Cmp a
  -> [a]
  -> (Pq a, [a])
heapify cmp ls = go (length ls) ls
 where
  go :: Int -> [a] -> (Pq a, [a])
  go 0 vs = (Lf, vs)
  go n (v:vs) = (siftDown cmp v t1 t2, vs2)
   where
    (t1, vs1) = go (n `div` 2) vs
    (t2, vs2) = go ((n-1) `div` 2) vs1
  go _ _ = error "heapify: invariant violation"

-- | O(n)
fromList
  :: Cmp a
  -> [a]
  -> Pq a
fromList cmp vs = fst (heapify cmp vs)

-- | O(n * log(n))
fromList'
  :: Cmp a
  -> [a]
  -> Pq a
fromList' cmp = foldl (flip (insert cmp)) Lf

-- | ascending order
toList
  :: Cmp a
  -> Pq a
  -> [a]
toList cmp x = go [] x
 where
  go acc t =
    case delMin cmp t of
      (Nothing, _) -> reverse acc
      (Just w, t') -> go (w:acc) t'

sort
  :: Cmp a
  -> [a]
  -> [a]
sort cmp = toList cmp . fromList cmp

sort'
  :: Cmp a
  -> [a]
  -> [a]
sort' cmp = toList cmp . fromList' cmp
