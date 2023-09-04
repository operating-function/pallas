-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Data.Sorted.BenchExe (main) where

import Criterion.Main
import Nat
import Prelude
import Data.Sorted
import Data.Set (Set)
import qualified Data.Set as S

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)


-- Examples --------------------------------------------------------------------

mkSets :: [Int] -> (ArraySet Int, Set Int)
mkSets ks = (ssetFromList ks, S.fromList ks)

(t1, t1') = mkSets [1,2,4]
(t2, t2') = mkSets [2,3]

(s1, s1') = mkSets [1..5]
(s2, s2') = mkSets [2..7]

(m1, m1') = mkSets [1..999]
(m2, m2') = mkSets $ take 30 [5,10..]

(b1, b1') = mkSets $ take 512 [1,3..] <> [99999]
(b2, b2') = mkSets $ take 512 [1,4..] <> [99999]

(ls1, ls1') = mkSets $ [1..1024]
(ls2, ls2') = mkSets $ take 7 [3,5..]

(lS1, lS1') = mkSets $ [1..1024]
(lS2, lS2') = mkSets $ [55,66,77]


-- Benchmarks ------------------------------------------------------------------

-- TODO: Can our intersection be faster on small examples too?

main :: IO ()
main = do
  evaluate $ force ( (t1, s2, t1', s2')
                   , (s1, s2, s1', s2')
                   , (m1, m2, m1', m2')
                   , (b1, b2, b1', b2')
                   , (ls1, ls2, ls1', ls2')
                   , (lS1, lS2, lS1', lS2')
                   )
  defaultMain
    [ bgroup "set-insert"
        [ bench "t0.mem" $ whnf (ssetMember 0) t1
        , bench "t0.new" $ whnf (ssetInsert 0) t1
        , bench "t0.std" $ whnf (S.insert 0) t1'

        , bench "t1.mem" $ whnf (ssetMember 1) t1
        , bench "t1.new" $ whnf (ssetInsert 1) t1
        , bench "t1.std" $ whnf (S.insert 1) t1'

        , bench "t2.mem" $ whnf (ssetMember 2) t1
        , bench "t2.new" $ whnf (ssetInsert 2) t1
        , bench "t2.std" $ whnf (S.insert 2) t1'

        , bench "t3.mem" $ whnf (ssetMember 3) t1
        , bench "t3.new" $ whnf (ssetInsert 3) t1
        , bench "t3.std" $ whnf (S.insert 3) t1'

        , bench "t4.mem" $ whnf (ssetMember 4) t1
        , bench "t4.new" $ whnf (ssetInsert 4) t1
        , bench "t4.std" $ whnf (S.insert 4) t1'

        , bench "t5.mem" $ whnf (ssetMember 5) t1
        , bench "t5.new" $ whnf (ssetInsert 5) t1
        , bench "t5.std" $ whnf (S.insert 5) t1'

        , bench "m0.new" $ whnf (ssetInsert 0) m1
        , bench "m0.std" $ whnf (S.insert 0) m1'

        , bench "m1.new" $ whnf (ssetInsert 1) m1
        , bench "m1.std" $ whnf (S.insert 1) m1'

        , bench "m500.new" $ whnf (ssetInsert 500) m1
        , bench "m500.std" $ whnf (S.insert 500) m1'

        , bench "m1000.new" $ whnf (ssetInsert 1000) m1
        , bench "m1000.std" $ whnf (S.insert 1000) m1'
        ]

    , bgroup "set-union"
        [ bench "tin.new" $ whnf (ssetUnion t1) t2
        , bench "tin.std" $ whnf (S.union t1') t2'

        , bench "sml.new" $ whnf (ssetUnion s1) s2
        , bench "sml.std" $ whnf (S.union s1') s2'

        , bench "mid.new" $ whnf (ssetUnion m1) m2
        , bench "mid.std" $ whnf (S.union m1') m2'

        , bench "big.new" $ whnf (ssetUnion b1) b2
        , bench "big.std" $ whnf (S.union b1') b2'

        , bench "skw.new" $ whnf (ssetUnion ls1) ls2
        , bench "skw.std" $ whnf (S.union ls1') ls2'
        , bench "skw.dts" $ whnf (S.union ls2') ls1'

        , bench "SKW.new" $ whnf (ssetUnion lS1) lS2
        , bench "SKW.std" $ whnf (S.union lS1') lS2'
        , bench "SKW.dts" $ whnf (S.union lS2') lS1'
        ]

    , bgroup "set-intersection"
        [ bench "tin.new" $ whnf (ssetIntersection t1) t2
        , bench "tin.std" $ whnf (S.intersection t1') t2'

        , bench "sml.new" $ whnf (ssetIntersection s1) s2
        , bench "sml.std" $ whnf (S.intersection s1') s2'

        , bench "mid.new" $ whnf (ssetIntersection m1) m2
        , bench "mid.std" $ whnf (S.intersection m1') m2'

        , bench "big.new" $ whnf (ssetIntersection b1) b2
        , bench "big.std" $ whnf (S.intersection b1') b2'

        , bench "skw.new" $ whnf (ssetIntersection ls1) ls2
        , bench "skw.std" $ whnf (S.intersection ls1') ls2'
        , bench "skw.dts" $ whnf (S.intersection ls2') ls1'

        , bench "SKW.new" $ whnf (ssetIntersection lS1) lS2
        , bench "SKW.std" $ whnf (S.intersection lS1') lS2'
        , bench "SKW.dts" $ whnf (S.intersection lS2') lS1'
        ]
    ]
