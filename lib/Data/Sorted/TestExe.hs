-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Data.Sorted.TestExe (main) where

import ClassyPrelude
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Sorted

import Optics (both, over)

import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Sorted Array Operations" $
    [ setTests
    , tabTests
    ]

setTests :: TestTree
setTests = testGroup "Set Operations" $
    let
        our :: [Int] -> ArraySet Int
        our = setFromList

        their :: [Int] -> S.Set Int
        their = setFromList
    in
    [ QC.testProperty "sorting" \lis ->
          toList (our lis) == toList (their lis)

    , QC.testProperty "reversed" \lis ->
          ssetToDescList (our lis) == S.toDescList (their lis)

    , QC.testProperty "size" \lis ->
          ssetSize (our lis) == S.size (their lis)

    , QC.testProperty "union" \x y ->
          (==) (toList $ ssetUnion (our x) (our y))
               (toList $ S.union (their x) (their y))

    , QC.testProperty "lookupMin" \xs ->
          (==) (toList $ ssetLookupMin (our xs))
               (toList $ S.lookupMin (their xs))

    , QC.testProperty "lookupMax" \xs ->
          (==) (toList $ ssetLookupMax (our xs))
               (toList $ S.lookupMax (their xs))

    , QC.testProperty "intersection" \x y ->
          (==) (toList $ intersection (our x) (our y))
               (toList $ intersection (their x) (their y))

    , QC.testProperty "splitAt-0" \ks ->
          (==) (over both toList $ ssetSplitAt 0 (our ks))
               (over both toList $ S.splitAt 0   (their ks))

    , QC.testProperty "splitAt-99" \ks ->
          (==) (over both toList $ ssetSplitAt 99 (our ks))
               (over both toList $ S.splitAt 99   (their ks))

    , QC.testProperty "splitAt-center" \ks ->
          let center = length ks `div` 2 in
          (==) (over both toList $ ssetSplitAt center (our ks))
               (over both toList $ S.splitAt center   (their ks))

    , QC.testProperty "ssetSpanAntitone-1" \k ks ->
          (==) (over both toList $ ssetSpanAntitone (< k) (our (k:ks)))
               (over both toList $ S.spanAntitone   (< k) (their (k:ks)))

    , QC.testProperty "ssetSpanAntitone-2" \k ks ->
          (==) (over both toList $ ssetSpanAntitone (<= k) (our (k:ks)))
               (over both toList $ S.spanAntitone   (<= k) (their (k:ks)))

    , QC.testProperty "ssetSpanAntitone-3" \k ks ->
          (==) (over both toList $ ssetSpanAntitone (<= k) (our ks))
               (over both toList $ S.spanAntitone   (<= k) (their ks))

    , QC.testProperty "difference" \x y ->
          (==) (toList $ difference (our x) (our y))
               (toList $ difference (their x) (their y))

    , QC.testProperty "insert" \k ks ->
          (==) (toList $ insertSet k $ our ks)
               (toList $ insertSet k $ their ks)

    , QC.testProperty "insert-existing" \k ks ->
          (==) (toList $ insertSet k $ our (k:ks))
               (toList $ insertSet k $ their (k:ks))

    , QC.testProperty "delete" \k ks ->
        (==) (toList $ deleteSet k (our ks))
             (toList $ deleteSet k (their ks))

    , QC.testProperty "delete-first" \k ks ->
        (==) (toList $ deleteSet k (our (k:ks)))
             (toList $ deleteSet k (their (k:ks)))

    , QC.testProperty "has-0" \ks ->
        member 0 (our ks) == member 0 (their ks)

    , QC.testProperty "has-first" \k ks ->
        let lis = k:ks
        in member k (our lis) == member k (their lis)
    ]

tabTests :: TestTree
tabTests = testGroup "Tab Operations" $
    let
        our :: [(Int,Int)] -> Tab Int Int
        our = mapFromList

        their :: [(Int,Int)] -> M.Map Int Int
        their = mapFromList

        ok :: IsMap m => m -> [(ContainerKey m, MapValue m)]
        ok = mapToList
    in
    [ QC.testProperty "singleton" \k v ->
          (==) (ok (singletonMap k v :: Tab Int Int))
               (ok (singletonMap k v :: M.Map Int Int))

    , QC.testProperty "sort" \lis ->
          ok (our lis) == ok (their lis)

    , QC.testProperty "size" \lis ->
          tabSize (our lis) == M.size (their lis)

    , QC.testProperty "insert" \k v ps ->
          ok (insertMap k v (our ps)) == ok (insertMap k v (their ps))

    , QC.testProperty "insert-first" \p@(k,v) ps ->
          ok (insertMap k v (our (p:ps))) == ok (insertMap k v (their (p:ps)))

    , QC.testProperty "lookup" \k ps ->
          lookup k (our ps) == lookup k (their ps)

    , QC.testProperty "lookup-first" \p@(k,_) ps ->
          lookup k (our (p:ps)) == lookup k (their (p:ps))

    , QC.testProperty "delete-first" \k ps ->
          ok (deleteMap k (our ps)) == ok (deleteMap k (their ps))

    , QC.testProperty "delete-first" \p@(k,_) ps ->
          ok (deleteMap k (our (p:ps))) == ok (deleteMap k (their (p:ps)))

    , QC.testProperty "split-at-0" \ps ->
          (==) (over both ok $ tabSplitAt 0 $ our ps)
               (over both ok $ M.splitAt 0 $ their ps)

    , QC.testProperty "split-at-1" \ps ->
          (==) (over both ok $ tabSplitAt 0 $ our ps)
               (over both ok $ M.splitAt 0 $ their ps)

    , QC.testProperty "split-at-20" \ps ->
          (==) (over both ok $ tabSplitAt 10 $ our ps)
               (over both ok $ M.splitAt 10 $ their ps)

    , QC.testProperty "split-0" \ps ->
          (==) (over both ok $ tabSplit 0 $ our ps)
               (over both ok $ M.split 0 $ their ps)

    , QC.testProperty "split-first" \p@(k,_) ps ->
          (==) (over both ok $ tabSplit k $ our (p:ps))
               (over both ok $ M.split k $ their (p:ps))

    , QC.testProperty "span-first" \p@(k,_) ps ->
          (==) (over both ok $ tabSpanAntitone (< k) $ our (p:ps))
               (over both ok $ M.spanAntitone (< k) $ their (p:ps))

    , QC.testProperty "span-first" \p@(k,_) ps ->
          (==) (over both ok $ tabSpanAntitone (<= k) $ our (p:ps))
               (over both ok $ M.spanAntitone (<= k) $ their (p:ps))

    , QC.testProperty "map-with-key" \ps ->
          (==) (ok $ tabMapWithKey  (+) (our ps))
               (ok $ M.mapWithKey (+) (their ps))

    , QC.testProperty "map" \ps ->
          (==) (ok $ succ <$> our ps)
               (ok $ succ <$> their ps)

    , QC.testProperty "union" \x y ->
          (==) (ok $ union (our x) (our y))
               (ok $ union (their x) (their y))

    , QC.testProperty "union-with" \x y ->
          (==) (ok $ tabUnionWith (\x y -> x + (x*y)) (our x) (our y))
               (ok $ M.unionWith  (\x y -> x + (x*y)) (their x) (their y))

    , QC.testProperty "intersection" \x y ->
          (==) (ok $ intersection (our x) (our y))
               (ok $ intersection (their x) (their y))

    , QC.testProperty "difference" \x y ->
          (==) (ok $ difference (our x) (our y))
               (ok $ difference (their x) (their y))

    , QC.testProperty "tab-alter-const" \k v ps ->
          (==) (ok $ alterMap (const v) k (our ps))
               (ok $ alterMap (const v) k (their ps))

    , QC.testProperty "tab-alter-existing" \v k ps ->
          (==) (ok $ alterMap (const v) k (our ps))
               (ok $ alterMap (const v) k (their ps))

    , QC.testProperty "tab-map-with-key" \o ps ->
          (==) (ok $ tabMapWithKey (\k v -> k+v+o) (our ps))
               (ok $ M.mapWithKey  (\k v -> k+v+o) (their ps))

    , QC.testProperty "lookupMin" \ps ->
          (==) (toList $ tabLookupMin (our ps))
               (toList $ M.lookupMin (their ps))

    , QC.testProperty "lookupMax" \ps ->
          (==) (toList $ tabLookupMax (our ps))
               (toList $ M.lookupMax (their ps))

    , QC.testProperty "tabFoldlWithKey" \ps ->
          (==) (tabFoldlWithKey' (\x k v -> (k,v):x) [] (our ps))
               (M.foldlWithKey   (\x k v -> (k,v):x) [] (their ps))
    ]

--------------------------------------------------------------------------------

main = defaultMain tests
