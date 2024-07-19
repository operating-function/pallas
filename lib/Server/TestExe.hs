-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Server.TestExe (main) where

import Server.LmdbStore.PinMetadata
import Server.LmdbStore.MdbValue
import PlunderPrelude
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Hash256

import qualified Data.Vector.Storable as VS

--------------------------------------------------------------------------------

main :: IO ()
main = do
    defaultMain tests

instance Arbitrary Hash256 where
    arbitrary = Hash256 <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary PinMetadata where
    arbitrary = PIN_META <$> arbitrary <*> arbitrary <*> arbitrary

instance (Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
    arbitrary = do
        elems <- arbitrary
        pure (VS.fromList elems)

mdbValRoundTrip :: PinMetadata -> Bool
mdbValRoundTrip meta =
    unsafePerformIO $
    withMdbVal meta \val -> do
        meta' <- loadMdbVal val
        pure (meta == meta')

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests"
    [ QC.testProperty "PinMetadata Roundtrip" do
        withMaxSuccess 1_000_000 mdbValRoundTrip
    ]
