-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wno-deprecations #-}

module Nat.TestExe (main) where

import Data.IORef
import Prelude
import System.Exit
import System.IO.Unsafe
import Test.QuickCheck

import Control.Monad         (when)
import Data.ByteString       (ByteString)
import Data.Vector.Primitive (Prim, Vector)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Primitive  as VP
import           Nat


-- Instances -------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance (Prim a, Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = VP.fromList <$> arbitrary


-- Utils -----------------------------------------------------------------------

stripBytes :: ByteString -> ByteString
stripBytes buf = BS.take (len - go 0 (len - 1)) buf
 where
  len = BS.length buf
  go n i | i < 0                     = n
         | 0 == BS.unsafeIndex buf i = go (n + 1) (i - 1)
         | otherwise                 = n

stripWords :: Vector Word -> Vector Word
stripWords vec = VP.take (len - go 0 (len - 1)) vec
 where
  len = VP.length vec
  go n i | i < 0                     = n
         | 0 == VP.unsafeIndex vec i = go (n + 1) (i - 1)
         | otherwise                 = n

dumpLoad :: Eq i => (i -> o) -> (o -> i) -> (i -> Bool)
dumpLoad dump load x = x == load (dump x)

loadDump :: Eq o => (o -> i) -> (i -> o) -> (o -> o) -> (o -> Bool)
loadDump load dump norm x = norm x == dump (load x)


-- Test Reference Implementation -----------------------------------------------

-- prop_nat_bytes_roundtrip :: Natural -> Bool
-- prop_nat_bytes_roundtrip = dumpLoad slowNatBytes slowBytesNat

-- prop_nat_words_roundtrip :: Natural -> Bool
-- prop_nat_words_roundtrip = dumpLoad slowNatWords slowWordsNat

-- prop_bytes_nat_roundtrip :: ByteString -> Bool
-- prop_bytes_nat_roundtrip = loadDump slowBytesNat slowNatBytes stripBytes

-- prop_words_nat_roundtrip :: Vector Word -> Bool
-- prop_words_nat_roundtrip = loadDump slowWordsNat slowNatWords stripWords


-- Test Fast Implementation ----------------------------------------------------

prop_fast_nat_bytes_roundtrip :: Natural -> Bool
prop_fast_nat_bytes_roundtrip = dumpLoad natBytes bytesNat

-- prop_fast_nat_words_roundtrip :: Natural -> Bool
-- prop_fast_nat_words_roundtrip = dumpLoad natWords wordsNat

prop_fast_bytes_nat_roundtrip :: ByteString -> Bool
prop_fast_bytes_nat_roundtrip = loadDump bytesNat natBytes stripBytes

-- prop_fast_words_nat_roundtrip :: Vector Word -> Bool
-- prop_fast_words_nat_roundtrip = loadDump wordsNat natWords stripWords


-- Fast and Reference Implementations are the Same -----------------------------

-- prop_fast_words_nat_correct :: Vector Word -> Bool
-- prop_fast_words_nat_correct x = wordsNat x == slowWordsNat x

-- prop_fast_nat_words_correct :: Natural -> Bool
-- prop_fast_nat_words_correct x = natWords x == slowNatWords x

-- prop_fast_bytes_nat_correct :: ByteString -> Bool
-- prop_fast_bytes_nat_correct x = bytesNat x == slowBytesNat x

-- prop_fast_nat_bytes_correct :: Natural -> Bool
-- prop_fast_nat_bytes_correct x = natBytes x == slowNatBytes x

-- prop_fast_nat_export_correct :: Natural -> Bool
-- prop_fast_nat_export_correct x = exportBytes x == slowNatBytes x


--------------------------------------------------------------------------------

failed :: IORef Int
failed = unsafePerformIO (newIORef 0)

checkProp :: Testable prop => String -> prop -> IO ()
checkProp nm chk = do
    putStrLn nm
    res <- quickCheckResult chk
    putStrLn ""
    case res of
        Success{} -> pure ()
        _         -> modifyIORef' failed succ

numTries :: Int
numTries = 100_000

main :: IO ()
main = do
    -- checkProp "Reference: Nat <-> ByteString roundtrip" $ do
        -- withMaxSuccess numTries prop_nat_bytes_roundtrip

    -- checkProp "Reference: Nat <-> Vector Word roundtrip" $ do
        -- withMaxSuccess numTries prop_nat_words_roundtrip

    -- checkProp "Reference: ByteString <-> Nat roundtrip" $ do
        -- withMaxSuccess numTries prop_bytes_nat_roundtrip

    -- checkProp "Reference: Vector Word <-> Nat roundtrip" $ do
        -- withMaxSuccess numTries prop_words_nat_roundtrip

    checkProp "Fast: Nat <-> ByteString roundtrip" $ do
        withMaxSuccess numTries prop_fast_nat_bytes_roundtrip

    -- checkProp "Fast: Nat <-> Vector Word roundtrip" $ do
        -- withMaxSuccess numTries prop_fast_nat_words_roundtrip

    checkProp "Fast: Bytestring <-> Nat roundtrip" $ do
        withMaxSuccess numTries prop_fast_bytes_nat_roundtrip

    -- checkProp "Fast: Export->Import roundtrip" $ do
        -- withMaxSuccess numTries (dumpLoad exportBytes bytesNat)

    -- checkProp "Fast: Import->Export roundtrip" $ do
        -- withMaxSuccess numTries (loadDump bytesNat exportBytes stripBytes)

    -- checkProp "Fast: Vector Word <-> Nat roundtrip" $ do
        -- withMaxSuccess numTries (prop_fast_words_nat_roundtrip)

    -- checkProp "Fast matches reference: Vector Words -> Nat" $ do
        -- withMaxSuccess numTries prop_fast_words_nat_correct

    -- checkProp "Fast matches reference: Nat -> Vector Word" $ do
        -- withMaxSuccess numTries prop_fast_nat_words_correct

    -- checkProp "Fast matches reference: ByteString -> Nat (Cast)" $ do
        -- withMaxSuccess numTries prop_fast_bytes_nat_correct

    -- checkProp "Fast matches reference: Nat -> ByteString (Import)" $ do
        -- withMaxSuccess numTries prop_fast_nat_bytes_correct

    -- checkProp "Fast matches reference: ByteString -> Nat (Cast)" $ do
        -- withMaxSuccess numTries prop_fast_nat_export_correct

    res <- readIORef failed

    when (res /= 0) $ do
        putStrLn $ "FAILURE: " <> show res <> " tests failed."
        exitWith (ExitFailure 1)

    putStrLn $ "SUCCESS: All tests passed"
