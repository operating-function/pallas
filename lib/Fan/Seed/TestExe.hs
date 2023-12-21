-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.Seed.TestExe where

import GHC.Word
import PlunderPrelude  hiding (hash)
import Test.QuickCheck
import Loot.ReplExe
import Fan.Seed

--port Data.Bits       (bit)
import Fan.Types       (Fan, Pin)

import qualified Rex
import qualified Fan.Util        as F
import qualified Fan.Eval        as F


-- Generating Exampels ---------------------------------------------------------

{-
    Some law are data-jets.  We don't want to worry about that here,
    so we just generate them with (%%).
-}
arbitraryLaw :: Gen Fan
arbitraryLaw = do
    body <- arbitrary
    args <- F.NAT . succ . (`mod` 10) <$> arbitrary
    name <- F.NAT <$> arbitrary
    pure (0 F.%% name F.%% args F.%% body)

-- TODO: Old implementation of a shallow arbitraryFakePin. This was changed to
-- make real pins because this sort of fake pins break the pack testing code
-- since it has an empty blob. Leaving this here commented because there was a
-- request to break the generation code into `generateFan` and
-- `generateShallowFan`.
--
-- arbitraryFakePin :: Gen Fan
-- arbitraryFakePin = do
--     let refs = mempty
--     let blob = mempty
--     hash :: ByteString <- pack <$> replicateM 32 arbitrary
--     args <- succ <$> arbitrary
--     let item = F.BAR hash
--     quik <- arbitrary
--     let exec = error "placeholder"
--     pure (F.PIN (F.P{..}))

-- Make sure to hit all four length-encodings for leaf entries.
genZeros :: Gen ByteString
genZeros = do
    wid <- (`mod` 96) <$> arbitrary
    pure (replicate (wid::Int) (0::Word8))

instance Arbitrary Pin where
    arbitrary = do
        item <- arbitrary
        pure $ unsafePerformIO (F.mkPin' item)

instance Arbitrary Fan where
    arbitrary = oneof
        [ F.NAT <$> arbitrary
        , F.BAR <$> genZeros
        , F.BAR <$> arbitrary
        , F.mkCow <$> arbitrary -- (COW 0) == (ROW [])
        , genFanClosure
        , arbitraryLaw
        , F.PIN <$> arbitrary
        , genFanContainer
        ]

-- TODO Other types of rex nodes
genRex :: Gen (Rex.GRex Fan)
genRex = do
    sz <- getSize
    bt <- arbitrary
    case (sz, bt) of
        (0, _)     -> pure (Rex.T Rex.WORD "x" Nothing)
        (_, False) -> pure (Rex.N Rex.OPEN "|" [] Nothing)
        _          -> scale (`div` 2) $ oneof
                          [ Rex.C <$> arbitrary ]

genFanClosure :: Gen Fan
genFanClosure = do
    scale (`div` 2) $ do
        hed :: Fan <- arbitrary
        let arity = F.evalArity hed
        let maxArgs :: Int = arity - 1
        params <- scale (minNatInt maxArgs) arbitrary
        let res = foldl' (F.%%) hed (params :: [Fan])
        if (length params + F.evalArity res /= arity) then
            error (show (length params, F.evalArity res, arity))
        else
            pure (F.normalize res) -- TODO: Hack, we don't want a full normalization every time!
                                   -- What we want is just to normalize
                                   -- this specific closure into a single
                                   -- KLO node.
  where
    minNatInt n i | (fromIntegral i > n) = fromIntegral n
    minNatInt _ i                        = i

genFanContainer :: Gen Fan
genFanContainer = do
    scale (`div` 2) $ do
        oneof [ F.ROW . arrayFromList <$> arbitrary
              , F.TAb . mapFromList <$> arbitrary
              , F.SET . setFromList <$> arbitrary
              ]

instance Arbitrary Nat where
  arbitrary = fromInteger . abs <$> arbitrary @Integer

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary @[Word8]

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = pack <$> arbitrary


--------------------------------------------------------------------------------

newtype Check a = CHECK (Either Text a)
  deriving newtype (Eq, Functor, Applicative, Monad)

runChk :: Check () -> Bool
runChk (CHECK (Left msg)) = error (unpack msg)
runChk _                  = True

failChk :: Text -> Check a
failChk = CHECK . Left

guardChk :: Text -> Bool -> Check ()
guardChk _   True  = pure ()
guardChk msg False = failChk msg

save_seed_same :: Fan -> Bool
save_seed_same fan =
    unsafePerformIO do
        packed <- saveSeed fan
        loaded <- loadSeed packed
        let !ok = (Right fan == (loaded :: Either LoadErr Fan))
        unless ok do
            pPrint (Right fan :: Either () Fan, "/="::Text, loaded)
        pure ok

save_pod_same :: Pin -> Bool
save_pod_same pin =
    unsafePerformIO do
        packed <- savePod pin
        loaded <- loadPod packed
        let !ok = (Right pin == loaded)
        unless ok do
            pPrint (Right pin :: Either () Pin, "/="::Text, loaded)
        pure ok

checkHead :: Vector Pin -> ByteString -> IO ()
checkHead deps bled = do
    refs <- case loadHead bled of
                Left msg   -> error (show msg)
                Right refs -> pure refs
    unless (refs == hashes) do
        error "fanSlow: Pinrefs did not roundtrip"
  where
    hashes = deps <&> (.hash)

fanFast :: Pin -> IO Pin
fanFast pin = do
    (deps, bled, blod) <- savePin pin
    checkHead deps bled
    case loadBody deps blod of
        Left msg -> error (show msg)
        Right vl -> F.mkPin' vl

fan_fast_save_round :: Pin -> Bool
fan_fast_save_round inp =
    if inp == out then True else
        error $ ppShow (inp, out)
  where
    out = unsafePerformIO $ fanFast inp

failed :: IORef Int
failed = unsafePerformIO (newIORef 0)

checkProp :: Testable prop => Text -> prop -> IO ()
checkProp nm chk = do
    putStrLn nm
    res <- quickCheckResult chk
    putStrLn ""
    case res of
        Success{} -> pure ()
        _         -> modifyIORef' failed succ

numTries :: Int
numTries = 1_000_000

main :: IO ()
main = do
  Rex.colorsOnlyInTerminal do
    writeIORef F.vTrkFan trkFan
    writeIORef F.vTrkRex trkRex

    -- TODO Gross (need to explicitly think about lifetimes + evaluation order)
    getCurrentTime >>= print
    checkProp "Single Pin+Header Save is always the same" do
        withMaxSuccess numTries fan_fast_save_round

    -- TODO Gross (need to explicitly think about lifetimes + evaluation order)
    getCurrentTime >>= print
    checkProp "Seedpod Round Trip is always the same" do
        withMaxSuccess numTries save_pod_same

    -- TODO Gross (need to explicitly think about lifetimes + evaluation order)
    getCurrentTime >>= print
    checkProp "Raw Seed Round Trip is always the same" do
        withMaxSuccess numTries save_seed_same
