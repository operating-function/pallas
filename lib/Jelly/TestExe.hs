-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
--- OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Jelly.TestExe where

import GHC.Word
import PlunderPrelude  hiding (hash)
import Test.QuickCheck

import Fan.Save         (loadBody, loadPack, saveFan, savePack)
import Fan.Types        (Fan)
import Jelly            (loadBodySlow, loadDepsSlow, saveFast, splitBlob,
                         withContext)
import Jelly.Reference  (Node(..))
import Hash256          (toHash256)

import qualified Fan.Eval        as F
import qualified Fan.Types       as F
import qualified Jelly.Fast.FFI
import qualified Jelly.Reference
import qualified Rex.Types       as Rex


-- Generating Exampels ---------------------------------------------------------

instance Arbitrary (Node ()) where
    arbitrary = oneof
        [ arbitraryNat
        , BAR <$> arbitrary
        , arbitraryPin
        , arbitrary >>= \case { True  -> CONS <$> arbitrary <*> arbitrary
                              ; False -> arbitrary <&> \v -> CONS v v
                              }
        ]

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

arbitraryFakePin :: Gen Fan
arbitraryFakePin = F.mkPin <$> arbitrary

-- Make sure to hit all four length-encodings for leaf entries.
genZeros :: Gen ByteString
genZeros = do
    wid <- (`mod` 96) <$> arbitrary
    pure (replicate (wid::Int) (0::Word8))

instance Arbitrary Fan where
    arbitrary = oneof
        [ F.NAT <$> arbitrary
        , F.BAR <$> genZeros
        , F.BAR <$> arbitrary
        , F.mkCow <$> arbitrary -- (COW 0) == (ROW [])
        , genFanClosure
        , F.REX <$> genRex
        , arbitraryLaw
        , arbitraryFakePin
        , genFanContainer
        ]

-- TODO Other types of rex nodes
genRex :: Gen (Rex.GRex Fan)
genRex = do
    sz  <- getSize
    bit <- arbitrary
    case (sz, bit) of
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
        oneof [ F.ROW . fromList <$> arbitrary
              , F.TAb . mapFromList <$> arbitrary
              , F.CAB . setFromList <$> arbitrary
              ]

instance Arbitrary Nat where
  arbitrary = fromInteger . abs <$> arbitrary @Integer

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary @[Word8]

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = pack <$> arbitrary

arbitraryNat :: Gen (Node ())
arbitraryNat = do
    arbitrary >>= \case
        NatS# w -> pure $ WORD $ fromIntegral (W# w)
        nat     -> pure $ NAT nat

arbitraryPin :: Gen (Node ())
arbitraryPin = PIN () . (toHash256 . pack @ByteString) <$>
  replicateM 32 arbitrary

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

node_roundtrip_fast :: Jelly.Fast.FFI.Ctx -> Node () -> IO Bool
node_roundtrip_fast ctx node = do
    Jelly.Fast.FFI.c_wipe ctx
    (oldDeps, bled, blod) <- saveFast ctx node
    pure $ runChk do
        (bled', blod') <- CHECK (Jelly.splitBlob (bled <> blod))
        unless (bled == bled') do failChk "Head split mismatch"
        unless (blod == blod') do failChk "Body split mismatch"
        deps' <- CHECK (loadDepsSlow bled)
        guardChk "Deps mismatch" (length oldDeps == length deps')
        vl <- CHECK (loadBodySlow (PIN () <$> deps') blod)
        guardChk (tshow ("Value mismatch"::Text, vl, node)) (vl == node)
        pure ()

node_roundtrip_slow :: Node () -> Bool
node_roundtrip_slow node =
    let (oldDeps, bled, blod) = Jelly.Reference.save node
    in
        runChk
    do
        (bled', blod') <- CHECK (Jelly.splitBlob (bled <> blod))
        guardChk "Head split mismatch" (bled == bled')
        guardChk "Body split mismatch" (blod == blod')

        deps' <- CHECK (loadDepsSlow bled)
        guardChk "Deps mismatch" (length oldDeps == length deps')

        vl <- CHECK (loadBodySlow (PIN () <$> deps') blod)
        guardChk (tshow ("Value mismatch got=" <> tshow vl, "gave=" <> tshow node)) (vl == node)

        pure ()


fan_save_same :: Fan -> Bool
fan_save_same fan =
    unsafePerformIO do
        b@(_, _, _) <- F.saveFanReference fan
        a@(_, _, _) <- Fan.Save.saveFan fan
        evaluate (a == b)


fan_pack_same :: Fan -> Bool
fan_pack_same fan =
    unsafePerformIO do
      packed <- Fan.Save.savePack fan
      loaded <- Fan.Save.loadPack packed
      evaluate (Right fan == loaded)

checkHead :: Vector F.Pin -> ByteString -> IO ()
checkHead deps bled = do
    refs <- case loadDepsSlow bled of
                Left msg -> error (unpack msg)
                Right refs -> pure refs
    unless (refs == hashes) do
        error "fanSlow: Pinrefs did not roundtrip"
  where
    hashes = deps <&> (.hash)

fanSlow :: Fan -> IO Fan
fanSlow fan = do
    (deps, bled, blod) <- F.saveFanReference fan

    checkHead deps bled

    -- TODO How to test all combinations of things?
    -- TODO We have fast save, slow save.
    -- TODO We have fast load, slow load.
    -- TODO I suppose it's easiest to just have one test routine that tests them all?
    case loadBodySlow (fmap F.PIN deps :: Vector Fan) blod of
        Left msg -> error (unpack msg)
        Right vl -> pure (F.normalize vl)

fanFast :: Fan -> IO Fan
fanFast fan = do
    (deps, bled, blod) <- Fan.Save.saveFan fan

    checkHead deps bled

    case Fan.Save.loadBody deps blod of
        Left msg -> error (unpack msg)
        Right vl -> pure (F.normalize vl)

fan_fast_save_round :: Fan -> Bool
fan_fast_save_round (F.normalize -> inp) =
    if inp == out then True else
        error $ ppShow (inp, out)
  where
    out = unsafePerformIO $ fanFast inp

fan_slow_save_round :: Fan -> Bool
fan_slow_save_round (F.normalize -> inp) =
    if inp == out then True else
        error $ ppShow (inp, out)
  where
    out = F.normalize $ unsafePerformIO $ fanSlow inp

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
numTries = 50_000

main :: IO ()
main = do
    getCurrentTime >>= print
    checkProp "Fan RoundTrip via Jelly (Optimized)" do
        withMaxSuccess numTries fan_fast_save_round

    getCurrentTime >>= print
    checkProp "Fan RoundTrip via Jelly (Reference)" do
        withMaxSuccess numTries fan_slow_save_round

    getCurrentTime >>= print
    checkProp "Jelly RoundTrip (Reference)" do
        withMaxSuccess numTries node_roundtrip_slow

    -- TODO Gross (need to explicitly think about lifetimes + evaluation order)
    getCurrentTime >>= print
    withContext \ctx -> do
        checkProp "Jelly RoundTrip (Optimized)" do
            withMaxSuccess numTries (unsafePerformIO . node_roundtrip_fast ctx)

    getCurrentTime >>= print
    checkProp "Both Fan Save methods are equivalent" do
        withMaxSuccess numTries fan_save_same

    getCurrentTime >>= print
    checkProp "Fan pack round trip" do
        withMaxSuccess numTries fan_pack_same
