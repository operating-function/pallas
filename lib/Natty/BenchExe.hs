module Natty.BenchExe (main) where

import Criterion.Main
import Natty
import Prelude

import Data.ByteString (ByteString)


-- Examples --------------------------------------------------------------------

a64, a32768 :: Nat
a64    = (2^64) - 1
a32768 = (2^32768)-1

bDog, bBigDog :: ByteString
bDog    = "The quick brown fox jumps over the lazy dog."
bBigDog = mconcat (replicate 800 bDog)


-- Benchmarks ------------------------------------------------------------------

maiDump = natBytes
sloDump = slowNatBytes
sloLoad = slowBytesNat
gmpDump = exportBytes
gmpLoad = bytesNat

main = defaultMain
    [ bgroup "lit-dump"
        [ bench "slo" $ whnf sloDump a64
        , bench "gmp" $ whnf gmpDump a64
        , bench "mai" $ whnf maiDump a64
        ]
    , bgroup "big-dump"
        [ bench "gmp" $ whnf gmpDump a32768
        , bench "mai" $ whnf maiDump a32768
        ]
    , bgroup "lit-load"
        [ bench "slo" $ whnf sloLoad bDog
        , bench "gmp" $ whnf gmpLoad bDog
        ]
    ]
