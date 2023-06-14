-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{- | This is a hack to avoid long compile times when changing jet hashes.

    TODO The `unsafePerformIO` hack could be avoided through careful
    engineering.

    TODO Maybe we should even read these from a file?
-}

module Fan.JetHash (jetHashes, installJetHashes) where

import Fan.Jets
import Fan.Print
import Fan.RunHashes
import Jelly.Types
import PlunderPrelude

--------------------------------------------------------------------------------

{-
    Call this immediatly on executable startup.
-}
installJetHashes :: IO ()
installJetHashes =
    writeIORef vJetHash jetHashes

checkHash :: ByteString -> ByteString
checkHash bs | length bs == 32 = bs
checkHash bs                   = error ("Bad hash: " <> show bs)

e :: Text -> Text -> (Text, Hash256)
e nam haz = (nam, btcToHash haz)

tabulate :: [(Text, Hash256)] -> Map Text Hash256
tabulate = go mempty
  where
    go :: Map Text Hash256 -> [(Text, Hash256)] -> Map Text Hash256
    go !acc []          = acc
    go !acc ((n,k):nks) =
         if member n nks then
             error ("Duplicate entries" <> show k)
         else
             go (insertMap n k acc) nks

validate :: Map Text Hash256 -> Map Text Hash256
validate tab = unsafePerformIO do
    for_ runHashes \(nam, hax) -> do
        unless (lookup nam tab == Just hax) do
            error (unpack nam <> " jet has conflicting hash definitions")
    pure tab
  where
    runHashes =
        [ ( "_If"       , ifHash        )
        , ( "_Seq"      , seqHash       )
        , ( "_Trk"      , trkHash       )
        , ( "_Add"      , addHash       )
        , ( "_Sub"      , subHash       )
        , ( "_Mul"      , mulHash       )
        , ( "_Switch"   , switchHash    )
        , ( "tabSwitch" , tabSwitchHash )
        , ( "_Idx"      , idxHash       )
        , ( "_Get"      , getHash       )
        , ( "_Eql"      , eqlHash       )
        , ( "_Lte"      , lteHash       )
        , ( "_Lth"      , lthHash       )
        , ( "_Gte"      , gteHash       )
        , ( "_Gth"      , gthHash       )
        ]

jetHashes :: Map Text Hash256
jetHashes
    = validate $ tabulate
    [ e "_Seq"                "4bEt86gt6UWqH5zbjzCwD1ZsTUrQCma8Ur3e8KJAyNAq"
    , e "_Trk"                "AXuX8gnKrPy5MNN4C3G4ZrqyjeAzCm5K5PVhibnFXFwM"
    , e "_IsNat"              "7JUtr2HZmbdp2mXRiqmDi3UGhhMdCMUtiBKKYApi7Xet"
    , e "_PinItem"            "CcxYUhG78V8DhnaP5eGt36tZYPuoTA8Z5KxTUMCfsJEM"
    , e "_IsZero"             "BSZtuwZSii3x5sCAEske5bEbdqzdgVbHFVcpv9SnmLip"
    , e "_If"                 "DiwZ6jrm6gs9AcVsNDvPfUoYXN1upQbDBMWcEDE66rYu"
    , e "_Not"                "5d1jtANA59DQvT3TvTkSVv3HPT4qNA3yu3iaZCqGXqQr"
    , e "_Bit"                "7ebfLaG8Q9bXrosCAd5g9UAMaHV2nsUVfDSYvtnUBakU"
    , e "_And"                "H9Qn8HEimcMwUw9UifERx8sXPrMYWB6FxENeNfqx7LYT"
    , e "_Or"                 "6bR3TKvSsoeM9WY3fwagHYNM8WRBH8HvyLuJmtUtK4xY"
    , e "_Dec"                "CeUYZzpJkr2h1VR8jmRvm5F1K4ZWgVfPKBo4heaVA1ci"
    , e "_Add"                "8r4PSHT5eB44W7NmQgYc9aZ8thEAfaReAS5nP1w9mQhf"
    , e "_Mul"                "5rQ4eHtxRtwud5754UPLnQDrGLbhquvAe1M2QDGpuNya"
    , e "_Sub"                "GZqurSgma16NoHgM1aAhXWzYigxvgHFKhvzLLgEAmGHZ"
    , e "_Bex"                "5FTCygXoLjeaozWp3jtuBJzgVneWxRrGFdsfrzgorsc"
    , e "_Div"                "HFVYF4AZbhgXyZrFUKStjioWSXTZoRUzDu1A5yDk7ZEw"
    , e "_Mod"                "8J5QHKx2rTg7TPqLbcRsXzRPrPLDyCFAs9oZSCB2SG5H"
    , e "_Lsh"                "EAYGD3goVXutbMzLHeHDwVkrdfgcUnCXk2UTQLgauqpf"
    , e "_Rsh"                "BM822cesJNRJmJzkVZWdqNW7ksdBAH7FqNHACi3ncegF"
    , e "_Dis"                "3FnhdjCwf2DF2Tz3LjtdGxSgNRqQmjCSJG4fKa1ikQKT"
    , e "_Con"                "HtwJry896dVqYQ8YqvyrfNPDJT5mZRDUrMHfRLU6dUJh"
    , e "_Mix"                "FGmyagTzHKk4LbzPnfkDSE3XzjuveAMCUz2C49kVVjA5"
    , e "_Met"                "BEi8krApskKrk83zEL8dQeB8Ljvj7QgbchpnTaz9kf1j"
    , e "_Cmp"                "7SvNCtuhCSxMZErGuXpmuvPt1GH2cDKKw7Bd4RAkZmKF"
    , e "_Eql"                "Em9hprMJLmPK7AjA84aWtEsVMoK5ZMzVjL3ZoSUdTiYd"
    , e "_Neq"                "DA1ntWU9wuPtUJn5UeBgRfBTq2SVzH1XQsm4H6Gzqr8a"
    , e "_Lth"                "2jRjhVE66Jp9BmHCnwBvvkcRYNVgtun3TWBu64STCLav"
    , e "_Lte"                "d3u2pdDEfHopurUEqE5jSWmwoEKTxYZNiQSZNUeTjbn"
    , e "_Gth"                "AHrpkoxayTG974cLqrroe5gmu1x1ptopvdXVgjpRo42L"
    , e "_Gte"                "BwBFEaPt4QFP57yrw9f9diecEKSf9BjpW3Z7fiDUx9Rf"
    , e "_Len"                "FXMzGXLaagW6KzfeP84eJNuBdTYbqt1so9ty6JTPsPTv"
    , e "_Put"                "DWhgN7BJ1AxzeT3qRp2mPCoyyUw3QdkJGZYhXcWCsLW7"
    , e "_Get"                "EJ5VWbFAtZmB8zqj2bhmGvEvdn12uGAHsYA4NGr6Avr3"
    , e "_Idx"                "2HHYTgGduTBgZMmP9AtKk2vRZ2N7p3WpFuqA7wSGJ83Z"
    , e "_Mut"                "zjLgGmYXCLajCz4gUQMRP13mqQmkPMRHnNnJbopAHha"
    , e "_Switch"             "7CQTVw7xCfqkznhkeWoYo8PdhBqbLyLcXv8bDYpfVp13"
    , e "_Weld"               "GUTDuYPHrqH4YzQuugU578YauBpsoEVuvtdWcQjjWHs"
    , e "_Map"                "EtAadbu6s3kpcr5c3LjvFRHtNSX1XUXmCmY9k9KXFQJw"
    , e "_Rev"                "AGvTd4pH1LdaLpFmAT2yrsre5LKVdG1rTmDMX3u1xEin"
    , e "rowCons"             "2hzWkrDUsW1YVF45PC1eJqnvu47v7Qip4S7Kx7o95W9e"
    , e "rowSnoc"             "3Aat88yvxhadTPmDVjxehfmfhmYJHNuCoeqbYrcEds6f"
    , e "sum"                 "3Lx85uYHb3EcFyqrZAkTqzW5oG866iFVcqYYd5mruWtu"
    , e "cat"                 "BVAbW1WNywQmGLsBK2U6JiDq1mdDPRwY8XPdAytV6Qem"
    , e "zip"                 "95SXVT334juwuFURsBBgtJu6KDzxarJexxGcFNpJY3cZ"
    , e "drop"                "7zwbJa4uQ2DCzTec6WSHBBMmmUnvTonBFibqmTMoFCNh"
    , e "take"                "AEhdPSNfwEWkRGABM7zECRCe8wCcjrm8w9tJwfa2H4xT"
    , e "unfoldr"             "DKjdVSxF662rrzisipv2xySGAcTnkbCM8qp8A8n7smPa"
    , e "listToRow"           "4bgGturnvrdjjKa4RGRWsu4CYMuMg7qUsmVND1sgTWSc"
    , e "listToRowReversed"   "7WixiwheGSP1LBmoNNJzTY41zXH3c7ZpxFVWYfZHBAMR"
    , e "isDigit"             "GWxivSETStPa86suK4SEGw4MnXKnRHdo2QcqJ9ggFtPb"
    , e "implode"             "7M9a5dXyPFegtZmH8ipsDRwKkEf71Kipxowbk7nW8oUz"
    , e "cabSing"             "M1vdgF63XmdhtsU6wHJNkunFUB7pb9iABMmwXZ1LBjM"
    , e "cabIsEmpty"          "8g59dnfXDqcRwPCuC6gcDrw1hsjDVMZxBZR2rKvT2TUP"
    , e "cabLen"              "DME8WYeNZoNWczDS7No36kTQgp1fM8CB4oBG8qc8SACC"
    , e "cabHas"              "6KoetZ7E4NWjDPujCL1i5z74xM8qrvd92Ck4teE4r5bv"
    , e "cabMin"              "CHVu4kUoyJfyPaCjxtm5FJ1qgqjnaKWxVFLQtiiqPFaq"
    , e "cabIns"              "AELXDb8j72Km3g1ke1XaQGdNFaQgXCkhWE31mkdVaZ5p"
    , e "cabDel"              "4K1iPQ2YsXbH3Gw334a9ReHfsNMUBKf95RtzK8r4AcuR"
    , e "cabWeld"             "7Xyv74TrhKFJbNraakYAbs1sTxGB1K37KfVxGrQ7cXgq"
    , e "cabCatRowAsc"        "AmhGT4H89QkCbnZ8XoaA4g8N1W3vRW5WJQF13U88kWJH"
    , e "cabDrop"             "CnQ6FHn88jdxL1bfU21QndzC77uaDBC7UmLtW4Bh2wz1"
    , e "cabTake"             "C78A33Xt7MyaRVcf63yM86KJCJj1wZbi5DQDj6Q9yavv"
    , e "cabSplitAt"          "7ZHAKkfw1qwcBsZnqA9VSexTrMHbpLcePe367D6J88Gs"
    , e "cabIntersect"        "4qLXy8eJQMeLzDUbtiyG8hqEKZbvGsubANL1vApaW1Wh"
    , e "cabSub"              "4rSiNzER1UADFiQBDiwQwfwGdXDMDpNa2175sNPATAUH"
    , e "cabSplitLT"          "G3cc1KNdXh7TexeBUjocdhCw5o9obbzQwrRs4354thox"
    , e "tabSing"             "7NSnJUfw7bguJLdLqw65D5zXyQNwzhvjhnpwFMVeo4Rz"
    , e "isTab"               "AeKAnXW8KUQJYDTVFspktoMeEYVaU3Pwm8N2ba7WbKR7"
    , e "_TabKeys"            "BAKkJgTpSWF5oGhwqC3KezcK6usyHRvvZZuZ5BxNxLxB"
    , e "_TabVals"            "ErHrwh6PzV1xnga3SnpSPsVXRuhwuPECi1r1JoNQVbnk"
    , e "_TabKeysRow"         "AdLfRCTcrxfZpTnNyNdh6VnvECApiQR5jbvEuB9Audnm"
    , e "tabIdx"              "3jHdwKbcPzza3Y4wJ8mNspq1ne9L7a7Sxx5N3aqd1FnV"
    , e "_TabLen"             "J4vREepMSDCwJfi4P6jGCYWCQdafQwbYLsHuumMHMJQ9"
    , e "_TabHas"             "8uQNxUuMD15oY1Z4EjdXBw2vLR6Um7MYABQyUAq3Txj"
    , e "_TabLookup"          "Haw2W71fAMSb8exvr3GZdC8xU2GiftxqrqaC5hBLBN2p"
    , e "tabIns"              "AGdRLc3oJJYRBgYUdqYe4LTvfNVegMAzhsz6JTdZffFX"
    , e "tabSwitch"           "DNxLBLVwRPGtzbPbjMJXRVMBC9dsXrQ9wd7mh9EvxQR8"
    , e "tabToPairs"          "9GKzHAskGQaLAKsSGUFdom29XGF5ioJWJsMEN7oAgfFo"
    , e "tabFromPairs"        "AcKk75GnSrrJMAFL5NcfBvifYR9r2si2i7FzotgfeF8r"
    , e "tabToPairList"       "2RLPNfUvfSVJFBftCJExdaTuxVXWhAMc4qk1R2c92RFe"
    , e "tabElemIdx"          "6iKaK184bDRxJ1xGUS3HpexziZ1FEL7zrqJdLgVxvRVi"
    , e "tabSplitAt"          "nvzQBa8eanBdkHrrBRXv5fnziXYLyg6jfQmrahAfsds"
    , e "tabSplitLT"          "6phbbX34LEo3TYww2g7qQ5brDoDTZHm5ASUrZgpQvq83"
    , e "tabAlter"            "C6u3bNC83uvueiTipKAoZG2TKYUjoPw8fnTupBmN7ApD"
    , e "tabMapWithKey"       "5oAnkNVdPbTR42kJ4krAJeqnQaECW5akUPKZQ4qM352s"
    , e "tabUnionWith"        "6P8B5U9NRwPWzEyFuwE5w3ukwC1HLrwh7v8KNWX56J2H"
    , e "tabMinKey"           "4yQjY2D7eosVshNzNbBEjqWwsuWcrx2s7wN36CS7SBmc"
    , e "tabFoldlWithKey"     "Axnpye9ZjPAusjZoU88WpGGfFYkZ3gzfqbQiiaATZoWt"
    , e "padWeld"             "7MZnYrEPYprcB2CUFxULKQSbq15rQjQSynpgHb2W8uVH"
    , e "padCat"              "47FTsQSRHrrULPVkDiJZe3yfyRweZ3gkMRe4jZYztHXJ"
    , e "padFlat"             "Brg9B8GnSPAC2CLY6Z8bTWjGL6ZpGYWqSqBFKtDhRKLg"
    , e "isBar"               "3wg6Rsx4Qh9BzUxCSHgtoupKGiVFQNnD2n5gvdpBiAHW"
    , e "natBar"              "P4wZc5m5GX4NzDd3p992mXVwjJ5R13nWqK6o1oo47nC"
    , e "barNat"              "HfUArXzMPSVqXeMSsWWPEEyPrJ6QvotDf94BzoUukT8s"
    , e "barLen"              "DB1MZXakraoYPoWbeHVEKoB5S9K6Sh81Sk1PRcA2XJ1X"
    , e "barIsEmpty"          "BtvmrCULtfD2y4Avc78ndzDpC2xZ44wb9E7UnuipGAoc"
    , e "barIdx"              "AsVdpx1XgYzS3hFmsvfN5kzZ8YZaB9gSgAh9DkL7boEA"
    , e "barWeld"             "8HbGTT8BaZKZP9wryq6NaqPwKvXerUcZ9n2CxKv2e4uN"
    , e "barCat"              "9L4mGKPputDRx1n25MtP9b3mhz8UWiBdL85sj3kgVTRf"
    , e "barTake"             "8vm9GRRyoUJpGEWd4knphKpFswNf99MCwoiYWuQwidmn"
    , e "barDrop"             "BmVmGRM3VYRZdKgsxurGuzXe3GDAgm8rTDV6AabLFRms"
    , e "barElemIndexEnd"     "GxX4SzxrNTjXZrM5fjcTcLufE5ozkKQHntKmEuYJ4rzg"
    , e "barFlat"             "ErXKjduSVVFb8MwXGFsPDBWH2FWmzzmW5WYUaKiS7G6V"
    , e "barElemIndexOff"     "HZYXtkGfmhnim2BHcMq8D4gBRr1UNWgYoPUsqQozATXv"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "FkTwxZLxCJVBdUT1ZUAA5gZmhvNpTdXtomCV5dj8AzaJ"
    , e "w32"                 "LaB4H4Y8c4g783a2abh5kUbVjgjiNKxDgfmfeaY6Lt6"
    , e "add32"               "4G6gZdztwVJ3bPev15xoavnFpoNY6VdqwNERJXehubDs"
    , e "mul32"               "86VbiS1DHKpo34Ju1Ac1bvi4bMEavHdq9bzp4Ws5SMqz"
    , e "div32"               "HWMkZe62Tx6zHiVxExNkf51RUqx5rx47BFxn2JarFM9T"
    , e "and32"               "ADCum5xBsHEEkKzF3xVBN2H7qPGi7sgmSoudqfYGmQsA"
    , e "or32"                "CVAC3wnVNkdxik3nQbqrk557CsDbF22KmRRVx3yhWyiz"
    , e "xor32"               "BrmmUMPFEUoxmTxeAePxJDYKU6AtoLeUzqazHHRRvRhG"
    , e "lsh32"               "By4AsTWDBGxtTZdD87bzZqHaSWfpyGbk99pCkzhLB97m"
    , e "rsh32"               "HmJmk5tmYKCsrc6ayEVdMuX8jevbMXHYkh8mTaN8tVV2"
    , e "sub32"               "CXLWuVNhEHfL6PdpzMFWksjAsvCEF53oLCkSvLXqmRYR"
    , e "ror32"               "HBCzYHuimPmfH3uDT7PcPZdAxL8NiJsSpZH5NPJy2NHT"
    , e "rol32"               "EDTt4s514xc88fuPwtRmAPQt4fcSi8Q9r5hwT2izqDJA"
    , e "_DataTag"            "87jpJhDHAz6vb7oS5nrxQUraoyr4B3ShiNPPTX3XPyaQ"
    , e "_TypeTag"            "D5YyYco1rZnKhTyKgTdAGnNBt63eBuJY5YE86q6T8cHr"
    , e "blake3"              "8yqLmzz7bHaF9N8XDNZdmaED9LP6X91xZ1TS4y93nmst"
    ]
