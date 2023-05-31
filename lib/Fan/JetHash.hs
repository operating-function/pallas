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
        [ ( "if"        , ifHash        )
        , ( "ifNot"     , ifNotHash     ) -- use inlining instead of jetting.
        , ( "switch"    , switchHash    )
        , ( "_Seq"      , seqHash       )
        , ( "_Trk"      , trkHash       )
        , ( "tabSwitch" , tabSwitchHash )
        , ( "idx"       , idxHash       )
        , ( "get"       , getHash       )
        , ( "add"       , addHash       )
        , ( "sub"       , subHash       )
        , ( "mul"       , mulHash       )
        , ( "eql"       , eqlHash       )
        , ( "eqlNat"    , eqlNatHash    )
        , ( "lteNat"    , lteNatHash    )
        , ( "lthNat"    , lthNatHash    )
        , ( "gteNat"    , gteNatHash    )
        , ( "gthNat"    , gthNatHash    )
        ]

jetHashes :: Map Text Hash256
jetHashes
    = validate $ tabulate
    [ e "_Seq"                "4bEt86gt6UWqH5zbjzCwD1ZsTUrQCma8Ur3e8KJAyNAq"
    , e "_Trk"                "AXuX8gnKrPy5MNN4C3G4ZrqyjeAzCm5K5PVhibnFXFwM"
    , e "_IsNat"              "7JUtr2HZmbdp2mXRiqmDi3UGhhMdCMUtiBKKYApi7Xet"
    , e "_PinItem"            "CcxYUhG78V8DhnaP5eGt36tZYPuoTA8Z5KxTUMCfsJEM"
    , e "if"                  "9cFk4oxzbH92Bi8jgw7k5LYJNkhTLBxAiQfJ3x3baf6e"
    , e "ifNot"               "66DQ83azQBPKg3E5zy8TFyr7X9jcbNW3acZUtgJnhfdC"
    , e "not"                 "AhoArogkDqhsucCZH4seXXmLXr1a6b4z16gWm3jRiPLX"
    , e "bit"                 "GYgw6LH6KVRiYLFhcWrVpfAWK4PBuWWbiNYCvfx1FLFm"
    , e "and"                 "GiRoy1SVANxdk4cDQn6HkQYVVWZea5kovi8knYysadAX"
    , e "or"                  "63SSHPno9FG8uTkcSq5WGuu5uZgD7X9AoydRz5NhMzjv"
    , e "dec"                 "BKm4tM8GRUdHC26Vi4NRaxVVQrHtAy49om6wjB4kkD2Y"
    , e "add"                 "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"
    , e "mul"                 "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"
    , e "sub"                 "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"
    , e "lteNat"              "AhChaxy3yaRdMSLWzuhVkWb5sRHMtdUL6gV4FNNafE6g"
    , e "lthNat"              "2BDyomgtS4AkZutt77LiZtmipwPuKEYkPcdz6tr9URkD"
    , e "gteNat"              "BbKRjpR2Xs1XgGUb8ZSC1u4VceveHtyEkbo5t8tyRsJL"
    , e "gthNat"              "8itNV8d9SwGrkLb9VYE1oS9L1BsnxM1JtSehYvbcL57C"
    , e "eqlNat"              "HinQSPoGNvnL6DG3mkeKKtiyCTnCYaS3oNzRsGSw3Ccx"
    , e "cmpNat"              "VPtpxVf3w6zEogUcVdermsoD7kNzVea59TedJLwsse5"
    , e "div"                 "De42z4xU8sRe5uP8DmngkCR2RKSXUX6nUNvyGcCDLbyb"
    , e "mod"                 "HwFtJwQqtfdLsveAKoFQyD4sAt21S7pgYY2LYnrLqyM8"
    , e "bex"                 "DdmJnDFakbPhNoNnQuYRarPXMt3aaej5VLarUaja1PE6"
    , e "lsh"                 "27LCe5bqqWsF8CZBSYvHHK5wirqPueTbtJpaFiYdM4da"
    , e "rsh"                 "DC71Care5w6UeJ9cRMisCqX5DW15mNpY6QPR65HhaotT"
    , e "met"                 "9wcC9WfoA5Drzz44xWd5FM7D4JC3qqghzrBXth2dtnqE"
    , e "isZero"              "HKR8YB2R9f2i2U7oBhRqZSKdTWqJYr4UbH64MFuLzEZ2"
    , e "dis"                 "B3dNuJnws6wQwAEUXguYX6uTF3Pv94u5yHKt7hMzVZ43"
    , e "con"                 "G9tZP3bKXAZ8xeiP5jcxKGys77AyxtbUDUKM872s5LVQ"
    , e "mix"                 "D8LUwwTfnStDC8NMcfTJaD21BmViWcpamhmXTk6SKrL6"
    , e "len"                 "GAZbDeWDnxhs9uG5VWGDNJenBqpewfEo6wu2WSAXDTYU"
    , e "put"                 "9CtU2LAAw6VSvA43GggZQzPfS4TLBeYRgw9GXQdaFEeV"
    , e "get"                 "BJ3xwnsARKMVPxZSRckibgUMKKLd3Qo5XKvS8sK6BqU"
    , e "idx"                 "GkNafYAwRbzsipgsEWBrt9NUSgbQhAueCYa4J1mtCjUa"
    , e "mut"                 "6cpAn2bqg4v1SEQy5xUAziYVftzdbB9i9DnqHoMmRbaa"
    , e "cmp"                 "A8dQyDwwLv6cfD3xhsvdPzi9XHmy1LSoJdXbxzx9pWkN"
    , e "eql"                 "FKLUVd4DncStLVn75NiXLqW5JpHaLoTdVWpUnZY4hatg"
    , e "neq"                 "djHUBSQGDtzNdKmyG82sa8CaRHhmhsjdHdxzmqZh5WQ"
    , e "weld"                "DRWnW8LyFVNYPd4xdh2ghtRsnRZMcq82swCkL959g1ax"
    , e "map"                 "DN7ftYdagCVm266WNtBtSyuzQxS2SzMVxFgzPWzS6Rcj"
    , e "rev"                 "5RvQ5BbuJavcyryaN9SzwHfjLAax6s1ZodGaNYu3PYCA"
    , e "rowCons"             "DgZ58w7xDxytYBSoEDhKe7pbsAkcxUQW4QnuZfm8LTB4"
    , e "rowSnoc"             "8nqxgjEWqqhm9qHhxuKndUw7YiMKNo2LqNBH1xBSMXh3"
    , e "sum"                 "DE7CtDknPYzcGwYZgkxim1McQH5C9cLaGR47DrzQWB9x"
    , e "cat"                 "B5gHXsqMmYcbv5MWuVciVDu221sFM3LLiZBRvC1bt7mG"
    , e "zip"                 "DuJvfDdkDMocFXoBp5nDQ2n2kBZZhfnXdJLcLuAo4Z9b"
    , e "take"                "A8Ss9cDtHA27dUq71hdqDXGYZWgs4Lk9WMdzKYAK4CTD"
    , e "drop"                "6SmWXfq9T4Ej1am6E9CUEgWUEZWVDNZgos687ekQAtf7"
    , e "unfoldr"             "BTP3j6NtptkKkgGsXN9RMWpTSmMNMRT2b7siNJz4RGnt"
    , e "switch"              "7PUddHfrSKjH19U4nEj8z5nwV35QqSNrt8AiFgkQ18GX"
    , e "listToRow"           "72cfNTvfLDConfu3jgxDsg5ir653HUDfTbJmpYNHb1Mw"
    , e "listToRowReversed"   "9UjvXnSXG343vT3GcC5xF3PdfLZmY9ypDDmEeJaNT83Q"
    , e "isDigit"             "Auob9kWUEZr5JfLU9CgqzSU16E66X9GuBHRNmuRQJRdW"
    , e "implode"             "4V67EKTuBCb3yNhmzLqZKeH7kvQtxbaSQgHNWzD9Vpgj"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "8K8sT93TzYS5i9wXf6DiDLhwEta4suPwSWDkrXRrotCF"
    , e "cabLen"              "68tQb6rpq7gNA83TmKhXdQtBFPba9CxPkKttfZWzYQDm"
    , e "cabHas"              "FGWayd7UqLka3MteSAfktJF3bPzSaWjaPjUmyHmJmAXx"
    , e "cabMin"              "5uM14zJ819JYzzbmsjP3chqKtsmm3tqw1pu9KfLUpKsi"
    , e "cabIns"              "FG28VcXMVPWYhznyaFztFvrTyyWeqg66XT3eRkXPFbWS"
    , e "cabDel"              "4M2vs7oqpQLjVtenb7nw1dwdDfxUc3jEgFt6EwYmN1jD"
    , e "cabWeld"             "9zJTAuZHUj5mWFfVALBNtESDNmXnR389jqV3ajQoq3b5"
    , e "cabCatRowAsc"        "D1rigekWXHYWFNy1To953fPbbmTcq2nxmZL9J7yERQGB"
    , e "cabDrop"             "GM5FeBs6aEGaK6NbAHzYv4nHu5wXRvTzRjC2V7WA3W4B"
    , e "cabTake"             "Gi8AssNPtegPq2WPB37pTzLwsLgwqfEXdfyxgdbcnxBL"
    , e "cabSplitAt"          "BfrZ7am8ucn3iX1s15y5XQXVgo2bVFRx16m1dgUYxm4b"
    , e "cabIntersect"        "74MnjBSHGBdmWwriseZpEkFuwkMjAffdne1DYQzaxqwV"
    , e "cabSub"              "DRzzpfB55NFWbt3NKtwrXyffU98k6C6MkJZZ5mVs3hS"
    , e "cabSplitLT"          "ESTbrxV4Nt6CwmLoY2W9chU3RPYAcLWLAKM2fS8ANHXs"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "51DUhoSwv7ayy2bDaFqASE3eqperkx6ZRgkehSax7rry"
    , e "tabIdx"              "Cd5wpMtmxua7adh1DkrcJ11xuTqxqBFZvob4F3fZ3rjj"
    , e "tabLen"              "GYCTo3ngEG6C7XuwL1f9327Zm6EqdznafBvojxTAqQaA"
    , e "tabKeysRow"          "4r3Xpewt5wFmEEQ2kMaTkVkwmekiNDZ9W5gT4vztSQ4u"
    , e "tabVals"             "Fb6QL4y8iLG4nspQZjzoRNpQD2fqKQXb5gzuTC4YUyVY"
    , e "tabHas"              "9ss629s6U54ETgvkLVp9hTJfDEQpbZrr2MSTn2W3xxTG"
    , e "tabLookup"           "DiCENvT3g2GRQfMTZDxmkQtEmHsuCn2rEchFFdKwkqzM"
    , e "tabIns"              "HnEt9uM6PsQzyeq8VVapx73zo6UTzpbr2tyKD2sMZCos"
    , e "tabSwitch"           "66LcDLd15GTe48kuN1CS5BfRrrZaQPPo4VkYP5nEWMCp"
    , e "tabToPairs"          "BnctpwZGj5V78YrqGNiQpWiRD2DVW8Xx1B5QprKhEWMh"
    , e "tabFromPairs"        "7R8JcB3N3mZvEsecFEcnYBSUNFXk9YfZCSnvgYfEPCfM"
    , e "tabToPairList"       "5PMBmXDVGFUCDUSiEDmMJJAcFc1NyrF6JnuoAYKXFxiv"
    , e "tabElemIdx"          "26tXrDqQzktUtFNfh8LvLgmesusA35ryDMNFH1tQv3Sx"
    , e "tabSplitAt"          "6VcmBFBSfa52W4pk14pq2dSBjvtgNBQvnbuH2K6Kz4jz"
    , e "tabSplitLT"          "e6CkmvYqXuuEbivNZ7HU3CAAEYfUAZkCQoXvF43hppK"
    , e "tabAlter"            "HsRBFz2qhS8yMVuJS1DVgvQzWq82gSEoGNbhmynHaMyX"
    , e "tabMap"              "BA76WtQPs4AsGzJqWuzoA3uHNtXUouvN8n28wT7RGJcC"
    , e "tabUnionWith"        "72pfeoG69sESUAdDyQTCPWedMJpv7wVjvm2fVnWf2Q4Z"
    , e "tabMinKey"           "9yP32saU1K4hCC3Vg36kCo9P87MKbcD8nXdre9HqLPpo"
    , e "tabFoldlWithKey"     "FJ4hhiZJutj3VgxwsjA4DRFB36TCaiFMSptWAUnwtcT7"
    , e "padWeld"             "9rrhVTocNMg1jy4mji9qETRQk9TGAbKMaBfeQjjiZQHG"
    , e "padCat"              "A7VDR3KksLEBzYYv1q7HMqmnCxrrt8dgprVtESwH46zP"
    , e "padFlat"             "9S1jLDciVxqySLDaTEmUQS3saAH4pFCranpWnFVmKPq5"
    , e "isBar"               "7Tr1kbsoraadKRZzZjbr2tjjktzJuopZ4mmeGBQCppxw"
    , e "natBar"              "8HKuiEDXDebEMGjeN3ajh5e5sURtZFUzynh7eRgVtHPv"
    , e "barNat"              "C7JyeFGUQY32u96vz9gPNAgc1E6VEVhHjZfiN3rW4R5X"
    , e "barLen"              "E4xc2Xn5YB6eJQkZQnJSsfUrGJJhGrAMVrjEjv7BjMSY"
    , e "barIsEmpty"          "FSndzfCFLrYWW8kdHEcFabW5irqP2Z6RokJ8fVkQSFDp"
    , e "barIdx"              "7wx7GyPcNXpeUqYz9e6NYETATbCojU7kuGPnQKpNgNYM"
    , e "barWeld"             "6X2GXYbY55W2a93hxRyeSVqGBmuvZCc5K4Ps6gPpQgEs"
    , e "barCat"              "Fj9bNmhD6ZQDPhLMreXvSq1pa6YbHpCUhBTALSsc2QJX"
    , e "barTake"             "4PV9oCNdoXaak4Suj3QtnX4y7gU8JihKbJ73y7GuApbS"
    , e "barDrop"             "7dfg23oR5hQvfiU9QGpuDZ7P5EdSQY9nAFNNmwN6SaBC"
    , e "barElemIndexEnd"     "J4bn58it8MZeLJWkaZZM1Gg6H3i6EqZ3H4dvo3us1sWo"
    , e "barFlat"             "FK6W2BboikNQVAFBAqsLUTSQ1K4S2yBFbXRvVt4FMAyF"
    , e "barElemIndexOff"     "4U7GsajEUbKeQpdsAWFYrChfQkpdzW2o7wrmaAdX4hWF"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "FkTwxZLxCJVBdUT1ZUAA5gZmhvNpTdXtomCV5dj8AzaJ"
    , e "w32"                 "7wSdWvgXnoi4fwLqbfsdacVUs2yWRPxu5UECjbLPHHnJ"
    , e "add32"               "26B2743MLXb7ZMGxZjczUM9sH2vETq6Zb8DfPUU8D4SB"
    , e "mul32"               "4cFK6HPZ112WbMFSigMD2Tucr92h3njbjV1KfVSzGfZw"
    , e "div32"               "BDSFTfF9SZ6EHTrzMFLYVtv6HAxwpdo6Yz4Hj7VTnAcz"
    , e "and32"               "6ZLZp4wkYLnQBRdC3oaWG3n2TVZHdYSZdqCYUk9HCmK3"
    , e "or32"                "FyKU5HHmCkof116WwWiqnDRWC1LZL4xPMW2USEHCKhbK"
    , e "xor32"               "QCstBU9kg3YNhAv5WqqpacAFTph2AEUJFxRGAs9DMFf"
    , e "lsh32"               "BxkjxroujuJz7Fdcm1bVWCJnHdB18FTe3zeKw1RBo4U"
    , e "rsh32"               "AHPJBXxQnNkX9bHWPWyUNKfoh5xTHEYL9JaMWn81ics6"
    , e "sub32"               "9sTCbDhiqGnah3s45iAoj7YPqzg9GsLnXxXTWw4At3zj"
    , e "ror32"               "E82KR6x3po3NXnKVuyK2nbgTvseuBxujajuF2fDjzGdB"
    , e "rol32"               "CJXQJL74oTLB4iS9uB1tBhuWQr5aZzCWsctADYmDpTbn"
    , e "blake3"              "4PKR8Gy8TWCAgiRe7FPPbfVfh7UrmBDyV9gGR2GFXpSM"
    ]
