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
        , ( "lte"       , lteHash       )
        , ( "lth"       , lthHash       )
        , ( "gte"       , gteHash       )
        , ( "gth"       , gthHash       )
        ]

jetHashes :: Map Text Hash256
jetHashes
    = validate $ tabulate
    [ e "_Seq"                "4bEt86gt6UWqH5zbjzCwD1ZsTUrQCma8Ur3e8KJAyNAq"
    , e "_Trk"                "AXuX8gnKrPy5MNN4C3G4ZrqyjeAzCm5K5PVhibnFXFwM"
    , e "_IsNat"              "7JUtr2HZmbdp2mXRiqmDi3UGhhMdCMUtiBKKYApi7Xet"
    , e "_PinItem"            "CcxYUhG78V8DhnaP5eGt36tZYPuoTA8Z5KxTUMCfsJEM"
    , e "_If"                 "DiwZ6jrm6gs9AcVsNDvPfUoYXN1upQbDBMWcEDE66rYu"
    , e "_Not"                "5d1jtANA59DQvT3TvTkSVv3HPT4qNA3yu3iaZCqGXqQr"
    , e "_Bit"                "7ebfLaG8Q9bXrosCAd5g9UAMaHV2nsUVfDSYvtnUBakU"
    , e "_And"                "H9Qn8HEimcMwUw9UifERx8sXPrMYWB6FxENeNfqx7LYT"
    , e "_Or"                 "6bR3TKvSsoeM9WY3fwagHYNM8WRBH8HvyLuJmtUtK4xY"
    , e "dec"                 "BKm4tM8GRUdHC26Vi4NRaxVVQrHtAy49om6wjB4kkD2Y"
    , e "add"                 "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"
    , e "mul"                 "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"
    , e "sub"                 "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"
    , e "div"                 "9VJaJLPoEXj45a9maEGA7JmUenUXSYWEbftY7GL2voTw"
    , e "mod"                 "Apfx6KRcM2uyK9cKb37SJcZRtrSdFwh4MMvYr8fSgo66"
    , e "bex"                 "DdmJnDFakbPhNoNnQuYRarPXMt3aaej5VLarUaja1PE6"
    , e "lsh"                 "27LCe5bqqWsF8CZBSYvHHK5wirqPueTbtJpaFiYdM4da"
    , e "rsh"                 "A2nN1iz9pgZBG9TDN5uioBgsyHmg6Euunkf6QhCYQY5m"
    , e "met"                 "9FE1rHwbBGCCTjXJ4rHWSgzBcGFr5RQ2w1kUwdrafxZk"
    , e "isZero"              "4zPmKYdWixhH7wauRaPreLx5wUafiheqTTBCxAHD6jYe"
    , e "dis"                 "8cEotxtKKJVKKwKbmfVPhHoQanCWXGLnv6TZvZE9GST"
    , e "con"                 "GXaS3K4TUYwL4zZq9ZnwoCAzsSg1BYRmbz15ayAiiKpF"
    , e "mix"                 "3zmCFpgytxAsNigH1Nh4thnVb4kV7MoBKnRYjkWiEpfB"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "CwQqDoMvU3YdgN9nsxykafuUSZi35QghjNyECamjFVbM"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "HvVUAWSNH1E1wopVkBen5Vb4dFL9ToFkXCZw7MBiqJmK"
    , e "cmp"                 "Dqag8brQSvij7CXrs76W6qNQpLKD7vV9UHq6saWwgnod"
    , e "eql"                 "9JmUbMwoTSpB3SHs91kEhjowvGVdsciRc5gFXXSCNjH5"
    , e "neq"                 "6Ng5BHW9s3kyCdYZ3L2eaT6MDd1b7M2dDQtVrQDAMDSp"
    , e "lth"                 "JCkizcmensrSx8zG1EoJnVkt1LVixGnQQa3PFs8hvTax"
    , e "lte"                 "DSqUtagTWMuct45mY5hs8zcVykxDyUJfhknfDAKdf4h2"
    , e "gth"                 "3jB62MKq1so7YC2DX8Z3uuCQy9Lpys9cSGGzMvnuGTCE"
    , e "gte"                 "E6F3XSzuqVHD2r2v3wuAsRkPLG2X9onVHJ6DxKqgJLm3"
    , e "weld"                "FeG3XHXKg68xTERYswNNLQLJcCeyGV7NXPWE6V7EVfmz"
    , e "map"                 "8uDZW8m3aT4wT7MDzmi3b2NrA8AXZHkBn3jHoQiFPbjD"
    , e "rev"                 "DaP3nTdpSxdtuscMNyWatsAZtqYZMeQ6XCccY6fFsq4d"
    , e "rowCons"             "jaoLfe4Drq9cnUVx2nKsop4t9jCtiAdrHwUwdAEitxq"
    , e "rowSnoc"             "FNChRDUz4B8kQusrWdjLPqmc4RAZj1ju7Sk21kq7fw5Q"
    , e "sum"                 "HXE5Apzp6ZHUJUPcBnv9kGmfxPGqYniGfTDpDjzRWCbY"
    , e "cat"                 "GEHvU32VpKgzHb1xtAMjzztAV4vbHBabdvDXnTuG8eJF"
    , e "zip"                 "FSPqMCGLmrpkCk3qPwj7bj3a1RkYNraytF4UA8uD3RMt"
    , e "take"                "Co8nH2gk8rPAYimxixJ1gdeWww7LTiBRNYenDc3YH2FE"
    , e "drop"                "287VwgrwR27LodV4n3SBnjW7hUJ51Hckbi6ToLehU9oK"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "HSan7fkr6FhiNkBqmMVnA5TMPtAfVbDpwhLTqFe28y95"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "3RQtaCCRC3nwoi8P6Vgdhm8huf4jKrrYrhA9Af62yG8i"
    , e "implode"             "7Bpup7qVTz6GXkYQ2V4QrezTWUFFwoXCwGUo5Ru2Dey7"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "3WTQcN1wbB48PFTekYjsMrbfFZ3DEZmhHqXD7d5Wimp2"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "BmXVMcgnmWJ2ZY1ThBWGeFRqx1b3AVKui7igABr6N7zG"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "EPqXAR3WSfAWMdSHrjJJCBYJ8GWFTNMRMSoeMzVaWUMw"
    , e "cabDel"              "6CLHBAE1BYDSbNc8encfYPoykb7DBWMCgq58FTJu7FiW"
    , e "cabWeld"             "GU89tEPYUHeFzwHWgYZ1n2ZtRQkBd1wb8puFaj5QWpTD"
    , e "cabCatRowAsc"        "Dh6pE56Soh3Bq6zCMhHgTPA3EbkaAdCDRFwvwP8eNQJk"
    , e "cabDrop"             "GijstBKL1JpcUXrhCQgGJztp3aobaMWHxVrW7EaD25PM"
    , e "cabTake"             "6bJv4CtCoPS43YWgE2tuWH3GZxgZTSWskC88rHnquz19"
    , e "cabSplitAt"          "2whsLyxtWPttxJ7t23xZP3iqyEJJbwctokLZEK3dMxtc"
    , e "cabIntersect"        "5AVZtartUYXhNeVZsyJYmbDibcVToeME8PPQdVe2EtCi"
    , e "cabSub"              "8JJ8Qx7CqREcyqC28UGTyGJuxhY4PTQjmPdnSGebRSQQ"
    , e "cabSplitLT"          "HW7WKxB9uNBdvKNvewC2TsaNnpDEFYiTafXpe6nXUGDE"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "FXBbc6wdfiPonRxE5awg3NS7qMwQJ9h4ct8bE6t1vnQB"
    , e "tabIdx"              "FPjkg2TRJLvbKtuDXoXTJBJfsooPse72BJuraqPgHrkG"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "4HvqAzLw11cZyZWdb2Cv49yBN1dZPki2t4serLYBnZFP"
    , e "tabHas"              "BBGhfiJZScSDjCQHu6hRZ4cj9M3LAbr8Mz9V1WTCFdK6"
    , e "tabLookup"           "8RhfaXM7TdpipsZRn1wif9P3ZeoNtNcZ1myd7Wk6Ztbo"
    , e "tabIns"              "8nKtQ93845z4KNnVyVYwVcU13oQ3fE8paiyaUHiV9kg8"
    , e "tabSwitch"           "6LbGGZYcEtKVHBJWpFSjd44wx7CEYfRqtkS9cUDTeqiy"
    , e "tabToPairs"          "14iJeiwz6FmTvPb856K1z2F9EXxuDFQS2qS4zWsgLFRF"
    , e "tabFromPairs"        "5u1cxCJbjvwgPSn5sY6YMs1meFf865LHeGzZ9TF57wNV"
    , e "tabToPairList"       "Hjon2UygJEHEdmJqiXte5uQDExsV1VYHKUHwTKue4B9k"
    , e "tabElemIdx"          "2tH4ZHoqqdERhHVGzfUG7xB8VnJ3NjM3yRSaibdwr75t"
    , e "tabSplitAt"          "F9anw41hC9jqj5zKHFoLhESqhu5fnjYH8cttkMEShonU"
    , e "tabSplitLT"          "3zhPWcvXr6DhvxJKbmiMTmT21sVSxLFZnFAp7s2KfQSP"
    , e "tabAlter"            "BFvMrYxU3qEPysojzr9DpFtxTDGCJGkoaL7aU5i6fWdV"
    , e "tabMap"              "XK8oWS7H6kyvcLyjrUVoNM9ECijZMLBmWgKgW9KgbrQ"
    , e "tabUnionWith"        "DthYgjqrhQUDeJyf65uSdsgVa7rGeab4cMk8gbFdugjJ"
    , e "tabMinKey"           "EiGqWpcSmEpsvkXH5cX5Wj7WqHjU7P371NZ3nebe1Cc8"
    , e "tabFoldlWithKey"     "11euqoYUvgSJn3FyeYYnrw6q2A1tZKd2adTYt7tuLha"
    , e "padWeld"             "Hy1FKMJeVZedhbxyHYkWK4AaVPZ4W4Z7HYcEoqACj3NH"
    , e "padCat"              "9FxKMVoHkGoV6wv98gwuWvAQtBt7qsWpsxQ86wH7nRhM"
    , e "padFlat"             "EnRMXY8LfFHFSazhqVWbBeXvnDe9mWEhPZKsiJKnJnBc"
    , e "isBar"               "KeuBtG939QzPb1sz4tN4T1DjnnKGHBsk2YGnkAKyEFH"
    , e "natBar"              "H9hURYJgsiqWisHeM1Psc3H93fcvuWB4EMfGyn3Tqa9u"
    , e "barNat"              "GBJbBKs3V8t6KaKwEMGiXwT1agrDaPhbRS2j7gCXE6Qe"
    , e "barLen"              "7psaa4r72F3HJ1EYTVwh5cC5fR2JcKPWQHgd1uJdErTj"
    , e "barIsEmpty"          "Afm989n8QzpszgC49vseHH7gkjjPLBJcd9uGqya26hWQ"
    , e "barIdx"              "CPVmSMHtWqRrcM4rMyf5PUv5vMj3ASJzTEqP8474Co32"
    , e "barWeld"             "7EXm3FCFczVKsf5gij6fVi2mAwaE9CyygMnc6QFpowW1"
    , e "barCat"              "B3w1Ag26QyMowHqA2UAKniRhkJLqKXz5BVN5zfg1rX1X"
    , e "barTake"             "4rDccbqb9YGWfWffY7gpjUP1TkXFTPFGvRij4AegFj5z"
    , e "barDrop"             "dz1W1bZiWG6UxpZVRNYrbCpaoppcSFfTxsM8NPXQt8z"
    , e "barElemIndexEnd"     "G774kEyAd6V7oBDmJ3NHMZthah3RXSA1s8XUd2cGaMtm"
    , e "barFlat"             "Bn21KXpbUJC4CgkioxCKkakcemPNdqk44XmPQ9epCdzA"
    , e "barElemIndexOff"     "89NWY8rWCvKm4YQD91kMRVRyw78BVbhFGd82y8cSucux"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "FkTwxZLxCJVBdUT1ZUAA5gZmhvNpTdXtomCV5dj8AzaJ"
    , e "w32"                 "BKLuVT8MoChTENniRZyMQh7ZPK2PEEVeCK54EVaxdDZc"
    , e "add32"               "FREMMKCJmAoFYkaqP5CfeaxFH4vT3ExxmYizg1UEmNjM"
    , e "mul32"               "3tZp438whmzHYEohBxYzoJEr5Kkeiv7euHWEC83Qr6Zh"
    , e "div32"               "5ZyAcKDqp6DyimqtX13fcSeqq4U5AxzpUKj2hNuJuU21"
    , e "and32"               "6g6rg9VgBaJQxEoib2fXxvaWH6qDe3ZzSCJdvPWM9TQx"
    , e "or32"                "A62XAsZCHiqYGFUuiqVqUAoaS5CRMJFfWZe9Dxiz6Eyh"
    , e "xor32"               "EAdELB1XoQGYqazG7tSEdjodhF2Bd8Ghgi3ng9288Arg"
    , e "lsh32"               "GnNe1p3wRb25o3hnpkxv8hMNAPG2Hh7BmsB2xnMWTmZR"
    , e "rsh32"               "HnNUFimP7XZbpvrRWCEQtJiN9m2Au1ESN6TBDZ7NqNBL"
    , e "sub32"               "7VnBCB5SPR9KeAgrZMmBHjhZvfBvXDoMpkNvdQqvaDnX"
    , e "ror32"               "4UUQsZXMUNFv4MmGLfudiA9fcW6zgTvpEfozzjjeeLTU"
    , e "rol32"               "Bi6pmd9oRLTSri6SunT2Vk8B2ukdAjvuakrrKTMNyVMw"
    , e "blake3"              "CbK5DmC289qsm6ssxEy5AxvWGZJTwdEZZSuspy35iEZ7"
    ]
