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
    , e "div"                 "2ie29trqqTDUEsnWuahusdbb7D2dSXNVbXWqQif4WFGn"
    , e "mod"                 "7SZAbZusaBvQ92BaJufsg6ZFXFJoG5LnY4g5DnWc1itU"
    , e "bex"                 "DdmJnDFakbPhNoNnQuYRarPXMt3aaej5VLarUaja1PE6"
    , e "lsh"                 "27LCe5bqqWsF8CZBSYvHHK5wirqPueTbtJpaFiYdM4da"
    , e "rsh"                 "4AfNE6pJC3cjxq8vHdP1tNaePMTCNhbx9Y3FDTQTxrDZ"
    , e "met"                 "2WPxVZkeSSr9W3G7bmH6QTDQNM75xZCsK2f7N2y95r1C"
    , e "isZero"              "6kBCQYbaiU2j9NPEfo8syFDjeqHjiGgiGb8NN76nYeij"
    , e "dis"                 "5yz3PJBa3FWY85Gt1F3rAXvcAHZi9TJM8rHqsdLmNMX6"
    , e "con"                 "3JijyQqDU7JsZnyPYBPG5rxSVBQvVpEndQ5YD25tWEP4"
    , e "mix"                 "ASa4uYvowsWJW7TZU41uZX6LTpmMCR9y7yZWhDWH5uuF"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "8kard9QcyRbTELMjoC2Cwwekoe6ZEEir1pRjydMEZwX2"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "88r6vmsqcGdd8ewJ8KArHKtZeyQMKkRojHm7LsHM7bnB"
    , e "cmp"                 "Fz4UJGs1o36emDx9XeJf93VNN2RKUjLTKoXN4J6QofjZ"
    , e "eql"                 "3C4qgiC3peyCXFTB73Q2oSugC7XnrE3fBA54FhZr8rxc"
    , e "neq"                 "BR8zoHaDGCX3db7wJiDrq7aih1WbJ412n4ozwuW5gvD"
    , e "lth"                 "6HkyJnXVWywrjLEtSQSZkJV68R7X6gWpvR9pepkyZmR6"
    , e "lte"                 "65E7Af6LaiBbQJR5bt9zUf29Jhys5jXhEwspq8GwbgzD"
    , e "gth"                 "CN5kzfrV89d3ZgHUatog81YWKuHzeSAzXGDet9ahUtmz"
    , e "gte"                 "AKRLJRRRkS9fyfTXEH9DbMJRnANX7SsSztjMXFJfwyS7"
    , e "weld"                "eX89cPtvm6ib6hJcRzJHSN5xFta5AkE1cRVfRmg7tee"
    , e "map"                 "8uDZW8m3aT4wT7MDzmi3b2NrA8AXZHkBn3jHoQiFPbjD"
    , e "rev"                 "DaP3nTdpSxdtuscMNyWatsAZtqYZMeQ6XCccY6fFsq4d"
    , e "rowCons"             "9RyEKreJK6Dey47Qj42KnD7SA8Hdym1TMfsRW4JMVHGg"
    , e "rowSnoc"             "DooV4xPufuCXw4NzZs6ATUpMk4H89Zje8a8owrbFTyqs"
    , e "sum"                 "HXE5Apzp6ZHUJUPcBnv9kGmfxPGqYniGfTDpDjzRWCbY"
    , e "cat"                 "C8Dy5nRLieJD3h9bKp3i8HtmQVUNQAz7sU331GnG8ybN"
    , e "zip"                 "3bRA8UiUp9NyZyXTRgbBzP12m9n6SmF8QWJThaeBdEsw"
    , e "take"                "AvvBEG1c4MFe72vTetAg2PRJcCEsq7FdC7XV4sfqQcfA"
    , e "drop"                "287VwgrwR27LodV4n3SBnjW7hUJ51Hckbi6ToLehU9oK"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "9t4t4RauFafY6tnEsApH72B2BKNzvcwCkX9CKd6W217U"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "2dTMGMVvWXm1pJNW2FE94joeLxqunkedEJmSqvadZPAC"
    , e "implode"             "CumXLE4SvknYfKEP7xpPFPZbhCZ7oBaQFekWDEF9MnRd"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "8f7r5nW2oCEfd5smGiqjKs78rZz8v4kWrM8n4EvQwRhB"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "HwzSWLDxRK32rpnypqZavAbaKWxwuk1FYTK68siRS7yU"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "a9kC8NRPhXCGh1xMQzW1Wpqs3X4VXGUTgLGHLK2CKoq"
    , e "cabDel"              "2JC5pZoViRDLG5hmJ982ihhgrm8uPZ9reH4d54h5G6Su"
    , e "cabWeld"             "2jcf2ZgUcRBvHfArWn8ATCzPDcJqauQmwc7u7nJuyARt"
    , e "cabCatRowAsc"        "B9DN2bkVTyUnMvjUJckpgwdvRjPZi2a94MEQHs6TTNbZ"
    , e "cabDrop"             "GijstBKL1JpcUXrhCQgGJztp3aobaMWHxVrW7EaD25PM"
    , e "cabTake"             "CCGXA3qCWMKu1JnPFgYreGgstaZkFWgnXfa3rPNpfxji"
    , e "cabSplitAt"          "gr8rdkg4dckFSBb2fCdjhAZG2F5nUe1xiHrPBsF3dvn"
    , e "cabIntersect"        "AAnWAmWLJc8kg5aKLkHWV5R5tcgz3RkfavT8mMoSoW66"
    , e "cabSub"              "GmVgHWgxM7352BmX534RANhWBM3SqzpweW6tYgSbvm1F"
    , e "cabSplitLT"          "ANt1KjU69Ym1A1kXCrj9yVdPg45YuWxeYHxJf3hcfYqv"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "E2JkkxzAnVcUW73ZnBWhjfriiak62y9iKjz6HtRsUNnL"
    , e "tabIdx"              "DchAMRjZKkN7xLYNySQDeLKNjjMt99eQt4Q1eRakdNp6"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "4HJiE7FVpsk7vYpii4JN1LwFHutJ1TxTeNNtEHjmcUXN"
    , e "tabHas"              "7uq3NeFJWM8FG4dCLVrYXhpuPJfqfdiyZqHjcdaucJP"
    , e "tabLookup"           "4Sy6vmUzKmaAgBnGF8ZksbrXqsCnEEhBrLiV482sqfSt"
    , e "tabIns"              "EoWbU28Lw24L7NPWPHNSW59bVyEnyRXn4UwTGetaLbwS"
    , e "tabSwitch"           "G9jiYaJeWMwhCnRNQQTMcLNz6jMK5UEYpnjUmRAeNnnL"
    , e "tabToPairs"          "14iJeiwz6FmTvPb856K1z2F9EXxuDFQS2qS4zWsgLFRF"
    , e "tabFromPairs"        "DCUVw6AmV4zRiMxaphwRaXz5EymZeTmU5TAaBsn5h7MD"
    , e "tabToPairList"       "Fvbg22PX75Awq59tjTr4ibQwSXJwpAZDeQ21yei5nVnk"
    , e "tabElemIdx"          "DkzTyRvrCuhD7pNkDcN1q1rhKStovzoLGhpZDYUPybAx"
    , e "tabSplitAt"          "FG94LzZEeJznhzuWZ5QZDr6K6CYnr13hii4YkNu4662A"
    , e "tabSplitLT"          "8iSvyb5doMfiyNhVEkWwRPsFs9dtptXKoVR1qpXEurYN"
    , e "tabAlter"            "9QZwRoV3FX2Z2gq9YjZqC5nTZN3o8McmVTJfzTUR2fQV"
    , e "tabMap"              "7pjWvKdnczaj3HWXtJjBG2yKofEQ42zQt1Sq7NGMMfDk"
    , e "tabUnionWith"        "5QPXa83ukh5skCcVyAitR2BQmgF7wq3TyfFwpAixCzv2"
    , e "tabMinKey"           "EiGqWpcSmEpsvkXH5cX5Wj7WqHjU7P371NZ3nebe1Cc8"
    , e "tabFoldlWithKey"     "11euqoYUvgSJn3FyeYYnrw6q2A1tZKd2adTYt7tuLha"
    , e "padWeld"             "CEoGCG2su3jipMEQwFyG2mkFuF2uxEXekhvv1RFjzc33"
    , e "padCat"              "BTQHa8rfhFm9LYtTEazsBLHCVKkB3FhLqbnqdGFieafJ"
    , e "padFlat"             "7mXwNuTd98K89eqtFz63uRTE3odEhU3v6pCpALoCn5WP"
    , e "isBar"               "EgYgxV4gtR5VEGf62s95hh9boSxXkW5yX2UNsFZSDMH"
    , e "natBar"              "BfqTrWF21B2yancetfD1sAbEbWDxVaf8sTJW3XJJGht1"
    , e "barNat"              "FNV1tYeQV7qiwyrSTzGhXkUCLAfPXSWviBeASd4agxVB"
    , e "barLen"              "BkcJwNSukSPAj2ZftqnRW13sqP1padxqLyfiMZ7sVKTM"
    , e "barIsEmpty"          "CWrc6PBvyiGEQfvdRpKUbtfudBB7QCdNA8ognRpoUnHX"
    , e "barIdx"              "9wWBp35tvBSnBU6HCRtJ7iJZ5EpNVbw5wt742oPUVdPx"
    , e "barWeld"             "9r8fzyyxZmweLndNcEBtpCmqHPU56tLDVuSiTKmggsku"
    , e "barCat"              "6bq17zMZ34HFBDd2yeaiv7LwsVnx5Z3M2mPEFaxD4kjR"
    , e "barTake"             "Daaj9arYfxuLfFzFexgaFjUveZ5Qr6VqHiHtGTTN49Rr"
    , e "barDrop"             "F7gkUcnzoz4QDA1JixsqeyFdHxWrvJ8vVy5hUGMmg6j8"
    , e "barElemIndexEnd"     "HParruaVwWam8SiWXuajHhXiHTVR8yrYXTNzbgwVKSLz"
    , e "barFlat"             "Gu4gbpZjjXMwJZ36REEpqmzFyqUHM4Uxg5s3vmEwKK3y"
    , e "barElemIndexOff"     "7AUvojBr6UDbCqPqYctDHmpF4w9TGkWqar6oVhHtRQ36"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "FkTwxZLxCJVBdUT1ZUAA5gZmhvNpTdXtomCV5dj8AzaJ"
    , e "w32"                 "453DdWJKYgkzuypxxqr4CCr415EFBVp3WrqZ8Z3w1qz3"
    , e "add32"               "HmdyAhoxaFzFRsoQohyYwhhumwKgkg4tsDUEtFff8oeZ"
    , e "mul32"               "GHg7MVpGVxTS3tNsmob1RGNsGmhrGWCBhXYWdHnB3tjC"
    , e "div32"               "LewtiHV5Qb5NrZa4wW8GkUA87WCGQMiD2bn3qnHzu19"
    , e "and32"               "FJb5vwigtDuujYE4VdzsNz2xhxzCwdPaqzp67cTwqx72"
    , e "or32"                "65YsSS3WV3oFdY6dPEYGkE2FUPUUAFy1ApF2U7UH6jD9"
    , e "xor32"               "Ary4s1An1Su6erNFYNqRkWNRojoxxSsRQustLEBjPn77"
    , e "lsh32"               "6CfvzUMX5Lt3J8WBRMNG7H6YouW49td7kvH5pCABQfDh"
    , e "rsh32"               "14napWAGTsVf7HeEihLdzHEgi5VvcwCi186huaNYzVTo"
    , e "sub32"               "5MWy2WDUrgvrcJ3RWif8uEXtEj9VVTCPspZQrYCLGR61"
    , e "ror32"               "8NCtV96stiVtG8LMatjx7sto7bUhMEqn2Y7LbhvDWDAi"
    , e "rol32"               "2XS29TWA9sCN3ryxNiH4HMPyvDe96JzH1feyQJUWkJ2g"
    , e "blake3"              "Hno2qG651WavaPaAfFPHup6tAjgjDhf2YutnGEzypNK6"
    ]
