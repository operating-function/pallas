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
    , e "_If"                 "DiwZ6jrm6gs9AcVsNDvPfUoYXN1upQbDBMWcEDE66rYu"
    , e "_Not"                "5d1jtANA59DQvT3TvTkSVv3HPT4qNA3yu3iaZCqGXqQr"
    , e "_Bit"                "7ebfLaG8Q9bXrosCAd5g9UAMaHV2nsUVfDSYvtnUBakU"
    , e "_And"                "H9Qn8HEimcMwUw9UifERx8sXPrMYWB6FxENeNfqx7LYT"
    , e "_Or"                 "6bR3TKvSsoeM9WY3fwagHYNM8WRBH8HvyLuJmtUtK4xY"
    , e "dec"                 "BKm4tM8GRUdHC26Vi4NRaxVVQrHtAy49om6wjB4kkD2Y"
    , e "add"                 "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"
    , e "mul"                 "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"
    , e "sub"                 "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"
    , e "lteNat"              "HRtCMuhYa8zx7mPc7sLsV7vNBj2jfwcDpQJA3io8D5VA"
    , e "lthNat"              "UFfqirTSgN6Jm3uSLNL5kTqgc3HS3MacR7AyFG3bR29"
    , e "gteNat"              "2NtUWyQY6K6Ge7s6V13nZo3gDoBBmnyofmGHNW1jwUWY"
    , e "gthNat"              "29oM85LS1JY1Raehi3K9rJaDzhM5qt6raWQsxb2k5rDK"
    , e "eqlNat"              "HnebHagjsdtJFXGboRA2T6W1uWN4jfu5EJhLwzhxLh7j"
    , e "cmpNat"              "EaksX8C56F9ZPmRcYUSBokvrqnEF3XAGqV5BLFkXoiLj"
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
    , e "weld"                "DmCqw8C59oVAaYB2spANH3s5iARHJbT9Juz9CVMnZqx4"
    , e "map"                 "8uDZW8m3aT4wT7MDzmi3b2NrA8AXZHkBn3jHoQiFPbjD"
    , e "rev"                 "DaP3nTdpSxdtuscMNyWatsAZtqYZMeQ6XCccY6fFsq4d"
    , e "rowCons"             "DQi1YwwJqnaa89QmEpqGSUPAdzMvjz3ygoHsW5DEXzrK"
    , e "rowSnoc"             "2zV94fTAv7FxYiYWc16efCYxQhHqDEDhcqujDXdXBk2w"
    , e "sum"                 "HXE5Apzp6ZHUJUPcBnv9kGmfxPGqYniGfTDpDjzRWCbY"
    , e "cat"                 "9DDysSqqJkxYa7V25U7vs4moT7FGbiQZqGGZ7FUM7ktt"
    , e "zip"                 "6T2RKPwEsXtxiDYgRp4bFtBumnpHhDbguKkjgfC73tcn"
    , e "take"                "8YjZWsAmoKJfgBjdDvi8KcLucVGV1iwxuREhzoRQJJKy"
    , e "drop"                "287VwgrwR27LodV4n3SBnjW7hUJ51Hckbi6ToLehU9oK"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "5fTpEJKMUwgigpUfUfgaENeiJCWyG1ahq2yy8f3UjNJT"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "9zf5nYvW7znzoA4jgsVZizScjUBhcAieRRf7Hu3Mwj5g"
    , e "implode"             "CumXLE4SvknYfKEP7xpPFPZbhCZ7oBaQFekWDEF9MnRd"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "8f7r5nW2oCEfd5smGiqjKs78rZz8v4kWrM8n4EvQwRhB"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "HwzSWLDxRK32rpnypqZavAbaKWxwuk1FYTK68siRS7yU"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "12sjmJYdYeD5ULvjoHjVJVVYZfDsokjC7GWJs1oyHANW"
    , e "cabDel"              "2JC5pZoViRDLG5hmJ982ihhgrm8uPZ9reH4d54h5G6Su"
    , e "cabWeld"             "FKVPtaqCEqxnfEJZ4oxMH266tZcxinwNgAaXSQnCJqb6"
    , e "cabCatRowAsc"        "FmctenD13Nx75YMjsjh29EFtawYf2SJZW4gnL8VvHfLX"
    , e "cabDrop"             "GijstBKL1JpcUXrhCQgGJztp3aobaMWHxVrW7EaD25PM"
    , e "cabTake"             "61LkJ8BaTjNQq8GqgUc8DXNAnNM73MsjgxY5wbPm73i8"
    , e "cabSplitAt"          "6cyCapN6HcW2f9BEq3JnKzi1idnfvQEJWGrQVZGnk9i3"
    , e "cabIntersect"        "AAnWAmWLJc8kg5aKLkHWV5R5tcgz3RkfavT8mMoSoW66"
    , e "cabSub"              "GmVgHWgxM7352BmX534RANhWBM3SqzpweW6tYgSbvm1F"
    , e "cabSplitLT"          "6VzEUSgwoLjXFFUjwfFdLCvpXERy9C47mAfLVQCry966"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "D3q7Wh5ib8Keppj11MfZx1e11saDE4UjUzw25g2rJH1h"
    , e "tabIdx"              "DchAMRjZKkN7xLYNySQDeLKNjjMt99eQt4Q1eRakdNp6"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "4HJiE7FVpsk7vYpii4JN1LwFHutJ1TxTeNNtEHjmcUXN"
    , e "tabHas"              "7uq3NeFJWM8FG4dCLVrYXhpuPJfqfdiyZqHjcdaucJP"
    , e "tabLookup"           "4Sy6vmUzKmaAgBnGF8ZksbrXqsCnEEhBrLiV482sqfSt"
    , e "tabIns"              "9rULkvm4DEzJt8GNZnGriX69vxU2XoeMQTgJ5YGtKWBJ"
    , e "tabSwitch"           "G9jiYaJeWMwhCnRNQQTMcLNz6jMK5UEYpnjUmRAeNnnL"
    , e "tabToPairs"          "14iJeiwz6FmTvPb856K1z2F9EXxuDFQS2qS4zWsgLFRF"
    , e "tabFromPairs"        "bF5rEWCZS5Lw5a4LL1DGjow2DpvUQCtQwxWD74Rz2dP"
    , e "tabToPairList"       "Fvbg22PX75Awq59tjTr4ibQwSXJwpAZDeQ21yei5nVnk"
    , e "tabElemIdx"          "DkzTyRvrCuhD7pNkDcN1q1rhKStovzoLGhpZDYUPybAx"
    , e "tabSplitAt"          "39y4bq5t7dnVyQZpchpYsdGbpoDpBLVt9LFutVwFUuCj"
    , e "tabSplitLT"          "ouJhcTr39ounG6vjqaooJDgmofNH9XLWdhhGEah1E15"
    , e "tabAlter"            "GFqTavz64e8QoNaSw8d2mXPmrwcgmhQFrtSc7BxGnYYW"
    , e "tabMap"              "CoBu38cAM2MMHbMLhpuhQKnUqanKs1LZYerojoCsPzHs"
    , e "tabUnionWith"        "8pbXZuZ9yVyKXQMh8A7ApbKcACR9vEjtgy8jU4bNkWBZ"
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
    , e "barTake"             "GsmC34awRWov6X8Po3CVcGE7HR1MPQbfHZcG4Fh7aSMA"
    , e "barDrop"             "BnkrpqfdZHBQbJYsGjsifSg43rsKnk17FeGWVdWjUfEc"
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
    , e "sub32"               "67ncVLL4bbJ8YUFsTgT1XENEHhTNBkWYGSGxvEVfy78V"
    , e "ror32"               "8pdwheUfRRgQjZRSeH6DvLfTPsAMccdBR5eMRjpr7eYX"
    , e "rol32"               "HTT9qRaYQUcmRE2QWqmAgG2VpbTR1HfoLU8tmJ3Lx3wx"
    , e "blake3"              "FxpdDEg6yTGdvhRAGEseUBQ8XVBezWmgWxtndMRWo2ii"
    ]
