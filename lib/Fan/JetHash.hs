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
        , ( "switch"    , switchHash    )
        , ( "tabSwitch" , tabSwitchHash )
        , ( "idx"       , idxHash       )
        , ( "get"       , getHash       )
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
    , e "_Dec"                "4Q1rgmQEYDci7AFSvvS4NnPpx6ENc9btsEA67Qrcesuc"
    , e "_Add"                "8r4PSHT5eB44W7NmQgYc9aZ8thEAfaReAS5nP1w9mQhf"
    , e "_Mul"                "5rQ4eHtxRtwud5754UPLnQDrGLbhquvAe1M2QDGpuNya"
    , e "_Sub"                "Hz4sW9dParMqxwZX8iig5anT5o3YaEVg4ueJvmcMWPrn"
    , e "_Div"                "GQWCsN4NNSABs5xBEnUC9gqjrMxpnGxYcAnAb1yvq7bD"
    , e "_Mod"                "5RsD2t1dufWbmfXtipnrFBA1tMqf4cgJKEo9kqdzeGzh"
    , e "_Bex"                "5FTCygXoLjeaozWp3jtuBJzgVneWxRrGFdsfrzgorsc"
    , e "_Lsh"                "EAYGD3goVXutbMzLHeHDwVkrdfgcUnCXk2UTQLgauqpf"
    , e "_Rsh"                "4vEkzYh6mpQZjQ8wLtBT9Hr1aknQyk5QZZi8xyt7efJ5"
    , e "_Dis"                "Ddwn6iM8RsmAdw8Qp45XqtzhauPBpcx3tgBXizumvirE"
    , e "_Con"                "4qomzhq3NhsbnG9t8m3xmJKRPAiKNbga9qrYcqv3JpV1"
    , e "_Mix"                "GZLvG9o6B2JPFLyakw6HwzEJmo6XFCdtX4XFvS8nCLgj"
    , e "_Met"                "FicLe9SMxbgu3xgAUzkHPou82jXtdZj5UeWK8pPMuQQM"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "2RQtmT8fzx8TxbGDhYgCJctGVDXidr2iX4YqQosGpnCe"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "HGvMgqpn3pczfGea4d5Nsm4oM7z3f4bGmLvf9qouHHhq"
    , e "cmp"                 "9dS2fYh1dnNh6AwLy9NjoiPHuXY6tskW98p4QFSsm2fb"
    , e "eql"                 "637jHWFSgXPBh2Y3rhmdZoWnpBimie5u2zfBnys7CjU1"
    , e "isZero"              "B3SH7ov4SmshuSo4MEP4r3AXn3ehWVfA3MXvXns5FA8s"
    , e "neq"                 "1gAa4ZDyvXTqN5hUCgsquA9SChqcVe6HV5VmEjy7hRx"
    , e "lth"                 "EkkyMysbqLu5bZyyPBDMHq4yiVRkZpaPkZ9o961fYkWF"
    , e "lte"                 "9S1HgakGUsJfUgP4754y4qMy2La2cjHShKfEs3KeCmoN"
    , e "gth"                 "47f55uDvaw5tmPi4nGxkhmTKchrywTwL6nzQcs3BMxs3"
    , e "gte"                 "DSidbLsDRamEYDskLrMUNaFE8BbZApoQ2bP7td8QcYmQ"
    , e "weld"                "8BEdDez5X5DjJ13DyskwD5uATWq8CKwF7Huo2DGyaY2m"
    , e "map"                 "FreNhEFexGQpqwPhZedgnid4x5tsMfZc19rAm7HfwMbS"
    , e "rev"                 "3vLVUyNnoLzXTZmsbHPGTpbMY47rb3T63rbdJ22YQBmV"
    , e "rowCons"             "ZjRpRAsqGx8SeVhzNi6ux4DPNmHQkBYMMDJNtYpiGza"
    , e "rowSnoc"             "Fdwha9hhqp6UkjKS7xLXMBTgqFK9RCCNwGEDMdtjq1UT"
    , e "sum"                 "32nrm5id34EXajTShFbPe2rdexNV7NbQQocHXv5fiDgs"
    , e "cat"                 "EVyh6iGX2YBYHy4vC7kfwi18r1Zowfr9v5SsgWsrph5x"
    , e "zip"                 "45coaX2JWWc3a237fiBbYj1ydu7Hxbbu4ouuGLXZn13m"
    , e "take"                "8W2pejoPXCsvLkPGseCCsmRMpDudmJkbzvF3pshH4r5i"
    , e "drop"                "GNg3uUnojDGdXEm5qucnsedRjJHtVJQu4HmhmihaizJR"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "EofbLQKeZMEGoCm4YpftoPoVm6jurGCqRXz9DHkwbDeL"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "9wEuLAoXimBFVYtEFrRAAs1hX7z361kfYGEBwvfuRGkE"
    , e "implode"             "6jd1wtaQ9Lm6RoTSFj55H2W7je798mKT6ZrpcReYevyB"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "EdVAfNH3QtT83GerVRosBsGnJrtsCzkoA1Zyo8Zqzbwp"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "5AE1cwsQkYUQbRkhjmXCocqi1UumUUaq2kUNHu61LVtK"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "7VA9BuPudEGt8hofiSDPaiJNVKjUjP8Xf3YUpGhrJ53C"
    , e "cabDel"              "4R7mQxivkJrviUpdG5oc4BrLCQWog7w2WS5xRGPqHT7r"
    , e "cabWeld"             "AGw3SBoZYi8Ey538oLoNkYCKYAY11VgMkXmuBbXNAtQq"
    , e "cabCatRowAsc"        "EEZ6sHLvHFpKKkQkJysT34988YsC4c6982Wtekbf7hyk"
    , e "cabDrop"             "J2ksC4v4ErFgQzHxwekRe6w5YbJZL9Y7ZBqi1FrvNK2G"
    , e "cabTake"             "BUSewobSrjrkDhE9CGNAGdMhnb4QMASHsJckqrSFT5ce"
    , e "cabSplitAt"          "DfGxGmi7fDZrDDxZQ77zgF5fWZMvQpYypeSkZx9W4Vj8"
    , e "cabIntersect"        "FnbUcy375NXib7xXsYEFgDEG59u1ZxyzoD9soUJxTpTn"
    , e "cabSub"              "7bmU8g6xu6y9vvw43dAd7jEu6z2buM3rniiQ27zjYXeq"
    , e "cabSplitLT"          "9AqdvFDYUtSXqnhf3ZcdPeJb549BohomDpHkM8j3NGVC"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "37UvY7QoNW5fEfW6hSzb2Lh6M4M74j3R71fZ7t2eC5Mk"
    , e "tabIdx"              "3CK6pnfQqn9XtaoJmkVRqNxpxBBFcvik5vmHx36SJAQh"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "b9Y25Z29g31CEtfbdFSMARkJveqwWaA4RjVbdNcMw54"
    , e "tabHas"              "9ksNBnV7WWw2YQDTY3M7CFhp6xDYjDmunE8t4QhigqF3"
    , e "tabLookup"           "2DCWhR3Ywh1z5HYEDR3JcR4Vrui5Un5maBCTAgbuWHDG"
    , e "tabIns"              "A9LChicnDxyzNsSfgFNo7JJk7Jvi1Qtv4AX6MR1HDMoB"
    , e "tabSwitch"           "CjoGLcAostaNSzypY5G3gYgTCeVTEg4PebB1k2E9HNor"
    , e "tabToPairs"          "2Njs4B4FrjBgCk75iMGcRLczbzwbc2X4FKzmCBpj4iuQ"
    , e "tabFromPairs"        "Hn7mkFGwaEjweiazHsqJibG98Ukhnruc6FeYvnt5kafE"
    , e "tabToPairList"       "6p3QRWeSY9ezkvvAgVZcCTuvmUPBjmdaG8ybb4SLpVNM"
    , e "tabElemIdx"          "3ZGSgK7u5cWRv5iQLP2mx17p1VMNKrzRpzzpppVnTSqZ"
    , e "tabSplitAt"          "84W6YM2pRKuUY36ZUzQ1aQTQPAQw3uWWEZho6v6JjPuh"
    , e "tabSplitLT"          "FWdHhDT7FdKWokfJpXFBzLWf82PA46kWMctVPnm3MeNh"
    , e "tabAlter"            "Ek26FwUw1o7Z5Bb3c921ZUKCJJm2indk5Kk3E2BgcpCy"
    , e "tabMap"              "GEjCb8ndhhEgZ8xjssLy4GrvxXqsTnuhEmGJkZv1SwNR"
    , e "tabUnionWith"        "4iirDLKwEH9gp6GLYn9v33wy3CMF9n9WiowDBoXvKe4i"
    , e "tabMinKey"           "6z6YpxAzb2inWq4eccphFiSLcm4M2BDQjY1hYyweMT7a"
    , e "tabFoldlWithKey"     "Ea5YTdxvh4G15xFuGt8rNwYh4vsD7rHnXVBPPxkHU4aM"
    , e "padWeld"             "Cu2Y8upx3xHZarLpiVUt3NMtcw7bjp4WiELWgzAyD1Hx"
    , e "padCat"              "EnTHH7voZawzz3ry82RGXXfmicRW2y7DevJhHmPeFXTx"
    , e "padFlat"             "8vfpok7gqDmXNJqG14KGw2EMe9Z5xt1tCXaoxXxrMuEw"
    , e "isBar"               "D8xw5AmXSnUzPagy45DRKGQ3cTE4JBEd9ASbYoRR2RHt"
    , e "natBar"              "CgyESYRwvBgaQrNvJBwCghLAAZBuyzieDrvyCPYDapmq"
    , e "barNat"              "5yTUnDnv6f31Jp6ep113dmBcfF2V8qT31Umb4jkGq56Y"
    , e "barLen"              "56p7B3gfHoHuAGwKhdacgNqMDpR4gMiJ7DfQcZvUNwoH"
    , e "barIsEmpty"          "5S7mPBX6Hm8QNnzgRMjNnfE7U68wGwKtMsvTZtrxKNXs"
    , e "barIdx"              "Ex3TVhMafwdzE69icEr8jAgavjg6vbVPM2cFuoGCqTyv"
    , e "barWeld"             "GXCKRpCHfnnH4jXLaG6XeMgNLiwcGJg9ycD3K3AA9mEm"
    , e "barCat"              "3dque7JJzyQTmkT387yETX8zTHoZcRwungHSk8ZyYqAR"
    , e "barTake"             "GBQsEKoqTEDkvjVXT2udiSkf63qjxi1renDJUh2f7kz8"
    , e "barDrop"             "BkTZ3X8vKLCzzLUsEurDJZhRxA1b36DehZCMyoKwRqoK"
    , e "barElemIndexEnd"     "3t5aCvcfdk9jHD7QcVMEq6TW1fnmVure3Tj64hE1t4KL"
    , e "barFlat"             "FXwRAro4kMHVay99L1HzNP2Sqp2LuHPjVMTMUuJGgJXY"
    , e "barElemIndexOff"     "7wvNt4UXuX4LytrTmMwZewzzFffY3Mbx51TQ4rEUnK9x"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "FkTwxZLxCJVBdUT1ZUAA5gZmhvNpTdXtomCV5dj8AzaJ"
    , e "w32"                 "9xM9h41JkGBFfToVejHFc5yDDy6SNZRAGH71QyveN6eZ"
    , e "add32"               "2ba39JeTeVBgmbwRKCD43ewYx3FbyLYnDTcdcUhTU9He"
    , e "mul32"               "FYDqmVFZvn2SqeTgsAx4CWf6Ywo9zzbkCVmFx6xU3Mpn"
    , e "div32"               "5iZxzUecKNUQdz4ztG2ddbiotVjyBeB3q6tRko77rJmH"
    , e "and32"               "FBAKiYwPSDt2zTT9fETibt1bLe3aCR133aqo2Rz6FRGu"
    , e "or32"                "CkPHRvNxtSjdTuD1AHuJNG4mHKbNr3LuThGwrFpBvVSd"
    , e "xor32"               "4AADLmJb7YXyxc3hoyfxGWeCCM6Zaar9UYg8iEMF2xSj"
    , e "lsh32"               "2KQyvkvgSk7ePtmxNStBFVGjqEavcvu1s5pompUMLLQC"
    , e "rsh32"               "CTbr2neVrHdHMSUBwsiaKx99zcWGgSYA7DBf8eheZGMk"
    , e "sub32"               "8xTQhzbXjhYqtE7CM3cFeumEnnPQy4CBYVnHxCc57AYP"
    , e "ror32"               "DSWu1ryTBuQbUEJctWaeAxEjjRzPXCK4Njwbvugyf9B8"
    , e "rol32"               "FKuEdHWTGdvmR3dq6xtr5zi9q5PPJYNQ9Zff5D9iKQvT"
    , e "blake3"              "6G2jgs61mg3vCHzN3m7u9ya7xLKfHH9ucaHNXvTQiqP6"
    ]
