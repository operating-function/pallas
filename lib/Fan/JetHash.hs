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
    , e "_Dec"                "CeUYZzpJkr2h1VR8jmRvm5F1K4ZWgVfPKBo4heaVA1ci"
    , e "_Add"                "8r4PSHT5eB44W7NmQgYc9aZ8thEAfaReAS5nP1w9mQhf"
    , e "_Mul"                "5rQ4eHtxRtwud5754UPLnQDrGLbhquvAe1M2QDGpuNya"
    , e "_Sub"                "GZqurSgma16NoHgM1aAhXWzYigxvgHFKhvzLLgEAmGHZ"
    , e "_Div"                "HFVYF4AZbhgXyZrFUKStjioWSXTZoRUzDu1A5yDk7ZEw"
    , e "_Mod"                "8J5QHKx2rTg7TPqLbcRsXzRPrPLDyCFAs9oZSCB2SG5H"
    , e "_Bex"                "5FTCygXoLjeaozWp3jtuBJzgVneWxRrGFdsfrzgorsc"
    , e "_Lsh"                "EAYGD3goVXutbMzLHeHDwVkrdfgcUnCXk2UTQLgauqpf"
    , e "_Rsh"                "BM822cesJNRJmJzkVZWdqNW7ksdBAH7FqNHACi3ncegF"
    , e "_Dis"                "3FnhdjCwf2DF2Tz3LjtdGxSgNRqQmjCSJG4fKa1ikQKT"
    , e "_Con"                "HtwJry896dVqYQ8YqvyrfNPDJT5mZRDUrMHfRLU6dUJh"
    , e "_Mix"                "FGmyagTzHKk4LbzPnfkDSE3XzjuveAMCUz2C49kVVjA5"
    , e "_Met"                "BEi8krApskKrk83zEL8dQeB8Ljvj7QgbchpnTaz9kf1j"
    , e "cmp"                 "HtHU4X7itru8i8sXXMUsQoKDRsjaFnL5ho2GP9x32CRm"
    , e "eql"                 "B8kkVQs3WAevZoSej9bC1kbb2KatZfHQQeb2fmjbwZn6"
    , e "isZero"              "BqUuviz1sX8sPMSb7Phbrhe9mh34Bo6RgkSvvREpjP7W"
    , e "neq"                 "F8CPU9y2hF87jsyYFXMmWoobdtm24xDgm56xiymshD4q"
    , e "lth"                 "HS8oh3yzZFLAvkRzML4gjEsznyyunHY1q7jwsZogp8U1"
    , e "lte"                 "yX4Nv4huuKbVtXQCKw8W3ngQjK8ZfZZc1SRufu9xV7Z"
    , e "gth"                 "CNGosKDH9Hohw6GGak2U1xse92HFrt72GyAL65s81RXU"
    , e "gte"                 "A2cXNiWNDfkXoHdsSAAmTWgy3NtVZ47jEpXbwV9ERVa7"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "C3qXpf8mZYmTWhuTpHLPfWUB9bpiceNotpToCaMaMWTD"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "9aWrvicQoQzh2H8F86TJBQuAn8UQoCZmy416ZAqXZ5Dr"
    , e "weld"                "6ACaQN7Bvs1uHUSMPtaq5gWSAYpQC4VaT27mHEk9qdxr"
    , e "map"                 "3D8EJbVg7gzmESKnvqmG8c9CTTdUnwGRrkAyoByMm7bE"
    , e "rev"                 "CSkJ8Z66YwG25kDqweCKcDip6WXXDsNzQqNij2qYph17"
    , e "rowCons"             "FFYd2fGevgkt1FSXg8Yji16QjDkoEgxLP1bpNQ93VW3i"
    , e "rowSnoc"             "2Rmpx7zAjJoGGEveQUfPEbiHYucUNhjNo6ymvMpWMJyK"
    , e "sum"                 "78Ub72jGoGoF3rDk3RbEWWMGvXqqC8qw2HPpM3k5Wc7G"
    , e "cat"                 "AkXTfVfZbRVwusyUWLvkeJRzkFx4kkTY8QDzFEvUESME"
    , e "zip"                 "GaeZkXeSSteTdMNDkN9kXXWVeCpYasdJT6EBog3XvjUX"
    , e "take"                "6GfCu1EPstZssHmtrLUPTFdpWD43K4PV86yeABdh9VyC"
    , e "drop"                "CKskM9YyCDnb3LQwEHL2zTc7RpfqZzTXd25fkqX6JLPt"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "2UpVopQZYDkkqnjLargUm3A8rQKvtkGt4fzAyJYAiNLJ"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "49fhDWFj1Cur58jWrG9ZU9eu7dyHtDxNZ7XwpJSkuqhk"
    , e "implode"             "CqpTp3XCmgfeJE2s11edw99wGP3NNAYymD6VXiKQBUX6"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "Fu3szv5SAJaQnRxQwy5Qwus3Z2ZL1gUU7EJTBWX1iCbd"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "EKM47UUb8Kqrxr484ho4nLWSRhfyAua9wM2iqcAhA41C"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "CyqQELuSR9yroxQjMweMSdQayS816pS4rqa4j3M8Yhwh"
    , e "cabDel"              "Huk9kLHLpPUoYogrSxJrhsSvfcNtLygevbNU9kATy99v"
    , e "cabWeld"             "BWs2uTYewfyjTbecMiwhp2EsetPtDeBFEEZuzx1NMKyV"
    , e "cabCatRowAsc"        "2jS7FTyJEsVMQkLEyz16J7rENee5ow9xhXBWrwrh4Bj8"
    , e "cabDrop"             "4MzY1Hk3gtrpNqN6om4gQAs9RM2fdzL7etweRa3odtJn"
    , e "cabTake"             "CL71wVmy9841vexmpB3PZsrt65iXQmm73Woym56EY6xm"
    , e "cabSplitAt"          "E1NHmEMiaKXAAH8o7bY6TTvMmMSmaDN5fLLZq7af23hE"
    , e "cabIntersect"        "EyA3a7Esp9vgHiQ5hhmmbP8y2yRfyTKNDgoSc6ZPyAbN"
    , e "cabSub"              "E4UDoLvnsCXyqeSYedvP1WRRdn3p9N3tny7RLQYgHbma"
    , e "cabSplitLT"          "9UkbW93USC1wwyScmoHdPwBcvBFZahYps3dj4mTphzqW"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "CvmQeLNeMqV9UykPdADhWUk1jeSFSh8Hy5aeR3YMSmyy"
    , e "tabIdx"              "GhGkVWLy3QF8Q7WrM8sSiSEF7pz21wkhWNPVXb1Qakxt"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "4zYZsdYPsWQvbMKiFQ1HEXXppYEQWeshGL4C5j9AYxK3"
    , e "tabHas"              "2VP1CoKFjnBmmynYZJyunsr8xuqnHRJgKQqrdSZH273z"
    , e "tabLookup"           "6kC5wtKaLm3uLMfPXjzgEaqSH4sz1Q9LL12cP5CYU2eN"
    , e "tabIns"              "7C6Riy5fS37VsCwfGtTCpbCgcADrh5K8y4FXmJQRpYX2"
    , e "tabSwitch"           "VBeBGpYvsvzJKjpG2MA4JEjj5yxJrTCn1yEE3LQf6a7"
    , e "tabToPairs"          "A7G9jB1YNQx3vaSDicYyB4XzpRvuoYGstH13zXx4NiyL"
    , e "tabFromPairs"        "4VrHzUGwBeEJgtw9ESgZprtFFa5Qg9j9ApEyx5HoS3vz"
    , e "tabToPairList"       "F5gxxs4CcwDi5pDNGWYKBn4yDLgFBUdP57yFyoxWtNAo"
    , e "tabElemIdx"          "93nyADUrANFqyopvwnJ2J7UJtsrUV87STHJZwtvwqLrZ"
    , e "tabSplitAt"          "82oBJUwwFm2qhhKoFdKNAe5kDTMQiXDQ3huEotP9Z9Yr"
    , e "tabSplitLT"          "6WMQ1VLJymmwT1x1LYHHE2mtPte9efM8vuNSSztWPMFh"
    , e "tabAlter"            "7mXDnsfAENpFZZ5iaxDtNLJwb6VJQUSfx582raVrv2cS"
    , e "tabMap"              "D1jcuo6HSG8QwpE2WVYHA4FaSot244MMgMZzLLytP5Li"
    , e "tabUnionWith"        "8SoQsRHKTNv8s1qPEU3uF5oW6WiaVkCLcPafrAnCaneq"
    , e "tabMinKey"           "GHc7LYjdHEmJMNfBS65CFtRakKY5DMQswEx428H1uV8H"
    , e "tabFoldlWithKey"     "HeVZUktvsrpmw4UamDw6UVUeuhhaehWKZZ9ARfDpQWXC"
    , e "padWeld"             "7MZnYrEPYprcB2CUFxULKQSbq15rQjQSynpgHb2W8uVH"
    , e "padCat"              "9xJkv45HSD9vYyutCE1RqmD5D3Na3eTKLzXK6z6FY8PQ"
    , e "padFlat"             "dGRUUZ8q9jAHc5dL2V5qFSLAGyTQwF8CsCR2xEqPc3N"
    , e "isBar"               "64oH2dhw9c3FTwh6t5vxNeYYWMH8hXQzYqFUEUxmTuRg"
    , e "natBar"              "P4wZc5m5GX4NzDd3p992mXVwjJ5R13nWqK6o1oo47nC"
    , e "barNat"              "HfUArXzMPSVqXeMSsWWPEEyPrJ6QvotDf94BzoUukT8s"
    , e "barLen"              "DB1MZXakraoYPoWbeHVEKoB5S9K6Sh81Sk1PRcA2XJ1X"
    , e "barIsEmpty"          "Cc7krHUqwVCfVGvPJtkg63TCot73oBtCg1j1daSwJ3HE"
    , e "barIdx"              "AsVdpx1XgYzS3hFmsvfN5kzZ8YZaB9gSgAh9DkL7boEA"
    , e "barWeld"             "8HbGTT8BaZKZP9wryq6NaqPwKvXerUcZ9n2CxKv2e4uN"
    , e "barCat"              "3iQebSfgwoUzhDE6Rqs8is31DCybApSCZ3P7BSgcqU1E"
    , e "barTake"             "GtSLnq3zey9Z8ai5ie9CfQL96zGd4ULtLk3URS9HCZhx"
    , e "barDrop"             "C6RJH9vJqWxr5Lxyvy7gqN9xpGkJenjA9jdhuzBTEnoH"
    , e "barElemIndexEnd"     "7RtLXLxdc2xxPTRZWvCorQ4UB684x8hSqpaqTvs2hts"
    , e "barFlat"             "2ozr6zmL1JgmNwshRAD1DQLESFnz1Wk3jaX9jeaxcH45"
    , e "barElemIndexOff"     "3SsqskSfKK7vJS9aiC3U8ZzwyvbtD2z7d6iho4s7Zbs7"
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
    , e "sub32"               "657EXFN9tsL933fPi8ytSysErjt45P1zkugvppUToges"
    , e "ror32"               "836a5aHzwMGbnZzMTdJkNtpM5vMBjyqoPVG9BGvHiVPh"
    , e "rol32"               "9kipafPYNhjrPcEymx2FdQQFk2h6tbiPbGTKfasztcfR"
    , e "blake3"              "4CGykNa2cUrDyjxqbz86R9sMEmEz61AyWbcdraaLWmTP"
    ]
