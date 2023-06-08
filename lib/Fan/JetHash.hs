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
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "C3qXpf8mZYmTWhuTpHLPfWUB9bpiceNotpToCaMaMWTD"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "9aWrvicQoQzh2H8F86TJBQuAn8UQoCZmy416ZAqXZ5Dr"
    , e "weld"                "CMQVNJkrtXP79iZxZBtSGnWfpWsdp1paowxF5KL5iXUu"
    , e "map"                 "3D8EJbVg7gzmESKnvqmG8c9CTTdUnwGRrkAyoByMm7bE"
    , e "rev"                 "CSkJ8Z66YwG25kDqweCKcDip6WXXDsNzQqNij2qYph17"
    , e "rowCons"             "B4Gx5Msc5ydLFZsYVMCXSEVcEMQoQG5UwnketUdKHLcy"
    , e "rowSnoc"             "BqiDEEKv85qFGrM2YcuGf3o8J8ZdHHDJh3zjEYxUcjRb"
    , e "sum"                 "78Ub72jGoGoF3rDk3RbEWWMGvXqqC8qw2HPpM3k5Wc7G"
    , e "cat"                 "4rQ9CfWh4eczyRRMsnNGRZqCv7sBZyZ5xuMeo4cAdDoG"
    , e "zip"                 "4Ttueo1Bz3XcokrCozCrV7yebmNSU6sRRNJNuJjYHpVS"
    , e "take"                "BNzZm5oqwAYhubrfgiwREazzxd6efWa6W6d54evykeWk"
    , e "drop"                "CKskM9YyCDnb3LQwEHL2zTc7RpfqZzTXd25fkqX6JLPt"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "6DKJViv9XnbgKHBASSsbWdZaJrUd1oSCbuUBztJegyNo"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "GWxivSETStPa86suK4SEGw4MnXKnRHdo2QcqJ9ggFtPb"
    , e "implode"             "CqpTp3XCmgfeJE2s11edw99wGP3NNAYymD6VXiKQBUX6"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "DhCmEsWNZGVX3fTdoQdrFnibANSm5WYpB7FZ7PeDqNbg"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "JAd6tyxitqQizWZ5cNPeEuM9Rore94NanHRzTYKZfSdz"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "DuVF2VGc8gD4zJZ6PjKg4otTCk2Bo3hAbsvsHLHHPQJo"
    , e "cabDel"              "47LJSwNzTPWbLRApaySzV9mgfFQhDMTYkUqwvu6TFK1M"
    , e "cabWeld"             "4k2tAXKMeWEum5yJwENCPxDwGPQgw97sUxJ6xeqH4QXu"
    , e "cabCatRowAsc"        "BP1sZ9ZZPovHbm2A3ERAzf3GrFLAaY4kMpYSX9F2gC2b"
    , e "cabDrop"             "4MzY1Hk3gtrpNqN6om4gQAs9RM2fdzL7etweRa3odtJn"
    , e "cabTake"             "HEnpLFXvGM5NxKDXRTwxMgKpn7QSJs5n86ie1GP79QPS"
    , e "cabSplitAt"          "9pkfnUS8coDsKdk3Q3EKiFjpJb7DtvEwcshRf7csdDeo"
    , e "cabIntersect"        "S5upfMgiL4pqHrtjppC25nNpgJ93J4pq8GNSUz6mAXz"
    , e "cabSub"              "324gtgLeGdQU6JCCCrUq9tmnZt5kZMoMXfEBbfzYqJ91"
    , e "cabSplitLT"          "FwktYnjYRWUs4WRD2929nDQ7fBqh74TgNmDmfz8PpsMb"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "9eqkF5T528dTS6fiyArLq2BH8b8tciCJReb58opsgzX1"
    , e "tabIdx"              "47NmYuRq6tBngqj5LQ84eA7Xcuzdj866xf85J6qGoFL1"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "AdvEkKTRcjC95iiDVpZa2mTksh2WWMtarVKkfgBQbHVP"
    , e "tabHas"              "BxETtadGswUdBoaRntnoRCqUnXE16W7un5NjQCLmb4LW"
    , e "tabLookup"           "AXX3uNURDkisRopdskTcfST5pV1NCEcSEnUDJBDyHNtj"
    , e "tabIns"              "GEpft43RW9P1h7muoatkAH5HkGAQfwYL42mp4b4GsUQ8"
    , e "tabSwitch"           "23rjYDQU4Ai4hjE3vE4KkPV3SK4NFNRew3TyuQg8S6R4"
    , e "tabToPairs"          "A7G9jB1YNQx3vaSDicYyB4XzpRvuoYGstH13zXx4NiyL"
    , e "tabFromPairs"        "4g8ysVUq3PToTwDUtRuuxUiNoouC2CiM5KSKEBYiBjyj"
    , e "tabToPairList"       "5rvnDTK82PuagXpgB2RQX5Uze4ZFUVvXUng2Vj8BJHjc"
    , e "tabElemIdx"          "AE8phKeCfzMCZU5ciMitpQdC63ZXYAsM9GNqNro9XCMq"
    , e "tabSplitAt"          "Co44Dbj87vsP3xRseb3SkJihSe33SvZZF9nDbQv6YKcc"
    , e "tabSplitLT"          "2eqb5wAq3id5b6RErLgdKqaY9cKpJ2K2zn3h8gZ5tuRU"
    , e "tabAlter"            "JDydu85PZaMZLwbzDw8S6vwNRj7j5a9RJDRjShBHyAPm"
    , e "tabMap"              "EjhnJ1qA3bkfQUjWVkNgqQd5mfZdAzaivRvkN5xCJZca"
    , e "tabUnionWith"        "5BAggRkCPfL3NxYFfxKVTC34kcQ5Y4hGwsUfwFdMJ8f3"
    , e "tabMinKey"           "GHc7LYjdHEmJMNfBS65CFtRakKY5DMQswEx428H1uV8H"
    , e "tabFoldlWithKey"     "HeVZUktvsrpmw4UamDw6UVUeuhhaehWKZZ9ARfDpQWXC"
    , e "padWeld"             "7MZnYrEPYprcB2CUFxULKQSbq15rQjQSynpgHb2W8uVH"
    , e "padCat"              "9xJkv45HSD9vYyutCE1RqmD5D3Na3eTKLzXK6z6FY8PQ"
    , e "padFlat"             "dGRUUZ8q9jAHc5dL2V5qFSLAGyTQwF8CsCR2xEqPc3N"
    , e "isBar"               "8nJKJkvWRXPVDUhmxoSEnpkC2AaV42YKK9jcMdd3FZY5"
    , e "natBar"              "P4wZc5m5GX4NzDd3p992mXVwjJ5R13nWqK6o1oo47nC"
    , e "barNat"              "HfUArXzMPSVqXeMSsWWPEEyPrJ6QvotDf94BzoUukT8s"
    , e "barLen"              "DB1MZXakraoYPoWbeHVEKoB5S9K6Sh81Sk1PRcA2XJ1X"
    , e "barIsEmpty"          "BtvmrCULtfD2y4Avc78ndzDpC2xZ44wb9E7UnuipGAoc"
    , e "barIdx"              "AsVdpx1XgYzS3hFmsvfN5kzZ8YZaB9gSgAh9DkL7boEA"
    , e "barWeld"             "8HbGTT8BaZKZP9wryq6NaqPwKvXerUcZ9n2CxKv2e4uN"
    , e "barCat"              "3iQebSfgwoUzhDE6Rqs8is31DCybApSCZ3P7BSgcqU1E"
    , e "barTake"             "HurQzWS9Vvyj9MkBT3khVpZRTKqHWwXKknskdWVWvAXF"
    , e "barDrop"             "H7C1DSRb3wQUpEHkQsqpjwaFPPKN6mUD14q5UULNKTBA"
    , e "barElemIndexEnd"     "GxX4SzxrNTjXZrM5fjcTcLufE5ozkKQHntKmEuYJ4rzg"
    , e "barFlat"             "CQxJRthRuqzrKQdD1TbeBtBqH7kNrkZom5XiPpqo1WtY"
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
    , e "blake3"              "DsRCZjSBS9gR8rEoUoEgHmd8BSC2ECrAmPuVGVQNzYFq"
    ]
