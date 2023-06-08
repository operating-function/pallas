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
    , e "_Cmp"                "4qjS21Pm8ELd11V1MQCRERac3pUx9iDQ77ZVmpBc5L19"
    , e "_Eql"                "CtQVm2LVerYJfcomQbndMfLRuDvgiuCmTY5JbgimunBz"
    , e "_IsZero"             "B5JGEWcQ9g1KvumqZqRCHH2GMXPtYpEH85ZAJJysjD9T"
    , e "_Neq"                "4aUBDfHZSuuZNZGzNo5TaabZsRUbTCx2PWwY8snCLx3H"
    , e "_Lth"                "J8ngwfa75kZz1r5AmvNEKyZ1GBMpb6C2q7Bjji7N7Bc9"
    , e "_Lte"                "FDoEyXTJDwabpVyLMJjqqGLDooRjucG51H5TGQZcN9H9"
    , e "_Gth"                "Eh7t89fFutythVuBn9wwawX4ACDWotbPJxapdf3JdHdc"
    , e "_Gte"                "adLC7hh7Uu2WQXExUMxybYbLFN2HHHMcvdRWYEJ3Xvt"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "C3qXpf8mZYmTWhuTpHLPfWUB9bpiceNotpToCaMaMWTD"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "9aWrvicQoQzh2H8F86TJBQuAn8UQoCZmy416ZAqXZ5Dr"
    , e "weld"                "3kL6UvkDnbwJGhemXwmiDJHSTH3rTK3JaPN4RKTnrvgh"
    , e "map"                 "3D8EJbVg7gzmESKnvqmG8c9CTTdUnwGRrkAyoByMm7bE"
    , e "rev"                 "CSkJ8Z66YwG25kDqweCKcDip6WXXDsNzQqNij2qYph17"
    , e "rowCons"             "HmJeGJDaUmVUeEHB5taimBuvi9T7J1uHNzD2n7R218Tt"
    , e "rowSnoc"             "477NRXXYjhwW82bFnAtep49XCmzuhqYprLY2iyAT1Kjt"
    , e "sum"                 "78Ub72jGoGoF3rDk3RbEWWMGvXqqC8qw2HPpM3k5Wc7G"
    , e "cat"                 "2gBmnTuZxngDh9wygCSpJbBtB9X3uC3UqmrTMwNuo27Y"
    , e "zip"                 "CTXtEpJbrRukowEnQs3hB5eoVXutsTpppnjkgjs8aUDm"
    , e "take"                "9jMASzkADTT9FA7ECoBQtkiSe89rPxGB1tkU1qgkXLTB"
    , e "drop"                "CKskM9YyCDnb3LQwEHL2zTc7RpfqZzTXd25fkqX6JLPt"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "9aC7AVye2Wf9oWYxqsmBPxrZNHaai4XcJ2AAzk7WLmM5"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "6khDuZk2LVnixtkfD76ERssdQ3ZVR21p4RyXPRv1foeE"
    , e "implode"             "CqpTp3XCmgfeJE2s11edw99wGP3NNAYymD6VXiKQBUX6"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "7YVfwZAAmDGQsCJeZwtJcwuev6WUp3yMHGMDmJu1g5NW"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "7AvMX3qRB2NvfqapGi3nBmYYpv81H6zBNMeVLNyUYRHb"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "HHkcuMH2p9BpYDLDowwQBjHoBFXGGrMcMFZgZw8527bM"
    , e "cabDel"              "6AtRygUobheid4L2aFb8j1yCES3xGQv2y8YLBvzAXH6A"
    , e "cabWeld"             "Eh8aomZLRvZ3bkfWD2Xp7VTNnpKpVL2foh9AhU1wsN3T"
    , e "cabCatRowAsc"        "9aspQ923XvreDzssiAwnJmRxMRuf7UfUFknBU7NPvANH"
    , e "cabDrop"             "4MzY1Hk3gtrpNqN6om4gQAs9RM2fdzL7etweRa3odtJn"
    , e "cabTake"             "Eqx8GHeMrwq52g7KcNyDiUV1wvkrCjPsFtkmGmSRXNc3"
    , e "cabSplitAt"          "78H4riwdpkBPAdf7C6V6P3iYxKrBVaMndNYRj5Zc8DRG"
    , e "cabIntersect"        "BuNz2fAouiKg4zkRpyWTEsTfH1oJWfktnVUgjPgJ13zx"
    , e "cabSub"              "J6za4MWDejy8c86eAG4d8jwaRaAymFRp6xAkYCCSzvGA"
    , e "cabSplitLT"          "EhGEaZSa5V2Dc6Anh7MbQFo4jHjet9jMussQRF4n8St2"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "Fuyjom4fdDnnRoQ2XsmCN5ZWVhoSwF4MDzzCPgEDmoVt"
    , e "tabIdx"              "DdaRs4sUjsgsMTVBdy2s1QUHTJ58b32xNAZtzD9pcMtY"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "2jxgKgxvFF9ciUTVVBwwAnaQ5LFwfHDaGWCjHGZNPYX4"
    , e "tabHas"              "5w5J3hok2aMZMR6S3g7v6NbQdpRCYwJQjN5LuXGEsSwW"
    , e "tabLookup"           "2Y2SiyXwe6jAaAGyuu1qvVNAEvYWWAuzRA5wVqBwk6oT"
    , e "tabIns"              "3DeJSppELCuoW38qKugeewL3A6M8vrmtddXxApYH5zwz"
    , e "tabSwitch"           "7MrWQ3ctYFzrsYKt3SxDRMmpYBq5c1DAWh6HtmGCDFQi"
    , e "tabToPairs"          "A7G9jB1YNQx3vaSDicYyB4XzpRvuoYGstH13zXx4NiyL"
    , e "tabFromPairs"        "7xeuUJVaVCFwkXVMmFxGShm3wmAyyzLj2aWmAbqMLEn9"
    , e "tabToPairList"       "biCQb1gcFhnafAQT3YLnhDj63yV7t162iyf58HhkiNZ"
    , e "tabElemIdx"          "7AwUKzWNCyqxwgiW9U6W24P73jMo3Koipv2F6BpwZtrz"
    , e "tabSplitAt"          "9iLioq3Vjm8akFqsjfi4L315qvVfDDKFEZTEDkk2Pf95"
    , e "tabSplitLT"          "GhPc21xbpfK4kRuW3r7t5PQL3wkqWnsrvhQSZgEKc4X1"
    , e "tabAlter"            "23pMAojyeh1KPy55ya8GyNPvZN4miEWTcFxKp6jmnT88"
    , e "tabMap"              "7eMjhK3wFgBLCqgFozS4mZqFzRCA7haQdwYpCsnygk82"
    , e "tabUnionWith"        "3rwCbmpHgpDubY5wRrvZF9GN7mU8sKWjau5yx7arzu8F"
    , e "tabMinKey"           "GHc7LYjdHEmJMNfBS65CFtRakKY5DMQswEx428H1uV8H"
    , e "tabFoldlWithKey"     "HeVZUktvsrpmw4UamDw6UVUeuhhaehWKZZ9ARfDpQWXC"
    , e "padWeld"             "7MZnYrEPYprcB2CUFxULKQSbq15rQjQSynpgHb2W8uVH"
    , e "padCat"              "9xJkv45HSD9vYyutCE1RqmD5D3Na3eTKLzXK6z6FY8PQ"
    , e "padFlat"             "dGRUUZ8q9jAHc5dL2V5qFSLAGyTQwF8CsCR2xEqPc3N"
    , e "isBar"               "DPyNq6BMu43NbmS32JdH8amcSFgNWs7ufQNkmpVnFMwT"
    , e "natBar"              "P4wZc5m5GX4NzDd3p992mXVwjJ5R13nWqK6o1oo47nC"
    , e "barNat"              "HfUArXzMPSVqXeMSsWWPEEyPrJ6QvotDf94BzoUukT8s"
    , e "barLen"              "DB1MZXakraoYPoWbeHVEKoB5S9K6Sh81Sk1PRcA2XJ1X"
    , e "barIsEmpty"          "GUpsPeVAqdoMWH3xZafRXHvdQr8KBPwb3cuQyo2dTHQd"
    , e "barIdx"              "AsVdpx1XgYzS3hFmsvfN5kzZ8YZaB9gSgAh9DkL7boEA"
    , e "barWeld"             "8HbGTT8BaZKZP9wryq6NaqPwKvXerUcZ9n2CxKv2e4uN"
    , e "barCat"              "3iQebSfgwoUzhDE6Rqs8is31DCybApSCZ3P7BSgcqU1E"
    , e "barTake"             "GpCj3dAtoaPaVKGhrtfjHCysniaHLVNjP4uVbFpnB5ke"
    , e "barDrop"             "6Hrfo38a4WsC6mS6WK7HR7LfzjcWKCAAHgwnsPXzxjGp"
    , e "barElemIndexEnd"     "34Xgru1pshVuvnGSmgsF3q6qJGbyHvHZgoHy2AHyxWZ8"
    , e "barFlat"             "KLKKmN6PU7CETGCQgm8nL2iXSFxHgwBjaG7aGC3rgrA"
    , e "barElemIndexOff"     "285VcfsuMei7dK7y1nzDaTQs3dzKmvdzTPzdMt8oNhLs"
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
    , e "sub32"               "5ZGS7AsBqkAcFEdGzi7d64GEivoTUVaHVdfT1EfMnoua"
    , e "ror32"               "8iLHjx1yhQmvAFEWVUuRRMLSD7HgwMCEPiorDCZUbfX8"
    , e "rol32"               "BJPn5VGuDCip8x8eVMzgbZUNCuxpuqnoNyQSjoTMcy2w"
    , e "blake3"              "94Lh7QLgugG8Vn4p6JztcGhGujrS9n9Ydu23ouDGP9bQ"
    ]
