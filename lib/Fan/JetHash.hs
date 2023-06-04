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
    , e "cmp"                 "Hjk1i3Fva862QuAbfkGLbzRZmFixXtUZ51RbyBSe8Wsw"
    , e "eql"                 "CY2nfXMwFKM63boxJ18PLVxQX61NsX9d7bcCjVuRi2F7"
    , e "isZero"              "4QwjPzcuR6B14surpRVkSxT51Df5gijnhc28vvb6piPE"
    , e "neq"                 "BH2f2hYqbTWgqaWvYH8znQ97tVS9vq2jrxhGLwdRHMaw"
    , e "lth"                 "6rfscT4dpGzYH3vQ8xxXagsgAQ1Yn3ihnmK83cPBZKkD"
    , e "lte"                 "CSvbyXQDTwVRykvzxGZpvEYvWcmg3LFW3py3CBAQF7bZ"
    , e "gth"                 "AiT9eFcrVtMhdeHQkLiyXKqLwDpAJxVwTC4XDdbFTSb2"
    , e "gte"                 "CLavqVDfvQssmx1RSWpy61vsiE58DbNXRPfU14jvBhJK"
    , e "len"                 "EAVASVC8Uj4t6njGvNsgnDAwW21Nr7CWjzdBFjkEvgB3"
    , e "put"                 "C3qXpf8mZYmTWhuTpHLPfWUB9bpiceNotpToCaMaMWTD"
    , e "get"                 "Gymg6Aruyn4NuFJTNcwUXK3qfCJik55ivDvPWwpvRbpu"
    , e "idx"                 "C1RBvvDN37q5h3Mh4FVLqurHYELkT7zUBJhmfNS5WwLs"
    , e "mut"                 "9aWrvicQoQzh2H8F86TJBQuAn8UQoCZmy416ZAqXZ5Dr"
    , e "weld"                "HuY6QGXmonyDt1VdwuVS8eqqq17caNsUZKcQmjGYW7JV"
    , e "map"                 "3D8EJbVg7gzmESKnvqmG8c9CTTdUnwGRrkAyoByMm7bE"
    , e "rev"                 "CSkJ8Z66YwG25kDqweCKcDip6WXXDsNzQqNij2qYph17"
    , e "rowCons"             "CnYHorgPz12xhaFCTpGn79hKiyiYqCJuJ7TAnGH8MTs5"
    , e "rowSnoc"             "5MSPqEhgcYiJvmkZZJyCsf9UCKnkobYEKDpQUV2z17Wz"
    , e "sum"                 "78Ub72jGoGoF3rDk3RbEWWMGvXqqC8qw2HPpM3k5Wc7G"
    , e "cat"                 "BSKyaUFVD7ux1pPjMaK6AmpcMSVyzPhR2SjuNh6d7jTr"
    , e "zip"                 "EWLJQdFTAbdq1Q6i6XasoGqRzvfnPzshFDSWSRjTbTrL"
    , e "take"                "Aqe3UQaUionG59qw82mw68GVtEGPgn9xutNuPuf4cnt7"
    , e "drop"                "CKskM9YyCDnb3LQwEHL2zTc7RpfqZzTXd25fkqX6JLPt"
    , e "unfoldr"             "DPM9T2a4Lqt92AJSpZSqw6cfGwNZhLK6Est44nuQoNE7"
    , e "switch"              "6UT83nrQXsVY7im8gRSWvxp2g8y8BAgLG4E22fCVfRLm"
    , e "listToRow"           "6ujb5j5hAwK2nGsM72uLi51gfZzqgfMAnM3mLW8286u9"
    , e "listToRowReversed"   "EyL34pnPXpT2zgnrkGBAETCFe8pJyxygcFqVPiuojSrF"
    , e "isDigit"             "ABQe2qJeXpxsGUSB98utyQKW74SbC6EShCZeixfoCrFZ"
    , e "implode"             "CqpTp3XCmgfeJE2s11edw99wGP3NNAYymD6VXiKQBUX6"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "FG3smXbqxKWiyRuYif4Ywm8iaeMEs279X44GcCoiZDi6"
    , e "cabLen"              "BVi1faLJaqYjWrq3t3yzE64G58MVish7dPjhCbk23a4G"
    , e "cabHas"              "CUTfhBR1JU5tHzczsUYDZxMjVsBvEEsdBEkon87UHkus"
    , e "cabMin"              "ABw4RcJuZ5yoyVYb9NtVTos25b6AXMPR1W7sLJKEpoWa"
    , e "cabIns"              "DSwWopXPni9CAH6SP3AoUbsDWKnPEAJ4vqmLEtShbgyu"
    , e "cabDel"              "Ajj5AzNPmxeTnuDfqBvMcwtTfDMqDGkoktv4tr2MtCFt"
    , e "cabWeld"             "AUF4scE2BzBPKyZjsGmRKNvSZXDbS5CNatFY12pRjsA4"
    , e "cabCatRowAsc"        "31nQjwP3WzQ9KcG3tBVLTsjouoQofoqLPZfSCbamnJTy"
    , e "cabDrop"             "4MzY1Hk3gtrpNqN6om4gQAs9RM2fdzL7etweRa3odtJn"
    , e "cabTake"             "Hwwt6bSHxQvxyhDyrahoRgeY37xQjKEVJUPVCYmq4cC2"
    , e "cabSplitAt"          "7snnHkTS6kNCXE1dHx4HSGmvai4jBzBNJk957seYYvya"
    , e "cabIntersect"        "Erjs7JXH3C4LrZb6RCbt5RbqEwxcQjkTk1BDA8wmESby"
    , e "cabSub"              "2bj8a6BDE82DWzudxyoGyF1urg62XmeyoTeShEZAij3Q"
    , e "cabSplitLT"          "4r2AAxMRXTbe55s5qiyC3Dp8FtCZEkkJzBSaUm6kWH7c"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "9JHyX6Nb8aUX6RhUczZ8z3ykPNZcZnF227sHtoutSuTL"
    , e "tabIdx"              "D3JYzpytMRkQM5YRECZR1jCNNiBJK2zUUyQCUc2wS5uX"
    , e "tabLen"              "4LXgw6Q4nmdHWD78rQ75rDvPxaFsEecZCvMKsd1VrrgR"
    , e "tabKeysRow"          "Gc9cj3GhViYeWxjkmsNZZYWnfy7qyZunjXo5g1t8urUD"
    , e "tabVals"             "EffPH1s6m4G5w36US4mpseEbgHFiTcVUpgrte3Bbot5v"
    , e "tabHas"              "8MLCQ2dhW8xe77ivQydoBSNB7XE1N3XQeW6oMKPuKTH6"
    , e "tabLookup"           "4sPK5g1ZUebPX8q2LGbnc2QoQGwXPXaaA37SgU7vmDc6"
    , e "tabIns"              "47HLqzo41XArNVJvF7ntGpfDYi5N8o6pP2PaURDkQsNy"
    , e "tabSwitch"           "3K5GMSb8xv8Wow4hPZiLZ8Jbog5b7QJtUaYsxnCkU2rT"
    , e "tabToPairs"          "A7G9jB1YNQx3vaSDicYyB4XzpRvuoYGstH13zXx4NiyL"
    , e "tabFromPairs"        "EsnmAMdTwGp5MPraaMomHYd8K3zYexH6jdZgtaFXHAAV"
    , e "tabToPairList"       "8JtwSfTpJSWnmsXnpepqCwTWJB8pgxeGk4ECnqu5NFEm"
    , e "tabElemIdx"          "Ari8YzViHQo6TgtC4y3cskyJ5FUsTXUfYgcGT7bi3z8b"
    , e "tabSplitAt"          "kcYpLXh2SmxJyDUZ8hgmcQrAPP59X46tS2en7ohZLYr"
    , e "tabSplitLT"          "6SJV4Z4gciXuWioQRXKoiMPTX5FPkYNiB6DhmsL3Ztgi"
    , e "tabAlter"            "AAgpZvU8nQDp9WmLUHvFo3gX2dDzAbHF33aJUg4MHgAS"
    , e "tabMap"              "66KuwbcJNfBfR5ixuToykquYsCXBWiGBPZd9f1i8k9f7"
    , e "tabUnionWith"        "HgysewXdBN46yQjmW8LG4CjqLkiVforunUGyVdXL5Dpd"
    , e "tabMinKey"           "GHc7LYjdHEmJMNfBS65CFtRakKY5DMQswEx428H1uV8H"
    , e "tabFoldlWithKey"     "HeVZUktvsrpmw4UamDw6UVUeuhhaehWKZZ9ARfDpQWXC"
    , e "padWeld"             "7MZnYrEPYprcB2CUFxULKQSbq15rQjQSynpgHb2W8uVH"
    , e "padCat"              "9xJkv45HSD9vYyutCE1RqmD5D3Na3eTKLzXK6z6FY8PQ"
    , e "padFlat"             "dGRUUZ8q9jAHc5dL2V5qFSLAGyTQwF8CsCR2xEqPc3N"
    , e "isBar"               "4XNL8iBKpMMW31fVzMAAsZy1Vj8NZEEkQu1dqVf2nZfM"
    , e "natBar"              "P4wZc5m5GX4NzDd3p992mXVwjJ5R13nWqK6o1oo47nC"
    , e "barNat"              "HfUArXzMPSVqXeMSsWWPEEyPrJ6QvotDf94BzoUukT8s"
    , e "barLen"              "DB1MZXakraoYPoWbeHVEKoB5S9K6Sh81Sk1PRcA2XJ1X"
    , e "barIsEmpty"          "BuwohzPLZ3XtFwgNZBHCWv8WUYjQea2A4g33oR2zXidS"
    , e "barIdx"              "AsVdpx1XgYzS3hFmsvfN5kzZ8YZaB9gSgAh9DkL7boEA"
    , e "barWeld"             "8HbGTT8BaZKZP9wryq6NaqPwKvXerUcZ9n2CxKv2e4uN"
    , e "barCat"              "3iQebSfgwoUzhDE6Rqs8is31DCybApSCZ3P7BSgcqU1E"
    , e "barTake"             "GNZNmZcQjtbT8EPKnsVQSAkSBbYez6VSKsjRVoTqzUcV"
    , e "barDrop"             "9fgsL5r12m4qPYN3FXPPTFfpDYTKNAbTBNaBbm2Yn2Xn"
    , e "barElemIndexEnd"     "8vfTSZMJfzw67DxkyaJH9gRmsKrxuTPRw1fDFNcnhKNi"
    , e "barFlat"             "G76tE8nf8jxxvdDMiiWnWAtCLcvwj7Xii7MRWNQkur2r"
    , e "barElemIndexOff"     "8TCYVpu3Ftuc6cyyTosJNArqpXx4ymV2rJgqosHKoNmF"
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
    , e "sub32"               "34hiFRiJXJC8d8i6oUtgkW8Qjj7UHSYKjG7PakAaGqBr"
    , e "ror32"               "GRG8DvxSQWzVPBjrMF9f8dQHtBUYjFAhD373rUuXAwVS"
    , e "rol32"               "4hMnYuWKuW7SgfptVsN62PtuMwQ2tX9GzhBxv9CUaArX"
    , e "blake3"              "8gNM6cGBiHNLLU3B1hTuBLADWqYvo2sEookn1rRZHpzf"
    ]
