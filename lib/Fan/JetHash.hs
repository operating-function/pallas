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
    , e "unfoldr"             "2gfx8wf5fPpWtHrtL8qknGY19bGK6DQWtRoctHo17tj2"
    , e "listToRow"           "4bgGturnvrdjjKa4RGRWsu4CYMuMg7qUsmVND1sgTWSc"
    , e "listToRowReversed"   "7WixiwheGSP1LBmoNNJzTY41zXH3c7ZpxFVWYfZHBAMR"
    , e "isDigit"             "GWxivSETStPa86suK4SEGw4MnXKnRHdo2QcqJ9ggFtPb"
    , e "implode"             "7M9a5dXyPFegtZmH8ipsDRwKkEf71Kipxowbk7nW8oUz"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "DhCmEsWNZGVX3fTdoQdrFnibANSm5WYpB7FZ7PeDqNbg"
    , e "cabLen"              "HrzMxtJHGMB1FkAqmQzsERHgcLmxkNQGBve9vqVavfA2"
    , e "cabHas"              "Gsmn6HsJkM3veWn9KcVQqMVDXMNi3VtRYoMHpJbymz5t"
    , e "cabMin"              "AyCffDgMhaedY5AVWVzTu7Qoaoof2ebK1QGGGYkMuF9Y"
    , e "cabIns"              "JByxu16BsLinHv1twepaUMtnY3S1M9sYUoki4bneYigT"
    , e "cabDel"              "7SMdDFDgKvpt1ZnLNcg36dFVz9ntfKGuzmsfjBsJnYP8"
    , e "cabWeld"             "Abex5RBgAbRxPmH8cKbo1HoxjomsTQw8AERNbUgXyCMR"
    , e "cabCatRowAsc"        "Eha2kpASK7RWsDPhWUTJBtQATwwyxSCx8TddFPKsRJ3N"
    , e "cabDrop"             "G7KvqnKXizzEjd9qqfm6qUwcbVhJUH71tHkxkpqYRkzo"
    , e "cabTake"             "FwFQCVY9KdkEuFk9GVcUHLv7FraG5AkH4vwdk7JByDpN"
    , e "cabSplitAt"          "7SdYwAqoGB5sGk6YtcJkC3U5q1842a9JhuDTeoe7CX7v"
    , e "cabIntersect"        "CFssUER7tCGhseTSTnkF41HBwGXmb8qynQxkz5mVRviL"
    , e "cabSub"              "7SA7JyvnftemdJAJA2pjmLmCRZb3145o57a5EpwZCvcx"
    , e "cabSplitLT"          "5Ck1r8FqTzjds97UrDudoKdPE1viN54W8GxvdQcWWdKw"
    , e "tabSing"             "4SX6W9jfvWRTHNgMR5QdUxsQ32rdPdvavaLmbojoynha"
    , e "isTab"               "3g3CbgbxYq61nYPCPwTkjHkEWDUX6SDEDY1vmwXUvnFU"
    , e "tabIdx"              "4Uwpmc65Xz2C5wNNTMnhqY1oA3UuZcLdNTbEVmrNMvuK"
    , e "tabLen"              "8DQHQ2pnFKMPF83mxd5bjHq683C3XP5dNAnCEiFJxcBx"
    , e "tabKeysRow"          "GDrr3bdLWALVUKvsQaXHXALuBiGu3ky4UtwwHejCz4ka"
    , e "tabVals"             "7qWhxsSLzupKC4mqNWsozzH2mrqmGXEt98MqZagx13tN"
    , e "tabHas"              "HmFZ8EdsYwV79EhsXhdNvvPdwus5AycvWYVXmARPjXtH"
    , e "tabLookup"           "72GnMbETGadupErZTAfJAWhq35vwjzkQkWce8aZTDcwU"
    , e "tabIns"              "3SAQ6n9oBuJ7KbokJ3T41WrHfmPWJk5Mr3o2pighxkcW"
    , e "tabSwitch"           "GZhfwc8nn37wXp2kih4u7rRRQ6cQNbxioGKKgLfSvirW"
    , e "tabToPairs"          "6Q1zV4e8BmvseVFz2Gu3DPRXe7QXsVy1GXoFcGVWi12B"
    , e "tabFromPairs"        "ApMnWAmf3ky4HnSbqwagS5D63K4VNX3ihiM6HBtxAM4S"
    , e "tabToPairList"       "4SWJHoGg5321xFFt97Bi1iMPUs63iNY42VfHNsJdsWR6"
    , e "tabElemIdx"          "E6FfxpnVgZ6peeviKPS9KAsmT3JYxutgicWdj6PrZazr"
    , e "tabSplitAt"          "CGzUhtCE3ftpYXngQMEPJLE8d2Ttxhzen6FN8kYndSaN"
    , e "tabSplitLT"          "3piVjrexs2LYLr5593fUvNoLq5giWSzjHRPdg4SntjVw"
    , e "tabAlter"            "h3acwX8hLfAtBSJErLTnsrP35w4p1ZxqaqeAoRF5oc9"
    , e "tabMapWithKey"       "7rB3mSNqPfnrjk3pD1VyAqbuSZyCS1AM7TbERYDTiGSi"
    , e "tabUnionWith"        "7svnbNKRNkjtjkXTDWC7AnaBhPbEAk7PTW5UGDYLHPh6"
    , e "tabMinKey"           "6dq3qQAA8FcehnUMKycJeke7dQMqECcrwZJADxmCpKce"
    , e "tabFoldlWithKey"     "7FjEsh2j1ZBFCnTJYBmyNJJYAzye2atAFSLzGD7dgaZT"
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
    , e "_TypeTag"            "3eGcEJWsnTNYiMCGFGSEcpVURgBhpzG1ycobEN5qqAod"
    , e "blake3"              "8yqLmzz7bHaF9N8XDNZdmaED9LP6X91xZ1TS4y93nmst"
    ]
