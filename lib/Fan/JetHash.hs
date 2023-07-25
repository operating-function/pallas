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
    [ e "_Seq"                "7fPzp1b5QXpDDkvpt2oBGUm3w6eCPVKjDnUQ8i8w8mj5"
    , e "_Trk"                "eWQEfSHjEKocmrBmVkDecBHtkTKPwo88jZ94BS2mPQt"
    , e "_IsNat"              "UUg4gA355ySzHkLR9uyNbH667o6b4NkAv4rQ3pwJYqj"
    , e "_PinItem"            "7EaNWdf4EqHNCEBCScEuW4CrxbMzbadgkXgQcrbLwS2C"
    , e "_IsZero"             "DFAgjdmrasgLM1P776gpZDU9DVA2gBtyWuaTWb3fvoaZ"
    , e "_If"                 "CGWi51frV26N7KUBbso2WzBad8W2mbix6HaejVem5YaZ"
    , e "_Not"                "6mbxDTonsXzZJowVdsQtP5bbqfHn6DTghb11eYi6s9qB"
    , e "_Bit"                "D518gnvYpR68fwAkvSghyATH1oCh8LVG9jeQDUuws7HG"
    , e "_And"                "9JhVfFRzAkwjt7VyhC5yi7wHekWx7h4CLn36Pd9xjD7c"
    , e "_Or"                 "ALbovHrbg3HkpkB1DPXDrkPCSSgNfoz5ZgF2j1z99yU1"
    , e "_Dec"                "CQono44WNJmmGYhDL1zTmF38VwyZ2oUbhbeZxnyXcVhg"
    , e "_Add"                "3Hzr1k86NC8chd5Nfar1RuGtoaJSXKvCFKFYUxt8ZkjU"
    , e "_Mul"                "8YwEx9nXr46SsZokBzNtaaUVqYCszr5ir7FEmy6hEcv4"
    , e "_Sub"                "3xEhUsRSmD5ircNXST1QYogahZP2nc9ifWPKimQaJbiX"
    , e "_Bex"                "5UckctWkGNtkFB897kwYX8SwL8NFGJfnrFaT4U5RpRfv"
    , e "_Div"                "9gFFSd7kS4dAWKt8tAHgrV3u2iiXdhqUEdSGyJyPYo3r"
    , e "_Mod"                "44ndqWLkdXfw1Z54eapF6WbMEtxei8GXz18r9Cpxxoyk"
    , e "_Lsh"                "9mxj1s1WTtk4isk6Kz5ANHzMHfpijqDhkuxpBKPH7hYB"
    , e "_Rsh"                "CmvXgWsrSn6jthM7TRoz29Png4bJRdDVouS6owsegeRQ"
    , e "_Dis"                "J6usndApP8CiWmNrweTxCPXDtMkAayNvTHGddu4rzPM2"
    , e "_Con"                "9NpGL1uwBtXNTjpjkBNLZmGtqYTqEPbvhRPP2oqgtynn"
    , e "_Mix"                "EyFnE7tL4y6rt7exwYWEEmnSEC4jBi7Z2no4WAqUFe2g"
    , e "_Met"                "AUy2MnVXzCWna9CbYmheKSDMMd7GvVjffkRkxiFuZTMU"
    , e "_Cmp"                "J7qmkTg7sk7mRc5HPPdrnQoAfJbpRk671SQ2zGpx3pNY"
    , e "_Eql"                "xmNLNZULWYMJStQivFQuHJ9JPEgNXP8cSoB7GqQVPSZ"
    , e "_Neq"                "Djr25jdwvCzt5TyBMDQP3Dp9RxHRDVaayPvUqxX4B1jU"
    , e "_Lth"                "Gc5MVhptiGKTgjScbHg8LTDsmNCp31gQpEXKsWmJjtXa"
    , e "_Lte"                "8CWPnrxcGTi9VtXqAk4orjLEu4x73qZEfrcC83PLBsXS"
    , e "_Gth"                "2LkfSrB2etA6EZL8MpPhBae955HVm4UsCtasGSBXjn1o"
    , e "_Gte"                "53WWoHD4agmCTt843Q39yAion1tPqs7gkHfXGFaSA16m"
    , e "_Len"                "AUDshrNCtvjQfRYsYHv9s4YJDZ3f7P5MKwPkLJrNX8aD"
    , e "_Put"                "ACEkgfjSxxhvXkh7iyPg64Siwp2fpURFFREkX935v1pj"
    , e "_Get"                "386VJFmFLRAcsu3gXtsNK4s5muQusWJ4SMzgjjS6YUPf"
    , e "_Idx"                "H4Zc8hKR4nzf2w1FC8mXQmsmMrNvJ86yRh2hLK9GL3ds"
    , e "_Mut"                "4a8fsQF4NQfaKfKpTzk7vxu2DaDCuDPUAtedb7k6nCua"
    , e "_Switch"             "EL7kkFVeJ1KYbMmkJ38yTXn2KLnBBZti1vWwQdZ4mFmV"
    , e "_Weld"               "5iRVgDyQYvT25D35UjY62wEMHkT7pYxKq16GdAhDQ8c2"
    , e "_Map"                "CthwUe29n4ZGHWoLwR542FC9dCVrjPYEEaZDbQib1tj4"
    , e "_Rev"                "G1g6QG3Jh6zoHDstBVPYah8aoCGNkg8jKh8rsQnEn6hs"
    , e "rowCons"             "6SM88ydMM19ksj1Vrkp8fVpZbZQPrUg3XnDT3L3DPcV"
    , e "rowSnoc"             "CwM7r5N6oe1ZdxgUy4fKumZn13RW7wRDh7zNqmwLpz1w"
    , e "sum"                 "3T3VDQGAPGabjo64qDSLGxD9JVdvZqPYEB1wEgEm6mgo"
    , e "cat"                 "2A7BCenKB8dxcFpctZHGYCSJKzcjg1TihTfNxeb1QvnP"
    , e "zip"                 "8qD9Mvtgc9mQtUS8Z8XUGntpVxdhpT7v8KRroZphtJCV"
    , e "drop"                "5VCieXW8sUMFpw4tNuhFaoDYwKf7xd6mXhZZpbytV2th"
    , e "take"                "9Bx1LgjDJWCCNdnPeosNtsqAVpAkrfvvjbZwWNhkLNjX"
    , e "unfoldr"             "EhMbquBLZ8uXjqypKS2dTdxMpMdaUsMq6NG1LxNh1Kwi"
    , e "listToRow"           "5a4pB8s1SZmwxFa8fydvvvbycBUQ1RPcmKRHQ5FmYJDL"
    , e "listToRowReversed"   "EaRpRmBZVbQV8t3cUj1B77dxMKR13NKUyk5ueGsF2AcG"
    , e "isDigit"             "3oV11ebAnQW8JfhkBWH7A2GyW6HBL5LqrsbZjxf3TosR"
    , e "implode"             "5dpdHhM1cyexVWARXFpJWWAvJsXNprVcC8mcvK7Sr7ga"
    , e "cabSing"             "4yFji8RuGGj7ZNSyfyYaHZ5wFV3ezht4dLkjTLJYNfu3"
    , e "cabIsEmpty"          "4pn4U24yHZfr8cohDBCsPijVtEo6UENSRcL3TjVFB8sv"
    , e "cabLen"              "ALL5Zkb1QAX52C9GZcRbmyR5NcEUrEkCLo6JXtnnd7YQ"
    , e "cabHas"              "7o75jSn9JFESkH34out7HACfb2picRWCv3jUYHJfJozi"
    , e "cabMin"              "3AuxRphJUumw9RfDukbiK1mtckkbV2egTHSZBfdSf7DJ"
    , e "cabIns"              "2ZsCuyofefkr9P2tED3KhQuVH6fqFsB6oYgh5LPyTAfL"
    , e "cabDel"              "GQS6NigzrfLQ7B3uA3p8WxQ4gK2RHky64sRfyaT913B6"
    , e "cabWeld"             "AGKvvmpkMVNBdzxwqrrxZFrLguygqryVyfzdrqMNwRY6"
    , e "cabCatRowAsc"        "8frYdTvjQznjecWrSKYJkBpaK1mDD6ecdjxex7xcnKEW"
    , e "cabDrop"             "6b8wQvbEMN5NH8ezWYkDwuq8MuVKWAYDeiL6Pq334yro"
    , e "cabTake"             "9AgTWAjoHuRFdTa9i1zVDQ9926mbkUUC8wSQp3cnWmTV"
    , e "cabSplitAt"          "6yzWQ2HQJsiGhX89VW9r9SyFkymrtBERVZtS2v3Naa9A"
    , e "cabIntersect"        "HHufEqN9oFa7xRSbEU5cEDeLGz1fuj6VPZGrc4MoqUAa"
    , e "cabSub"              "FTWBsjCTEdTtSmggc7LQCysr9N3hjwTkYQ9JC48NsgRZ"
    , e "cabSplitLT"          "DV1DsxAdFGvd33Ympd7cgiRxXwaJZC5vBGNGPef4WmnR"
    , e "tabSing"             "GyfgVRmfC2WJdehXZj8sjXsaH41uDkLkn3iz1txd7WjB"
    , e "isTab"               "J79kx1EvDyLPjNYhdkrdyjxTjANyY3nPuyTMay7EDgzT"
    , e "_TabKeys"            "8b5KfdDnpHdUNavaYcVkyY4UWvgkmzhboa2WtHq8hvE8"
    , e "_TabVals"            "ChipTE8EqGJx8mKh7vKk3uLCoe87uzr57puknUVwGMGL"
    , e "_TabKeysRow"         "7GNupNFbHC2R8FoX71nRHqJdNKhA6nrPugm4PCBnfnYL"
    , e "tabIdx"              "CHn8HqZ3iq9Yoaq5CDoe8k88FBm5qraAaCqDeFNUarGs"
    , e "_TabLen"             "4zBRGTdC8o8VqyVbae2RH7HhCXjkzahcyMvBKPegci3Z"
    , e "_TabHas"             "3viEXdwoM6BcP2rUuJDRkxNjd6hGVcLwQ1ykEMqDcwwf"
    , e "_TabLookup"          "CMLeZy6YYdmwUiJRt66kcM1TZ7kqCZsq16eDUaRdhAhz"
    , e "tabIns"              "4cH1cyDFP9gzfSAC59wpM6RWpt1FgLqGF32CQMwxRDKR"
    , e "tabSwitch"           "EWbmCFyYXwcmxPs6YrB4G4zdRUC8BGgAT1d71Zp3QxMF"
    , e "tabToPairs"          "EgGHeMwTmraRnKMkccehsaCBCPWfyH6ENfhorHYn34gQ"
    , e "tabFromPairs"        "AcHyeVKhGLq7jtxRzxEyHM7bJLwir8hDr8qCrBUj4ggD"
    , e "tabToPairList"       "BHXDNjZMX7dSgfEHQbRZa8LC5RFqgTRbnp74J6gPFdBt"
    , e "tabElemIdx"          "3cYnNj7JabK6MAWwM1GHTBAqkMrmTFTSUwy86BC8C6eh"
    , e "tabSplitAt"          "AWyfK53zgX8zeKHzQSV3gPJAevV1AVERcUtVHk2tjpRr"
    , e "tabSplitLT"          "9RGatNaNWAPqfqmXM73FXrPsDq7pY9ax63A1655vuUbs"
    , e "tabAlter"            "9Ym27GuLphU8ifJ8vCnMwWfPQCo5oxcmfnNJyt7dsxy8"
    , e "tabMapWithKey"       "GJVpGGED3DCoF7pJioj3A7JkcFKuoVbGGzwfYp7GFkey"
    , e "tabUnionWith"        "9CZxFrjFwo6V7kL1btxn6yekkyAN7wUyc9zyF3ans3tz"
    , e "tabMinKey"           "65WjMT3obThTT9ivxzxj9kWiQ2PGc8ekv8ce57Kt327X"
    , e "tabFoldlWithKey"     "ARtxvih4fefF8YcbwZ8XinJbbNPQLfQErUtohwtgkGRk"
    , e "padWeld"             "4bkXyh7SUM1zHBmznchkeFBBrue63qcNMk32XmQD8TNa"
    , e "padCat"              "G6NMF6dqz9uskyFjn9SjERhc5xZjLNyMq8rwE1giM7rG"
    , e "padFlat"             "FSMJuzSha9LdR9tiXea61YZEMVeXLNFZ1k2WWiLtmPuq"
    , e "isBar"               "Ai8FN1oMeN1HjzozVCiaZW59WTxmbGdm1HLrprdGNxMs"
    , e "natBar"              "8dC8M7ME7FgoqbatYYEszZ62p8qCEEfsZ4wJrPdRyjM6"
    , e "barNat"              "FZfRKbBwmrk7FJjLdfxFJkcToywJYdMBABhQ3fa5zeGP"
    , e "barLen"              "3hZhHfmMQMcUbGrXcmFfa2smJV2SNQWfc6FvMa77zr7K"
    , e "barIsEmpty"          "HD2mt7KHf5s1HQjQdwEr7fEyxE12v636CBofWfBfJRSZ"
    , e "barIdx"              "DVrCoUq4CT9aeVZNZNuf46oQNocMJKYStJzuXARqGG5"
    , e "barWeld"             "8NnTuew6QZLe9YwYaRKUSY7ozDrThPdC9sCawTFC9Sti"
    , e "barCat"              "CEniUPx7G8JXeu9gahWNUFYzd5wSWaVXcREhXEVCa46B"
    , e "barTake"             "3R4kvk1J7NnopFRsZjFXWUJY2TfFauU89WgrxkxvskXw"
    , e "barDrop"             "EZxoqN7W8QYjcqfdkQyys2AUePpupjRDh3QNLJbZ68H"
    , e "barElemIndexEnd"     "BWLwj5NP1NDB7mNUKYXsJFTgkzLdLQNGo4n16NdDvGKK"
    , e "barFlat"             "GSe26GhyvGUQLDq3tGKBV75J5Zo42qg5cQ1cjXGcnzKu"
    , e "barElemIndexOff"     "914jZZE2AqrSjJEPSojDsjSNjtXhMPr7vgxaAsGXwA8K"
    , e "par"                 "BfENjJMJwCJxcXUrfMkv2Z5Mpyt4npKH7aZaQ2BCDMEM"
    , e "pseq"                "3oUDABdakHWyyZyPQVBjRtGG3dXrLG3FdB1P1QR9V4os"
    , e "w32"                 "25gasDA9WjWeuLoBYD8K5Wj2xGRnsXS77DtcXv2EvxTM"
    , e "add32"               "ATEFLckL8eZTcoKxveuEmnHyipVkh6P2hn5vmjec8AcL"
    , e "mul32"               "od6Z94otKU7Yt7tj2aoVPz7P46VDf4ahdWqPXwhenkf"
    , e "div32"               "7dxJrbryaPV6PSBqBQRJdrmfhUG7KAX1nxx5y71omUnr"
    , e "and32"               "HhSd1RYyvECMtf9p5PgW4Bjmh2NTQmz596CvLAum9gi3"
    , e "or32"                "9cWVjG9cxQ5ZZnVz2X7CGtMaMrVTtHYJwarzC3M9s4Mv"
    , e "xor32"               "DMH7DukjfuMapc4AUSszqBQy8xp5QoAEdEkwwWBwVAS3"
    , e "lsh32"               "4hBNWCPYTFNMn4Jg9EYRQ7uQFFeZDCqBdcj8zyHT8Rmk"
    , e "rsh32"               "3jC4v7cqorNMd8J9n1BFKJD5RfsWMpiorS7bqphVxnNJ"
    , e "sub32"               "ChdreZSsajAQxfHCu1e8sAr1MYvWy53JvYWn8Py5m5t8"
    , e "ror32"               "5GoRhDNHUgs2BVpzNYnRZbsVYwqyFdhSSRrx9ny9SzD8"
    , e "rol32"               "7wUZHamoFyYzFf4D6skMr3iHS6grNMFJePcmFQd4dVB3"
    , e "w64"                 "Gmkhocdn6PjiaRom27DBeYZ85pSWyy26TeMva1Z8nxo2"
    , e "and64"               "HS6H43YYABKNqAmqE8hxL5724HMJchxFQp9r2khM3o3u"
    , e "rsh64"               "67XfaRodAjUhBFGnpuAxJDqzx6pLFGKiRyQPLvJ1iWbm"
    , e "xor64"               "23q8XuvhEognUEk4xYixqA3s1MKx7JYi2KPZqVSf5iYU"
    , e "mul64"               "AphXApov5VYBpNGQDxyeyNav58i8JMJf3jB2aY9qBjxD"
    , e "or64"                "7tw3Gh1tLUUyf7LLfjAizCgyTfSJq83UnBiwogHJ4EMy"
    , e "add64"               "CTD63hHuMybVg6G6foDg7YH1H2LxDs4i6rVTSk2eNCqZ"
    , e "sub64"               "F4VTU3myYEm7VCshhXzzBMFh5SRpnMTKt7QL2P5DmCdh"
    , e "div64"               "A12PreDCbFd7CECMoVWL6sKseNY9ssLnRFzTtrUSSruQ"
    , e "lsh64"               "DCxwASnrVAaV5qBEmKGCiGqJpGfK3PKMLeABJgEnq1zd"
    , e "ror64"               "HRbNLodsp3DCZ5rjazpu1njEe4xeFsz1VRgV8cKTES9F"
    , e "rol64"               "3nBfAu5yfGzZ1rxLJFJMs1FRmpWiAPN3rgpK9BgWUybo"
    , e "iDiv64"              "HMUUUr1SDL7qZvE15yTr8xosyHLj4uxiREGW5uDsK2me"
    , e "_DataTag"            "EDQm1rudzF71qQPiAERwabdVg3drR91iWvZ5wswYBqpP"
    , e "_TypeTag"            "87uikPRjRLmDMJbVf4NAgh21Bhn7KKpTC3RGvPDfkE3e"
    , e "blake3"              "9mcU2MPVSaizs312TsDjwYSXUrExW3fU4XSGmKWHnfUg"
    ]
