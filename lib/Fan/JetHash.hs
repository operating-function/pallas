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
import Hash256
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
        , ( "_Trace"    , traceHash     )
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
    , e "_Trace"              "DWsz4CYrc3xzUQRxtibBiSXUx8HzR3tRgGNsbAMJLe9w"
    , e "_DeepTrace"          "7oDjbbsyiM1paMXmzzaR3P3KEit8MHRVSRduAX3qan5V"
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
    , e "_Dis"                "2Z8qLQTuYxay3WtcYbc9SUuMrPQ9pYW1vBmxDeSMJUTr"
    , e "_Con"                "GZePgtHxmDxgCLsaP9pppHuUk41poFdjKpqcScnfYk9M"
    , e "_Mix"                "81VFGaHf55JanaXuzyMLeuEoekC5sSiu55hMZh9otQ1g"
    , e "_Met"                "4GUFwMLeLx8uj6k9inYRZNUWtUfy4H9GkpD4883VJmBp"
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
    , e "_Weld"               "3WdMEVASETpgXzKSqbVJnHQU48R3BYY9TzA4WCZgSoYh"
    , e "_Map"                "FZF5tePfHzx5fS8AKiguJFVNsxmru7qtW2gx3XnDAtse"
    , e "_Rev"                "CqUxtHFrwrMr4uJxbtRe6mXqeLsuMrbGDH5zfrUopJv8"
    , e "rowCons"             "BYkKdsLmEejiiWc2udBQpiVMV1VdzBmmMDg4wvrG6BAY"
    , e "rowSnoc"             "jBJVBkpj6Wh4juRkZSjYSFHGhQdxg6wTPKZdA9qtp3p"
    , e "sum"                 "3T3VDQGAPGabjo64qDSLGxD9JVdvZqPYEB1wEgEm6mgo"
    , e "sumOf"               "2m4vPxjKE3WFZC2YKxRixvkQ85sHMZQksNHWrL9jLJXw"
    , e "cat"                 "AptmxG38USTvJSNFUQjiiNhuzqungTfPX72txwxaTfFn"
    , e "zip"                 "ChjJhJdpZVRQfzLHgjnjBYPTQa2yhwJBv8RZ8aYBezBV"
    , e "drop"                "4nfi3eGqymzWw8rtsktVhMH2ELkdkjhWsmfqrucZmQ8W"
    , e "take"                "FcSpbFLeogZfphiawDm8GF9fQGLBfGqZuGAPFoKBRAQX"
    , e "unfoldr"             "AqALs9y4dSE4nUxpGv6vYjqtq4eJL9ZXQSeMb9n7a2vK"
    , e "listToRow"           "5a4pB8s1SZmwxFa8fydvvvbycBUQ1RPcmKRHQ5FmYJDL"
    , e "listToRowRev"        "87BKrGSnBAxqnn1Zq7LhiDnv63RiJJj7DCuVcenHkMqH"
    , e "sizedListToRow"      "9cYJs4NyusCFj8vEoddGEKvNriavxoJ6nLb3rp94gMQn"
    , e "bsearch"             "6Tqi6mdjKgB9aui3QNyPsduraM6vMr9BVUy2CWbnYzDt"
    , e "isDigit"             "3oV11ebAnQW8JfhkBWH7A2GyW6HBL5LqrsbZjxf3TosR"
    , e "implode"             "6F1Rms5CjELnCFVsrdKg8ajgBHi63wgC5ZBDb9Ny5yQF"
    , e "setSing"             "2S8yfCmRiecvoxgLJVJg5L6MxLm5avjoGqGJmnbukt27"
    , e "setIsEmpty"          "BDgQFXd9VbEEdoBcaqMys39ucwLS4WFhQLAxypncGj3W"
    , e "setLen"              "4yZ1gLVWzH46af8cUrqhJiAZdSNTpRMPw97JVZH3Z6Ya"
    , e "setHas"              "HvnjdtWtVFXD828Q9Zrjda4kSaYnnsMzQ7C1C9Crmqyb"
    , e "setMin"              "99RzLRXhcLMYwmi3Z92BzxyP7ByJepiSNbMhH3tYThxr"
    , e "setIns"              "BYQMu8TPZcdJeemWuFm4UFUiEo8CYgRSok2piroVssJC"
    , e "setDel"              "FTdiNqErdmfHkHKsEsajuMEWii3H86cEd3yzd4PaguDV"
    , e "setWeld"             "8P2pX55htrEGo44kK86MtUfgeiX8FcNaJb2yFrGFPND1"
    , e "setCatRowAsc"        "A4z7FKWzUfEQysfDrMUtVWQ8ngEASsvn8tEVhEZb6uvF"
    , e "isSet"               "2H6DyaV9HxmYyoW1JbmrQ16cAPVhmUYV1CcN7iCPwjcW"
    , e "setDrop"             "GakVM3AE2TiRiCMW9m3kkfJqAS5C9q35x8fg8C7GdZfr"
    , e "setTake"             "9YFfy9eHPxUksLkeuNrVWbBctrEJGXunE336Kj8zxuwY"
    , e "setSplitAt"          "2heHVeDAvKimrSAJrd6UTKdvtdjiyCSrvTwe3srX7s6r"
    , e "setIntersect"        "EyzYuio2Pk286xNHG6D8yEsM6VJN72X687y9sKgFyQH1"
    , e "setSub"              "DcmrdLwhpHTfnDZZJits16poGpUKcWnHGduFpGNeWuT5"
    , e "setSplitLT"          "FHP2WH4TqK6tCLdDMamsLmb245q1yyg3Hh2sy399PPwh"
    , e "tabSing"             "7AfouhmiExP9KSySjrw4k4VZ5JY2TE4cW53xo1EhHsfz"
    , e "isTab"               "24ZTpfkWiejf34UAL21RNUWtTwz2sckvBQqYj4Yn1d4f"
    , e "_TabKeys"            "8b5KfdDnpHdUNavaYcVkyY4UWvgkmzhboa2WtHq8hvE8"
    , e "_TabVals"            "ChipTE8EqGJx8mKh7vKk3uLCoe87uzr57puknUVwGMGL"
    , e "_TabKeysRow"         "AbTABFk1h4qcAoW2nzQ37JtnzPfVgapUSXLBcZqid4rH"
    , e "tabIdx"              "HkDJsyACmEQwdcc7CtcpPkNdVGRhzkVmWY1gDNhN83J"
    , e "_TabLen"             "4zBRGTdC8o8VqyVbae2RH7HhCXjkzahcyMvBKPegci3Z"
    , e "_TabHas"             "5Z6xRsyAtA2EPf8Gsn1swvDN8ckHFk2V2hjf8zU6RndK"
    , e "_TabLookup"          "BaFsYEXP3AUHtJxum1fgNewFtdkuhHtupWB5MGoVewZe"
    , e "tabIns"              "Hv6x3DoMXZ6TrRAV5jEWeRNV8GMKSPcfCoQexq1hoEAf"
    , e "tabSwitch"           "6pLsy7ZCw5ZmZNaCrroA6YMBCttkZ694DSASKqHMRrGQ"
    , e "tabToPairs"          "7c6Xrr4NFJ2veyQiYh7C23u4TZsPgrondGdDC8ReDHkS"
    , e "tabFromPairs"        "DGsrGkuTWJsU16AAuGFxqfYbuZy3YF56FVZbTCLdvt8t"
    , e "tabToPairList"       "CnE3UnKi3bhmFJoyygDz6uf5vwJNxPntv6ZLbmdEHrDo"
    , e "tabElemIdx"          "4xF4VL35Z7goFdxf3KmqwwFo6j55hVWporTtveHRoCCD"
    , e "tabSplitAt"          "7Vcnbv95uH7tYNyo2dRanKZNHbXWrgZ2ygja3LVVQQfS"
    , e "tabSplitLT"          "By8KUd9gNXKw5rNvtLkzdcRQ2eR2utJcQbJzHBjPxaqK"
    , e "tabAlter"            "3n6S4sCM2ydnFZ4xEY1wyZEZZGe9AgAGg98HjYXpyvLr"
    , e "tabMapWithKey"       "GJuwcTKUQgm5PpvHCTM544AmCau8a7QxohrcK4waXuWH"
    , e "tabMap"              "Ey9LyxPSwZyFTL5jP8fNFNYDYSvSJzSE94Jdv8Nhhe9R"
    , e "tabUnionWith"        "33ufEAJUhoGctxLbj9j17U5CswwvXy8j2bdNv5cHkKZo"
    , e "tabWeld"             "659oxURPvnhKzCh35wFDLxvXDjmZG1z8X1Eis1ArYVKm"
    , e "tabMinKey"           "7rk82gGb1DPK28m6WdsagHCbtnneUraNnuUV4cdxttww"
    , e "tabFoldlWithKey"     "FXG4Rv1s1R6zsMCMbN4RTMbfGsCjoyZS13xcSAHDdh2B"
    , e "padWeld"             "H9aQ4XZ2PnsitMydvWYwKVav2KMtDuKBqpmvqiQNMYiK"
    , e "padCat"              "HrCM2aT94yGbCgNXbtx2ufCkpCqRPVS8PQ8hvubHh1ry"
    , e "padFlat"             "AQ1Lemy7HdgF3ZvjdhMGH43q9tMEi6YMG34Wcey7yCF4"
    , e "isBar"               "BjMDwdqPjVPaeP6Zcfad88s186gZp7binbLnuJ32mNGp"
    , e "natBar"              "8c16DSgGjQQU6TUZzabXnySwvq1GJrhtVpuAeEgo9izt"
    , e "barNat"              "FZfRKbBwmrk7FJjLdfxFJkcToywJYdMBABhQ3fa5zeGP"
    , e "barLen"              "3iayc5dkgkASTLshZ4tp5qcam1cg5QzTidmC7d94UK3m"
    , e "barIsEmpty"          "HD2mt7KHf5s1HQjQdwEr7fEyxE12v636CBofWfBfJRSZ"
    , e "barIdx"              "DVrCoUq4CT9aeVZNZNuf46oQNocMJKYStJzuXARqGG5"
    , e "barWeld"             "GKNvrpNsj8AfSFbSX5wQ9UoyUNRydJsTdnaqKvQoiV1F"
    , e "barCat"              "2GiCQPqbaH5bt5JqMHBqK86v4VR7goiAq4cCpd7Ndy7P"
    , e "barTake"             "Dx9hYYnVvAJqJkFL7pVBxkNC9TVRgX5MwTwsYSiPG1CU"
    , e "barDrop"             "9dRBXovsw8WyjrNyyNyVhkxi8xfG85Gf6qgJsJoLed7S"
    , e "barElemIndexEnd"     "6LRS3exKVrKgDTTWkPrCokXHPEJCbdwDLTASwACw6YW2"
    , e "barFlat"             "6nHtWf5221ZbJ1s3uu9dm9hnZZCUDU81rffxp6DKnS9s"
    , e "barElemIndexOff"     "BCdPrGzUN9TKoi4b9DkiMpgvvP4f5A4iy8Khqqpk3XHW"
    , e "par"                 "BfENjJMJwCJxcXUrfMkv2Z5Mpyt4npKH7aZaQ2BCDMEM"
    , e "pseq"                "3oUDABdakHWyyZyPQVBjRtGG3dXrLG3FdB1P1QR9V4os"
    , e "w32"                 "25gasDA9WjWeuLoBYD8K5Wj2xGRnsXS77DtcXv2EvxTM"
    , e "add32"               "ATEFLckL8eZTcoKxveuEmnHyipVkh6P2hn5vmjec8AcL"
    , e "mul32"               "od6Z94otKU7Yt7tj2aoVPz7P46VDf4ahdWqPXwhenkf"
    , e "div32"               "7dxJrbryaPV6PSBqBQRJdrmfhUG7KAX1nxx5y71omUnr"
    , e "and32"               "F3Ea6ATuT7GsxVXD4F8eZtaTvWpp58N9scB9L5q4aDSb"
    , e "or32"                "5gN9PvMQWctYVd71hUtZ7RpYL5sjytJwY5rDbLUCiteH"
    , e "xor32"               "581LBZjTnrt25xbZLz48E6JAHPsCia3Zs9eA2VpN6ctF"
    , e "lsh32"               "B8B4GPwSnjhAyL4cBzRQXpjrRYosioYuEy5LL1vuRuP2"
    , e "rsh32"               "4Sp9fvXUNm5dyfRF282nNqLZdb5qFmRBeJvPaTtvvoT3"
    , e "sub32"               "ChdreZSsajAQxfHCu1e8sAr1MYvWy53JvYWn8Py5m5t8"
    , e "ror32"               "51HcxfPXqrH5sy52KeCdV13oLt4HAPaTfMRohe7W7cup"
    , e "rol32"               "7PUiJpkNKByR4kLSBghEiUrjuyuF1jA19PPkeGh6dzRq"
    , e "w64"                 "Gmkhocdn6PjiaRom27DBeYZ85pSWyy26TeMva1Z8nxo2"
    , e "add64"               "CTD63hHuMybVg6G6foDg7YH1H2LxDs4i6rVTSk2eNCqZ"
    , e "mul64"               "AphXApov5VYBpNGQDxyeyNav58i8JMJf3jB2aY9qBjxD"
    , e "div64"               "A12PreDCbFd7CECMoVWL6sKseNY9ssLnRFzTtrUSSruQ"
    , e "and64"               "FrVrX9PQfgXzTv2FrWioMgsaqD7DY914xz1URKF6jtA4"
    , e "or64"                "43QxcgrCUb42FTfeTNLpp45jxWyLgHxu7YCK2FbppKpj"
    , e "xor64"               "G4baDxBKYaVwhF6UTXngZPxZhjBWSJFjFiGayWapPusw"
    , e "lsh64"               "CER1fr8eCGTnUrNxQyVySAjiTh9RDrL4oMeBFNVLrac2"
    , e "rsh64"               "Ejj81JYTgG1GNZE84ZLHv2EhngMT1DM2kPvn386YrVmF"
    , e "sub64"               "F4VTU3myYEm7VCshhXzzBMFh5SRpnMTKt7QL2P5DmCdh"
    , e "ror64"               "6zreqSdet1VMhqdcrUS9jKj8tsNB65Kv5SiXDNQbZnvZ"
    , e "rol64"               "DdFW2raxgRWjt68AmeSTz2RbfN3QCqHfEGuMnBdaMAXQ"
    , e "iDiv64"              "HMUUUr1SDL7qZvE15yTr8xosyHLj4uxiREGW5uDsK2me"
    , e "_DataTag"            "EDQm1rudzF71qQPiAERwabdVg3drR91iWvZ5wswYBqpP"
    , e "_TypeTag"            "BWgwW3xnmiAnA3FPhGCobMp1rkVTjhMTzgeDhAc7RBAU"
    , e "_Try"                "HtDbQzAUWkuPuo2eJAtj5M1ZCi9fjZ3UWiSVS86fBU8M"
    , e "blake3"              "CfoMMsB9VdrWQJ93HgK5DEhun9T293c29Ur9XWf4R8rd"
    ]
