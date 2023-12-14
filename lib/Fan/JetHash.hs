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
    [ e "_Force"              "A7ZtnajpFHHJ29VVzZoZ4BENtqNio1LZcG7Pdc9nMFvf"
    , e "_Seq"                "7fPzp1b5QXpDDkvpt2oBGUm3w6eCPVKjDnUQ8i8w8mj5"
    , e "_Trace"              "DWsz4CYrc3xzUQRxtibBiSXUx8HzR3tRgGNsbAMJLe9w"
    , e "_DeepTrace"          "7oDjbbsyiM1paMXmzzaR3P3KEit8MHRVSRduAX3qan5V"
    , e "_IsPin"              "9yKhatzX3nmiaJuvt9k4vC15TFpfLrdAJgBYAHbsNNLR"
    , e "_IsLaw"              "BWQ7azfQEprHXNumwKc2zKHSHxXdkydxg1zvCZKfXTGF"
    , e "_IsApp"              "2b6Ki7gnsoZnLFdXdNrdrF2PdehjsKpvFiHaD4y85VbY"
    , e "_IsNat"              "UUg4gA355ySzHkLR9uyNbH667o6b4NkAv4rQ3pwJYqj"
    , e "_PlanTag"            "Hbq7zzUZ4tQXWZv1RQLLTrvSvsCXWahVd4nzCn28kB4g"
    , e "_PinItem"            "7EaNWdf4EqHNCEBCScEuW4CrxbMzbadgkXgQcrbLwS2C"
    , e "_LawName"            "Gnorn9t78uWCBXACgxF2QEbXpXvHZZgA71Ryt8DL42Wx"
    , e "_LawArgs"            "Gvj7mVywtF79a5xV17UXceJLFtYAneSD1PJfrCEEMnEY"
    , e "_LawBody"            "CYHDZqaDfuN2MGLga3LuSUNeAHf5wmDvL928Ca97RwWw"
    , e "_Car"                "CT92yRzvnSAA5eqLf31G58S5kKCbv8A6JjWNtm8raZQp"
    , e "_Cdr"                "HiPkTzvPxdyzkza8Tq43m9kmAmv1jEgevkFngo8T7kGV"
    , e "_IsZero"             "DFAgjdmrasgLM1P776gpZDU9DVA2gBtyWuaTWb3fvoaZ"
    , e "_IsOne"              "D2WW4ViRDVPuGzGUpwxtu4Pfv8ovhdbtCwpdAM4X9H4y"
    , e "_If"                 "CGWi51frV26N7KUBbso2WzBad8W2mbix6HaejVem5YaZ"
    , e "_Not"                "6mbxDTonsXzZJowVdsQtP5bbqfHn6DTghb11eYi6s9qB"
    , e "_Bit"                "D518gnvYpR68fwAkvSghyATH1oCh8LVG9jeQDUuws7HG"
    , e "_And"                "9JhVfFRzAkwjt7VyhC5yi7wHekWx7h4CLn36Pd9xjD7c"
    , e "_Or"                 "ALbovHrbg3HkpkB1DPXDrkPCSSgNfoz5ZgF2j1z99yU1"
    , e "_Xor"                "E4tUTqjvQrnCpGFWNRhUoH2ZzrjxLyi7iDNVcPetqyMW"
    , e "_Nand"               "EcKTTCta47vbd1Fq85Dm214tZfuSbS5a3xeBX9LcPUkq"
    , e "_Nor"                "5Q3KgaFr5KdNVhMSA7CRD2WfNvaZM2JSG1XWnp8aK5BD"
    , e "_Xnor"               "XwJTASftvk563i9uqWyCjEnjHbNdtjwXEXDN5pXFGhP"
    , e "_ToNat"              "HedgTAZEjCCShX97HZNsm4gGGWyxhRf9jepYzrtMR6RW"
    , e "_Dec"                "CQono44WNJmmGYhDL1zTmF38VwyZ2oUbhbeZxnyXcVhg"
    , e "_Exec"               "6NXitaeCdj1wMcAvqtejHNNzU5jq4zWcX2FqwFLMUmu8"
    , e "_Add"                "3Hzr1k86NC8chd5Nfar1RuGtoaJSXKvCFKFYUxt8ZkjU"
    , e "_Mul"                "8YwEx9nXr46SsZokBzNtaaUVqYCszr5ir7FEmy6hEcv4"
    , e "_Sub"                "3xEhUsRSmD5ircNXST1QYogahZP2nc9ifWPKimQaJbiX"
    , e "_Pow"                "9SDk572JGpAwcKKacS5JXWKvTmzoHmXEmqDZSfgjpDep"
    , e "_Bex"                "5UckctWkGNtkFB897kwYX8SwL8NFGJfnrFaT4U5RpRfv"
    , e "_OrdWeld"            "3NEV4ZNZrDukganvzgH2bhgexDyruMoeE8cRmZK4HJdL"
    , e "_Div"                "9gFFSd7kS4dAWKt8tAHgrV3u2iiXdhqUEdSGyJyPYo3r"
    , e "_Mod"                "44ndqWLkdXfw1Z54eapF6WbMEtxei8GXz18r9Cpxxoyk"
    , e "_Lsh"                "9mxj1s1WTtk4isk6Kz5ANHzMHfpijqDhkuxpBKPH7hYB"
    , e "_Rsh"                "CmvXgWsrSn6jthM7TRoz29Png4bJRdDVouS6owsegeRQ"
    , e "_Bix"                "YRpj6KUnCQBH4GEPqjrnc8185m9xXRMwaZh5MSDtMPk"
    , e "_Bitwise"            "GqL9rtzxoQp3SKajU8DybMChXs6ZmkfRWHLotRwQWarx"
    , e "_NatFold"            "HMePscsWoKQMcEsXWTNFr9E9EgKtHVMhuwxE8uiwhZwt"
    , e "_Dis"                "2Z8qLQTuYxay3WtcYbc9SUuMrPQ9pYW1vBmxDeSMJUTr"
    , e "_Con"                "GZePgtHxmDxgCLsaP9pppHuUk41poFdjKpqcScnfYk9M"
    , e "_Mix"                "81VFGaHf55JanaXuzyMLeuEoekC5sSiu55hMZh9otQ1g"
    , e "_PopCount"           "7JzsJdSJRdJUEfdCo3rXTPSiPcT7uxq1SicW9rfdks92"
    , e "_Met"                "4GUFwMLeLx8uj6k9inYRZNUWtUfy4H9GkpD4883VJmBp"
    , e "_Trunc"              "3jyR8sAD5g6ABLgq6YaBArJicvF4kKAJKWAHYEPz9QPG"
    , e "_BitSlice"           "qaAaZyKQaztojPjZ72HM42yhyJv1vNeWmFyTr3QDvcB"
    , e "_SetBit"             "2Fz6d1GEoHPTTNYsnZN1HWsAxv1gihajqYPVS2FcaxLG"
    , e "_TestBit"            "6H7doMVpvoLBDSzfNK5ZTw44ijQpyhD7rNLKGUfCU3hW"
    , e "_ClearBit"           "57V4EuuGixDN2cttGJxSPd2XJjp9g9sYT1m324R1W4AD"
    , e "_Cmp"                "J7qmkTg7sk7mRc5HPPdrnQoAfJbpRk671SQ2zGpx3pNY"
    , e "_Eql"                "xmNLNZULWYMJStQivFQuHJ9JPEgNXP8cSoB7GqQVPSZ"
    , e "_Neq"                "Djr25jdwvCzt5TyBMDQP3Dp9RxHRDVaayPvUqxX4B1jU"
    , e "_Lth"                "Gc5MVhptiGKTgjScbHg8LTDsmNCp31gQpEXKsWmJjtXa"
    , e "_Lte"                "8CWPnrxcGTi9VtXqAk4orjLEu4x73qZEfrcC83PLBsXS"
    , e "_Gth"                "2LkfSrB2etA6EZL8MpPhBae955HVm4UsCtasGSBXjn1o"
    , e "_Gte"                "53WWoHD4agmCTt843Q39yAion1tPqs7gkHfXGFaSA16m"
    , e "_Min"                "BtHUXz5q2dQfYwWTkusJEAuGcSBvZj47GtPXa6HAWWNN"
    , e "_Max"                "39hPsbuHZDwaC4hYUH8qEUKd5fKVQSL9E6mzuffmuDz9"
    , e "_Null"               "B7Yd7cPdoDFsLgkYmuWdsvM4NJfMSmasj3Jhp5agrFbF"
    , e "_Head"               "qdNQEg1BoJgcRm1qRGdNqDK86bm7XSean7torY3Q2XR"
    , e "_Arity"              "2jz4zz3a621Z6uhWSpTbv2wRi91aQfsSRn76Yi6me5gy"
    , e "_Len"                "AUDshrNCtvjQfRYsYHv9s4YJDZ3f7P5MKwPkLJrNX8aD"
    , e "_Put"                "ACEkgfjSxxhvXkh7iyPg64Siwp2fpURFFREkX935v1pj"
    , e "_Get"                "386VJFmFLRAcsu3gXtsNK4s5muQusWJ4SMzgjjS6YUPf"
    , e "_Idx"                "H4Zc8hKR4nzf2w1FC8mXQmsmMrNvJ86yRh2hLK9GL3ds"
    , e "_Mut"                "4a8fsQF4NQfaKfKpTzk7vxu2DaDCuDPUAtedb7k6nCua"
    , e "_Last"               "HKg4NMDUSy9FkBNmsdNvExNHLf6VrL5xAZrJzBHvxxQy"
    , e "_Switch"             "EL7kkFVeJ1KYbMmkJ38yTXn2KLnBBZti1vWwQdZ4mFmV"
    , e "_Cow"                "3gqiFBpFd6UwEJSqH2MLPE1s3C5hTSra5uF6GYzsW1za"
    , e "_CowSize"            "G6gN1nJbBtohntPrsG3v6edTtr78MMPJLaehkQ7sLcSL"
    , e "_IsCow"              "fM38dUyDfW9k129YEL69JMB9NjaoVzWoi7w875HwckK"
    , e "_IsRow"              "8qWGT4Qp7MAQAVuAN4GdXDR1gz1LQMxQKwACo4jkxqBR"
    , e "_Gen"                "2Xjoe15B84gukTsF7RFCzQ9AvWq78pwRTgyAt3zRXdRC"
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
    , e "_SizedListToRow"     "12AFprhZ7HSa4451hkhveQTVCmuds6kvmLYmjQZsDUNf"
    , e "_SizedListToRowRev"  "EXQjpxzxxiLqW3xMwKqRYyUyFfxbEpbvfhRMsUBDmxtE"
    , e "bsearch"             "6Tqi6mdjKgB9aui3QNyPsduraM6vMr9BVUy2CWbnYzDt"
    , e "isDigit"             "3oV11ebAnQW8JfhkBWH7A2GyW6HBL5LqrsbZjxf3TosR"
    , e "implode"             "6F1Rms5CjELnCFVsrdKg8ajgBHi63wgC5ZBDb9Ny5yQF"
    , e "_MkSet"              "4RUfSzoWUyiYQ5omEL7XWABd3bCpp6Q92Gx62fdDvrVu"
    , e "_SetToRow"           "3GGGFTFjPGAJp75fDidHT2ir5gjRY96P9qhPAEi37L2R"
    , e "setSing"             "2S8yfCmRiecvoxgLJVJg5L6MxLm5avjoGqGJmnbukt27"
    , e "setIsEmpty"          "BDgQFXd9VbEEdoBcaqMys39ucwLS4WFhQLAxypncGj3W"
    , e "setLen"              "4yZ1gLVWzH46af8cUrqhJiAZdSNTpRMPw97JVZH3Z6Ya"
    , e "setHas"              "HvnjdtWtVFXD828Q9Zrjda4kSaYnnsMzQ7C1C9Crmqyb"
    , e "setMin"              "99RzLRXhcLMYwmi3Z92BzxyP7ByJepiSNbMhH3tYThxr"
    , e "setIns"              "2btWXRDPC3X8Ap6KbWdRdFcwfVdeX5dPdsuANvkGBtXo"
    , e "setDel"              "Hk6kNgbt2wTkXxQrfT5JMqeRPxnWHNFJoCZaNR7tpxk6"
    , e "setWeld"             "B9sWjF5rSY7ZPPv6Vc5Nyv94bmk1ckCCBu6SbHQTkE2X"
    , e "setCatRowAsc"        "A4z7FKWzUfEQysfDrMUtVWQ8ngEASsvn8tEVhEZb6uvF"
    , e "isSet"               "2H6DyaV9HxmYyoW1JbmrQ16cAPVhmUYV1CcN7iCPwjcW"
    , e "setDrop"             "GakVM3AE2TiRiCMW9m3kkfJqAS5C9q35x8fg8C7GdZfr"
    , e "setTake"             "9YFfy9eHPxUksLkeuNrVWbBctrEJGXunE336Kj8zxuwY"
    , e "setSplitAt"          "2heHVeDAvKimrSAJrd6UTKdvtdjiyCSrvTwe3srX7s6r"
    , e "setIntersect"        "5EGs5nH9F2rj33NPQjpp7XyohTUjHW8SYBxdxWhkA8pf"
    , e "setSub"              "BH2vkFXZ1KvSspzp8g7WdnYYAMfQkw4JLgffrphMGY2U"
    , e "setSplitLT"          "FHP2WH4TqK6tCLdDMamsLmb245q1yyg3Hh2sy399PPwh"
    , e "_MkTab"              "5hFXwrFMAS1ETGYZDANKd5Eo9xkjNnxPRKTL7H3Td4yv"
    , e "tabSing"             "7AfouhmiExP9KSySjrw4k4VZ5JY2TE4cW53xo1EhHsfz"
    , e "isTab"               "24ZTpfkWiejf34UAL21RNUWtTwz2sckvBQqYj4Yn1d4f"
    , e "_TabKeys"            "8b5KfdDnpHdUNavaYcVkyY4UWvgkmzhboa2WtHq8hvE8"
    , e "_TabVals"            "ChipTE8EqGJx8mKh7vKk3uLCoe87uzr57puknUVwGMGL"
    , e "_TabKeysRow"         "AbTABFk1h4qcAoW2nzQ37JtnzPfVgapUSXLBcZqid4rH"
    , e "_TabKeysList"        "HJCWTAgwp23pzpsfcFtztNa8uaNit6cSdYN19CqmejyZ"
    , e "tabIdx"              "HkDJsyACmEQwdcc7CtcpPkNdVGRhzkVmWY1gDNhN83J"
    , e "_TabLen"             "4zBRGTdC8o8VqyVbae2RH7HhCXjkzahcyMvBKPegci3Z"
    , e "_TabIsEmpty"         "2pqKJoYtYzxGuojhjcD7APfxGauDgrJr6frjcEKY75Cr"
    , e "_TabHas"             "5Z6xRsyAtA2EPf8Gsn1swvDN8ckHFk2V2hjf8zU6RndK"
    , e "_TabLookup"          "BaFsYEXP3AUHtJxum1fgNewFtdkuhHtupWB5MGoVewZe"
    , e "tabIns"              "Hv6x3DoMXZ6TrRAV5jEWeRNV8GMKSPcfCoQexq1hoEAf"
    , e "tabSwitch"           "6pLsy7ZCw5ZmZNaCrroA6YMBCttkZ694DSASKqHMRrGQ"
    , e "tabToPairs"          "7XNsmym7XdJvy2GqXPqLnFDgHaNbsh2adrFZBQrunB3c"
    , e "tabFromPairs"        "DGsrGkuTWJsU16AAuGFxqfYbuZy3YF56FVZbTCLdvt8t"
    , e "tabToPairList"       "CnE3UnKi3bhmFJoyygDz6uf5vwJNxPntv6ZLbmdEHrDo"
    , e "tabElemIdx"          "Avh54TuYvu1GmNwdHNFhyBpq8YNQmWosgLf5aeHNVMe3"
    , e "tabSplitAt"          "8N1WuQiB6UapjdsuMk7M6mzevkwzi7kVtbDjzRbqKRNC"
    , e "tabSplitLT"          "UdzPypcfJLtyMjLy87ukmU7nRHtfEv3nnZbfJ34isWM"
    , e "tabAlter"            "8De2Zp5USsNsg7jKUNqCVhBacjFavmMCbvtwcLuVpDXg"
    , e "tabMapWithKey"       "F7ZsfTuKUBFxjop8LJb6Wokgk4ZbtM4bhJqvvt74FAuf"
    , e "tabMap"              "21sMxqo5DrqX8oruXM3WqgmVb3BK9nkWHUoGuckt72jV"
    , e "tabUnionWith"        "EJ3XaNjVQgdngjRN8tAifM1eWSyTvZoJUFUYzkqwAJcV"
    , e "tabWeld"             "H7kNsSGBtLFaQ15CvvL7jph9WgVYi5EWX2xRPRH2xHD6"
    , e "tabMinKey"           "5e9RqAsaYgy7w8yt1DmgLN6e2nPUXqBT7ixPNvnG6MuW"
    , e "tabFoldlWithKey"     "DZ1uBEGqnQoVcC1sYiPSsSvZhfR4EZfghZAanHsag96M"
    , e "padWeld"             "H9aQ4XZ2PnsitMydvWYwKVav2KMtDuKBqpmvqiQNMYiK"
    , e "padCat"              "HrCM2aT94yGbCgNXbtx2ufCkpCqRPVS8PQ8hvubHh1ry"
    , e "padFlat"             "AQ1Lemy7HdgF3ZvjdhMGH43q9tMEi6YMG34Wcey7yCF4"
    , e "isBar"               "BjMDwdqPjVPaeP6Zcfad88s186gZp7binbLnuJ32mNGp"
    , e "_Bar"                "3rEdAKuWvV2VjaZoXae5fD1H3mAqkLJjZhqSp2djP9ez"
    , e "natBar"              "8c16DSgGjQQU6TUZzabXnySwvq1GJrhtVpuAeEgo9izt"
    , e "barNat"              "FZfRKbBwmrk7FJjLdfxFJkcToywJYdMBABhQ3fa5zeGP"
    , e "barLen"              "3iayc5dkgkASTLshZ4tp5qcam1cg5QzTidmC7d94UK3m"
    , e "barIsEmpty"          "HD2mt7KHf5s1HQjQdwEr7fEyxE12v636CBofWfBfJRSZ"
    , e "_NatToSizedBar"      "3kcNbAks4Lxjozp2phFVkXDh231DY329D1mVwboyoKEi"
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
    , e "_TryExp"             "31ZEB3kaWgWpa1kV2MAbeQVmbgdzqkWtEKpxnGfwERwP"
    , e "_Try"                "Aa59HEsdP7CKNvL4TwMrQjzLpBtGwECDfHt2dMuxMJWE"
    , e "blake3"              "CfoMMsB9VdrWQJ93HgK5DEhun9T293c29Ur9XWf4R8rd"
    ]
