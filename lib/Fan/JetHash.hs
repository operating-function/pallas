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
        , ( "_Ifz"      , ifzHash       )
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
    , e "_Eqz"                "HYgduheRaC4oQMy7qwLEGYLC9fhesds7HCgifxqfQHBR"
    , e "_If"                 "CGWi51frV26N7KUBbso2WzBad8W2mbix6HaejVem5YaZ"
    , e "_Not"                "6mbxDTonsXzZJowVdsQtP5bbqfHn6DTghb11eYi6s9qB"
    , e "_Bit"                "D518gnvYpR68fwAkvSghyATH1oCh8LVG9jeQDUuws7HG"
    , e "_And"                "9JhVfFRzAkwjt7VyhC5yi7wHekWx7h4CLn36Pd9xjD7c"
    , e "_Or"                 "ALbovHrbg3HkpkB1DPXDrkPCSSgNfoz5ZgF2j1z99yU1"
    , e "_Xor"                "E4tUTqjvQrnCpGFWNRhUoH2ZzrjxLyi7iDNVcPetqyMW"
    , e "_Nand"               "EcKTTCta47vbd1Fq85Dm214tZfuSbS5a3xeBX9LcPUkq"
    , e "_Nor"                "5Q3KgaFr5KdNVhMSA7CRD2WfNvaZM2JSG1XWnp8aK5BD"
    , e "_Xnor"               "XwJTASftvk563i9uqWyCjEnjHbNdtjwXEXDN5pXFGhP"
    , e "_Ifz"                "3C2F3FHjCiM8zaWbTKQnNJbCfs4vU11tpcj3wETVQJtZ"
    , e "_ToNat"              "HedgTAZEjCCShX97HZNsm4gGGWyxhRf9jepYzrtMR6RW"
    , e "_Dec"                "CQono44WNJmmGYhDL1zTmF38VwyZ2oUbhbeZxnyXcVhg"
    , e "_Exec"               "6NXitaeCdj1wMcAvqtejHNNzU5jq4zWcX2FqwFLMUmu8"
    , e "_Add"                "3Hzr1k86NC8chd5Nfar1RuGtoaJSXKvCFKFYUxt8ZkjU"
    , e "_Mul"                "8YwEx9nXr46SsZokBzNtaaUVqYCszr5ir7FEmy6hEcv4"
    , e "_Sub"                "3xEhUsRSmD5ircNXST1QYogahZP2nc9ifWPKimQaJbiX"
    , e "_Pow"                "9SDk572JGpAwcKKacS5JXWKvTmzoHmXEmqDZSfgjpDep"
    , e "_Bex"                "5UckctWkGNtkFB897kwYX8SwL8NFGJfnrFaT4U5RpRfv"
    , e "_OrdWeld"            "8ZeTyQLBT1MSmfbD8ZBdaXJHzwv1MEpL2DSZfRTcyY8h"
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
    , e "_Cmp"                "HEeWY7mnpMfwEoQkKnYz2Q86mK77xUVSw8PyZS8iM4cc"
    , e "_Eql"                "6tVP7tm9JYumzncsteAqf94NJBzL768n8u1ky7B3UjvA"
    , e "_Neq"                "3SpjpW59rGHt6iHo75jDaLUpNZiSGthQg1AmqrwQ2UEJ"
    , e "_Lth"                "D9Q6xrzVYu9YAyErcJ4Ws1cxV7PptSebfuuLGBP6NKyV"
    , e "_Lte"                "EJX3fqndYRo2bnxjvLLsQNZJbTnzuppti4pdiFAjWhMf"
    , e "_Gth"                "7NLD8TGpofDKEEE8Xq9ABNRQEgCoGXKzdwY7jHcAVgPq"
    , e "_Gte"                "8Hzs96yN8fX9Sni8e1JV56SjKbQMHTQbozpLFZqWtw2h"
    , e "_Min"                "CvpbU7SxbhpQqRGjdKN9AGGUWM6bN4gggLptV3nUgyJE"
    , e "_Max"                "tDMweegY2x4sRgWVomWntWRKu5FejDkbhoTTcZU19V9"
    , e "_Null"               "B7Yd7cPdoDFsLgkYmuWdsvM4NJfMSmasj3Jhp5agrFbF"
    , e "_Head"               "qdNQEg1BoJgcRm1qRGdNqDK86bm7XSean7torY3Q2XR"
    , e "_Arity"              "2jz4zz3a621Z6uhWSpTbv2wRi91aQfsSRn76Yi6me5gy"
    , e "_Len"                "AUDshrNCtvjQfRYsYHv9s4YJDZ3f7P5MKwPkLJrNX8aD"
    , e "_Put"                "ACEkgfjSxxhvXkh7iyPg64Siwp2fpURFFREkX935v1pj"
    , e "_Get"                "386VJFmFLRAcsu3gXtsNK4s5muQusWJ4SMzgjjS6YUPf"
    , e "_Idx"                "H4Zc8hKR4nzf2w1FC8mXQmsmMrNvJ86yRh2hLK9GL3ds"
    , e "_Mut"                "4a8fsQF4NQfaKfKpTzk7vxu2DaDCuDPUAtedb7k6nCua"
    , e "_Last"               "HKg4NMDUSy9FkBNmsdNvExNHLf6VrL5xAZrJzBHvxxQy"
    , e "_Switch"             "H7XPT97CQCEzTb7ekjsKPUhwCUB2mTpSg5pvQietomcd"
    , e "_Cow"                "3gqiFBpFd6UwEJSqH2MLPE1s3C5hTSra5uF6GYzsW1za"
    , e "_CowSize"            "G6gN1nJbBtohntPrsG3v6edTtr78MMPJLaehkQ7sLcSL"
    , e "_IsCow"              "J9D6GAKqFiyfbzyr9ejHExr4avfRHS9EycgSTqMpk77s"
    , e "_IsRow"              "8t2nzyaUKEB4iXUcLVaLwFLDHJ6tddQK7Dp5W9oZQLDW"
    , e "_Gen"                "2Xjoe15B84gukTsF7RFCzQ9AvWq78pwRTgyAt3zRXdRC"
    , e "_Weld"               "Dgjg8RbBS9do47JALpfmb7JZFPaT2YmL4dVNVtU3Suf2"
    , e "_Map"                "FZF5tePfHzx5fS8AKiguJFVNsxmru7qtW2gx3XnDAtse"
    , e "_Rev"                "CqUxtHFrwrMr4uJxbtRe6mXqeLsuMrbGDH5zfrUopJv8"
    , e "rowCons"             "4CGP5ji6z4B6n8vFpagW9A64VmxrSwXQhUrqM6dbtwdm"
    , e "rowSnoc"             "CpKNJ2e7r2yu6Ra4jra1YewpVCCzwiPN9T5Njp2Ups1B"
    , e "sum"                 "3T3VDQGAPGabjo64qDSLGxD9JVdvZqPYEB1wEgEm6mgo"
    , e "sumOf"               "2m4vPxjKE3WFZC2YKxRixvkQ85sHMZQksNHWrL9jLJXw"
    , e "cat"                 "5PxaigZh2fB1zbZUkcookVYUx8g7ya9RoiNA5nnPYyAs"
    , e "zip"                 "DM2n1Seqrc9kUNX6fr9raazscNKtK5mAP9it1ZXJghv8"
    , e "drop"                "2uo8V5i6LZhWtefJ4A3GA5Bw7SNoPg8NS9BYWduJeJb8"
    , e "take"                "BNwpEw8EnDijiFUhRzatVmfcAXj4xb953hxrEX3CeP3d"
    , e "unfoldr"             "AqALs9y4dSE4nUxpGv6vYjqtq4eJL9ZXQSeMb9n7a2vK"
    , e "_SizedListToRow"     "12AFprhZ7HSa4451hkhveQTVCmuds6kvmLYmjQZsDUNf"
    , e "_SizedListToRowRev"  "EXQjpxzxxiLqW3xMwKqRYyUyFfxbEpbvfhRMsUBDmxtE"
    , e "bsearch"             "CGJEryLZvzq7dVGk18M7RaCFivdRBe3TpvdY3aBspZyL"
    , e "isDigit"             "7h5HWpKay3BuhPwjqDTrUD1BQP5W6h2QU4dq9WzuJGzz"
    , e "implode"             "6F1Rms5CjELnCFVsrdKg8ajgBHi63wgC5ZBDb9Ny5yQF"
    , e "_MkSet"              "4RUfSzoWUyiYQ5omEL7XWABd3bCpp6Q92Gx62fdDvrVu"
    , e "_SetToRow"           "3GGGFTFjPGAJp75fDidHT2ir5gjRY96P9qhPAEi37L2R"
    , e "setSing"             "2S8yfCmRiecvoxgLJVJg5L6MxLm5avjoGqGJmnbukt27"
    , e "setIsEmpty"          "BDgQFXd9VbEEdoBcaqMys39ucwLS4WFhQLAxypncGj3W"
    , e "setLen"              "4yZ1gLVWzH46af8cUrqhJiAZdSNTpRMPw97JVZH3Z6Ya"
    , e "setHas"              "CHDmC2WwqNNZjqBA7rjKuNaWvkJNV6uoDGe3LytDHJLz"
    , e "setMin"              "99RzLRXhcLMYwmi3Z92BzxyP7ByJepiSNbMhH3tYThxr"
    , e "setIns"              "EiAK5LfJau9exivnfnAMUDDxRVK9ddocBTuMLvz5FTjN"
    , e "setDel"              "3say53RMvMcZm1ZJ6TTfNVgC9bhevcKNnfJ5xqheGUwN"
    , e "setWeld"             "F5Ftvg6kJhHWn2s3jXbsepJ6bq5icBr6YpLrB7d269jx"
    , e "setCatRowAsc"        "DfYnmsf3xq3tUNdKXDhxwXgDX7LsHKuf9T6qJw8FFjns"
    , e "isSet"               "ExHuAVPzpiWvUjKz7gbb6TQNQ6EojMGyDNwHP6nL2ibU"
    , e "setDrop"             "4rPxLE3Cp5MGT4CoZzV6GM7LkJd1EK8mC7vzNiATvFbZ"
    , e "setTake"             "AYHbLiypsbkPyzADxvavxwaHJgP4AWNa5jCwwnA1eUTm"
    , e "setSplitAt"          "4TvurabHTfTM44fgjFzLqxp1LoBPzQEVBAv5A8JpmADF"
    , e "setIntersect"        "9sryec7avFi4MGnTH47AyhwyPyuhD9NQm8UeabrW42FR"
    , e "setSub"              "pMA5VSwcBkHanUyBHG6UL7uGuTREjndAYxavCqeyHNv"
    , e "setSplitLT"          "36B2GbR2AQz1qWd8ky1SY4jEJnKBCb7unSxexXE6wWMn"
    , e "_MkTab"              "5hFXwrFMAS1ETGYZDANKd5Eo9xkjNnxPRKTL7H3Td4yv"
    , e "tabSing"             "7AfouhmiExP9KSySjrw4k4VZ5JY2TE4cW53xo1EhHsfz"
    , e "isTab"               "HgLsBbKYYChGEg7VAS2qpjvEpAn54osPCmhxQyzAFTP1"
    , e "_TabKeys"            "8b5KfdDnpHdUNavaYcVkyY4UWvgkmzhboa2WtHq8hvE8"
    , e "_TabVals"            "ChipTE8EqGJx8mKh7vKk3uLCoe87uzr57puknUVwGMGL"
    , e "_TabKeysRow"         "AbTABFk1h4qcAoW2nzQ37JtnzPfVgapUSXLBcZqid4rH"
    , e "_TabKeysList"        "HJCWTAgwp23pzpsfcFtztNa8uaNit6cSdYN19CqmejyZ"
    , e "_TabLen"             "4zBRGTdC8o8VqyVbae2RH7HhCXjkzahcyMvBKPegci3Z"
    , e "tabIdx"              "CDN2WfatwQERySm6UtxhhDnH47ogKiUkmts6qXViWvva"
    , e "_TabIsEmpty"         "2pqKJoYtYzxGuojhjcD7APfxGauDgrJr6frjcEKY75Cr"
    , e "_TabHas"             "GuATJsgcP7HFb1HdZtbwbJACaojQCMbQTECq5g9AMwmQ"
    , e "_TabLookup"          "ALyLJ8cApsR3FiRDEkoteGXwwn3NQDPj38KSSyrzBPPs"
    , e "tabIns"              "2e8jLXVcdm7RkNSzmkLTrmrQusXLrbb7SxWSYKxRzDKf"
    , e "tabSwitch"           "9NWcCQLRRPBPi9gTtAzDokrrUbXLZ6QZbo1p6pRh61nd"
    , e "tabToPairs"          "7XNsmym7XdJvy2GqXPqLnFDgHaNbsh2adrFZBQrunB3c"
    , e "tabFromPairs"        "2nZo6eGbh5n6J5bFtiARdjQM7PxnMkyqmjESi6CwyDKB"
    , e "tabToPairList"       "9RUjLxcuvjcbiSq1qdEPH4Q5izGYMAZsob2gR4FaWMw3"
    , e "tabElemIdx"          "GHMLaQCegBSBZE1fhyHy9GK9eH3SVqGeymT2bqcMwUTK"
    , e "tabSplitAt"          "EhTECLAbFmZ5gSMPpvGwa94DrBiKMwkbUtRmPYbgaeMR"
    , e "tabSplitLT"          "DBfF75LcnhSVyDaSGmies1hJJ3owVbY9k4cZ4eiNV6gJ"
    , e "tabAlter"            "9WZAg1ovk8eHsgv9MSG1YwKE9SvAKQW3VoirKNThRTYB"
    , e "tabMapWithKey"       "A1wvs8bJPk6nKVTGHikw3LwwnmRVTCSXEZF1xCTtnFGk"
    , e "tabMap"              "5c6A33aYeUyTLXMrB4wmzPCQasYP6sJhmPLxvFh3Hiw6"
    , e "tabUnionWith"        "6jEj6QQ9Dj4jgUCevVUMkm3X9iugso6auvJ8nRMyF1A9"
    , e "tabWeld"             "4UdV8haVduixBjxXVfTZ35BwMrnxRv9vzPa8CJ2BKcyZ"
    , e "tabMinKey"           "5e9RqAsaYgy7w8yt1DmgLN6e2nPUXqBT7ixPNvnG6MuW"
    , e "tabFoldlWithKey"     "DZ1uBEGqnQoVcC1sYiPSsSvZhfR4EZfghZAanHsag96M"
    , e "_TabFilterWithKey"   "E4LJnq8a9o3oiPMKRNy7oTRM8yaz88kxA92JnxRg6LDL"
    , e "padWeld"             "H9aQ4XZ2PnsitMydvWYwKVav2KMtDuKBqpmvqiQNMYiK"
    , e "padCat"              "HrCM2aT94yGbCgNXbtx2ufCkpCqRPVS8PQ8hvubHh1ry"
    , e "padFlat"             "AQ1Lemy7HdgF3ZvjdhMGH43q9tMEi6YMG34Wcey7yCF4"
    , e "isBar"               "9w3juh7uDkVsXGoftFj4Xi7zxoZLRoeKG9e1crjvbBFa"
    , e "_Bar"                "3rEdAKuWvV2VjaZoXae5fD1H3mAqkLJjZhqSp2djP9ez"
    , e "natBar"              "8c16DSgGjQQU6TUZzabXnySwvq1GJrhtVpuAeEgo9izt"
    , e "barNat"              "FZfRKbBwmrk7FJjLdfxFJkcToywJYdMBABhQ3fa5zeGP"
    , e "barLen"              "3iayc5dkgkASTLshZ4tp5qcam1cg5QzTidmC7d94UK3m"
    , e "barIsEmpty"          "C3NeHpA2Uy4a4qfLPiLCANQuiKw2M7pDXSEnJpHgAEaD"
    , e "_NatToSizedBar"      "3kcNbAks4Lxjozp2phFVkXDh231DY329D1mVwboyoKEi"
    , e "barIdx"              "DVrCoUq4CT9aeVZNZNuf46oQNocMJKYStJzuXARqGG5"
    , e "barWeld"             "GKNvrpNsj8AfSFbSX5wQ9UoyUNRydJsTdnaqKvQoiV1F"
    , e "barCat"              "2GiCQPqbaH5bt5JqMHBqK86v4VR7goiAq4cCpd7Ndy7P"
    , e "barTake"             "Xa9osVD7to1gmL6voNzhEdex5CZ4QcheXy2jRrguNNk"
    , e "barDrop"             "EVhZ4xYhRMS4k3vvtQHJG3Dor9DwFVddA1QbDZfo6AFa"
    , e "_BarSliceToNat"      "HhXHRPT2Zfr3PGqMnC33aMds9GBTmVQqcZkhyrLwPRee"
    , e "barElemIndexEnd"     "4vhgKSqURFrjzuroKG89NCVXKs5nzv8p6meLBeQW1wKv"
    , e "barFlat"             "52WNYBPPZGkeQJM9XDocop61EEbgE8tEf5B77YP7nPAu"
    , e "barElemIndexOff"     "8S6eun7FPwZTmJcKjg7LMWYjneQGfZ2zPMj4UrJXT6gL"
    , e "_DataTag"            "EDQm1rudzF71qQPiAERwabdVg3drR91iWvZ5wswYBqpP"
    , e "_LoadSeed"           "92ceuo1WjZSytsd8A9in5RezD3WqyScX88G3tLBL1cdk"
    , e "_SaveSeed"           "HvnmLkP5NkCj6odL7iymwUQejP6vCYYavmfzGzQo27Z9"
    , e "_LoadGerm"           "UBmKGsA3PRdkmLSfSFhn6BCVBxFJVENVzQd1XfLnYcF"
    , e "_SaveGerm"           "F5JCxtXGpdVjh2zLcUFGLMa3L1m4ixsXXYqYaLvaEqAU"
    , e "_TypeTag"            "FLM2g5QEBp83BzPjdkuBjyxws4gZY3ZSa8wsATdrVXyB"
    , e "w32"                 "25gasDA9WjWeuLoBYD8K5Wj2xGRnsXS77DtcXv2EvxTM"
    , e "add32"               "ATEFLckL8eZTcoKxveuEmnHyipVkh6P2hn5vmjec8AcL"
    , e "mul32"               "od6Z94otKU7Yt7tj2aoVPz7P46VDf4ahdWqPXwhenkf"
    , e "div32"               "7dxJrbryaPV6PSBqBQRJdrmfhUG7KAX1nxx5y71omUnr"
    , e "and32"               "F3Ea6ATuT7GsxVXD4F8eZtaTvWpp58N9scB9L5q4aDSb"
    , e "or32"                "5gN9PvMQWctYVd71hUtZ7RpYL5sjytJwY5rDbLUCiteH"
    , e "xor32"               "581LBZjTnrt25xbZLz48E6JAHPsCia3Zs9eA2VpN6ctF"
    , e "lsh32"               "B8B4GPwSnjhAyL4cBzRQXpjrRYosioYuEy5LL1vuRuP2"
    , e "rsh32"               "4Sp9fvXUNm5dyfRF282nNqLZdb5qFmRBeJvPaTtvvoT3"
    , e "sub32"               "6RM4SHHbXnUzN5rezZQeUavGDhfSgATkWQnsLinifY86"
    , e "ror32"               "A1kJ1BF7URmLxYHLQTEoYmgmq2XbB7jUDSugyKtPqWTe"
    , e "rol32"               "3LmQy2NKQGhTSfEjFigbxX49nx1FAocjkqXm42hMKWG2"
    , e "w64"                 "Gmkhocdn6PjiaRom27DBeYZ85pSWyy26TeMva1Z8nxo2"
    , e "add64"               "CTD63hHuMybVg6G6foDg7YH1H2LxDs4i6rVTSk2eNCqZ"
    , e "mul64"               "AphXApov5VYBpNGQDxyeyNav58i8JMJf3jB2aY9qBjxD"
    , e "div64"               "A12PreDCbFd7CECMoVWL6sKseNY9ssLnRFzTtrUSSruQ"
    , e "and64"               "FrVrX9PQfgXzTv2FrWioMgsaqD7DY914xz1URKF6jtA4"
    , e "or64"                "43QxcgrCUb42FTfeTNLpp45jxWyLgHxu7YCK2FbppKpj"
    , e "xor64"               "G4baDxBKYaVwhF6UTXngZPxZhjBWSJFjFiGayWapPusw"
    , e "lsh64"               "CER1fr8eCGTnUrNxQyVySAjiTh9RDrL4oMeBFNVLrac2"
    , e "rsh64"               "Ejj81JYTgG1GNZE84ZLHv2EhngMT1DM2kPvn386YrVmF"
    , e "sub64"               "4NqiUsBLrqP36nTaSdPFK7iWY8m85e1Wuj1v7jP5vNMH"
    , e "ror64"               "EBugSwEYsdUXzcpJGky9ttmMcGNrjya1jZP5N2TNc9Pr"
    , e "rol64"               "FKeawm3EuMARJSuYrL9eTcGeAcY9uFbM7NCV3k7E4gLf"
    , e "iDiv64"              "3nFrKyAiccHSG9qUBu1af5DTNtmwZhLvu17CUQPfseYC"
    , e "_TryExp"             "6E1tAVpi3oFLZuv3VVGH4iooFyevHkFKbHucVSvxhBLo"
    , e "_Try"                "9NiZfUVCt53uQbWbpdonaHKjLpu52gABZzkMvYVNf6aA"
    , e "_Blake3"             "FaW2kace7XsSCrhaH9PLdiTPbJS2ivyZu3fZE9QHJf2V"
    , e "par"                 "BfENjJMJwCJxcXUrfMkv2Z5Mpyt4npKH7aZaQ2BCDMEM"
    , e "pseq"                "3oUDABdakHWyyZyPQVBjRtGG3dXrLG3FdB1P1QR9V4os"
    , e "_PlanHash"           "ER2UK3ekTGZHpTT1HFwgKamiX6DVs7KZLyVAX3WmrmGp"
    , e "_PinHash"            "Cs5iguLfmot2NNWSb87zRXnimCvQr4rPWXHcDTWfUwYT"
    ]
