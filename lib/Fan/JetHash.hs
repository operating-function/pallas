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
    , e "_Times"              "FWYFvK5cNTUB1PAsYNvgzidAFxSMDTrqch7UsJjpJbvJ"
    , e "_Add"                "QeTTm1EFP1r7mkXJQVnBYUW2QeKUvjNfYA5PiGzFRVj"
    , e "_Mul"                "E2TXQPG89ykcNK9s7XSwDhvgQsUrc8ogjvoPsEZu7zyp"
    , e "_Sub"                "xruy2CzBgGc8fbn6wYDw4ofa4XXNgCsVGhySX6wr7LS"
    , e "_Pow"                "ENxYwA7Jp7wRsyaR8sHE3SPF3MoGzkmHWPC9g3ohSgR2"
    , e "_Bex"                "DGnbYD2zjfxzUt8WYxXPELfu2bULGhsjVeSyqBJdn97S"
    , e "_OrdWeld"            "8ZeTyQLBT1MSmfbD8ZBdaXJHzwv1MEpL2DSZfRTcyY8h"
    , e "_DivMod"             "EQWKsykhkbhSSa21fntij8K9bichnjWYeZwD2jPmu87t"
    , e "_Lsh"                "AF5txxxHqUzwbcECyb35uJiZzJSGS2fjSXu5uEtAzAoA"
    , e "_Rsh"                "4amLczv4X8RdJ9DRhkxkgNNndrd88yLa7WvYMQUxVmZN"
    , e "_Bix"                "Hjkr9TJMhDbJyGg9ySLFNC4BsjCqKygeUUbdv4nSgPZ6"
    , e "_Bitwise"            "6ikjA7QNUCFyFEDRxpn2UfWG24hWY6dux4zAa7vC9WbG"
    , e "_NatFold"            "AeoswpqhThZkaZWgbfxs1ZzaExMnBBvaqkwn6dHK6GdG"
    , e "_Dis"                "6qhd8cehqKB8G2KPVcq7GcfG84ys4bbMQFJRsNsQmT4P"
    , e "_Con"                "4APYFW36rK9Egnv8R6y2kdCCLK8h4mMd6XCnM2BrPngk"
    , e "_Mix"                "4yiULWrSVRiVveiZJs8r5wsQo2ugneZjeLEy976sa7jf"
    , e "_PopCount"           "GpBW6JPayBXGfWXSmymvgwPe8fDfDkBbzGHhPk1mMm85"
    , e "_Met"                "7jcqkHycHjZCaqaMA4FJswZvJtqLnfWFHtFN3Loki4XA"
    , e "_Trunc"              "GutH3EpWM6DXGp2Vbaa8RPRDn1VQdZSMgpSy4b3XHyM9"
    , e "_BitSlice"           "9SyrULWxbJPnb4ECsGqzkhRyUEDSbd1mCzoccWRSSshm"
    , e "_SetBit"             "3tKzc6g5fMaoh8wgSH8yTWSZyQxmxXEpYxXFjRo9RXpQ"
    , e "_TestBit"            "BG5ZzN6e4XubJH9YSwFdQfrqyVUdiekqD2JuJ72P8gMm"
    , e "_ClearBit"           "7boapdprcCWHitKgjMsww1Q6nPdhEvnnUp4yudMSR9LV"
    , e "_Cmp"                "6MtBxTQmFyGU1nSGVLeAvefWjEmhA42MwcfDH5L718iR"
    , e "_Eql"                "Adf29h2HgWaoiya1ZWMeMp4iMQfeUnNZzbVyZ5ocNuUz"
    , e "_Neq"                "Bsn2Gzwu5akm1x7L6pj3QqxgL1x6biEgKxiS9ahMJM8E"
    , e "_Lth"                "AdoCNFpqJGYuhrayP8pVoqzyfSR7UcPwDt86gyL7oe4A"
    , e "_Lte"                "3ERFoKQNfT27Q9uf356rXiUA9wuBxH1EX2abyZmikeVy"
    , e "_Gth"                "vzmdUQLNpoqDJxoyVpU4ESQVZg4eWfv9ye55MzHH5AB"
    , e "_Gte"                "36re96oJE2udJvUgERtaYYFWVm7AJNHe8pVufjoXkQet"
    , e "_Min"                "9PS8RSccDegsbr8VBYX7gESoEYV97nLer882MvdyGpZG"
    , e "_Max"                "8YxA9BhNBELQykHmLTg2dKZvZ7mxm9W94Ra4gQ9YXbMy"
    , e "_Null"               "B7Yd7cPdoDFsLgkYmuWdsvM4NJfMSmasj3Jhp5agrFbF"
    , e "_Head"               "qdNQEg1BoJgcRm1qRGdNqDK86bm7XSean7torY3Q2XR"
    , e "_Arity"              "2jz4zz3a621Z6uhWSpTbv2wRi91aQfsSRn76Yi6me5gy"
    , e "_Len"                "AUDshrNCtvjQfRYsYHv9s4YJDZ3f7P5MKwPkLJrNX8aD"
    , e "_Put"                "ACEkgfjSxxhvXkh7iyPg64Siwp2fpURFFREkX935v1pj"
    , e "_Get"                "386VJFmFLRAcsu3gXtsNK4s5muQusWJ4SMzgjjS6YUPf"
    , e "_Idx"                "H4Zc8hKR4nzf2w1FC8mXQmsmMrNvJ86yRh2hLK9GL3ds"
    , e "_Mut"                "4a8fsQF4NQfaKfKpTzk7vxu2DaDCuDPUAtedb7k6nCua"
    , e "_Last"               "HKg4NMDUSy9FkBNmsdNvExNHLf6VrL5xAZrJzBHvxxQy"
    , e "_Switch"             "3vzrXZQeTFijPUj8F4rF3QWJnzbEEMNZbjmGsustvzo8"
    , e "_Cow"                "3gqiFBpFd6UwEJSqH2MLPE1s3C5hTSra5uF6GYzsW1za"
    , e "_CowSize"            "G6gN1nJbBtohntPrsG3v6edTtr78MMPJLaehkQ7sLcSL"
    , e "_IsCow"              "J9D6GAKqFiyfbzyr9ejHExr4avfRHS9EycgSTqMpk77s"
    , e "_IsRow"              "8t2nzyaUKEB4iXUcLVaLwFLDHJ6tddQK7Dp5W9oZQLDW"
    , e "_Gen"                "2Xjoe15B84gukTsF7RFCzQ9AvWq78pwRTgyAt3zRXdRC"
    , e "_Weld"               "2pMmhrxJDkduFASR3WXHPpZCSyQhwsSttmBSma8NALnf"
    , e "_Map"                "FZF5tePfHzx5fS8AKiguJFVNsxmru7qtW2gx3XnDAtse"
    , e "_Rev"                "JD4CrHMtAntrmHNK7MZCZK5JDtvw4m26vGsNqy51qq6P"
    , e "rowCons"             "Dnf9n4wW7M7i4Xc5BhXQGCpTC5BhUM3m1wceckTpA1fK"
    , e "rowSnoc"             "A3FxtWw4MGyq8T6HLLvnDEJwkrAej7MCHVVtUTnzXvZm"
    , e "sum"                 "F6ob6mjnYAtCSdJQRFNWQGa1NVdSNbr5b77gESTPCFDT"
    , e "sumOf"               "ahTFxWPHYCTdTKNW3ES6TTvBE3K2ZNrXs7Ta31atipQ"
    , e "cat"                 "89EWhEFWgFigSEwzcGfi3mAgDer1QTWspWWBMEVg6Bns"
    , e "zip"                 "8xVraNf4pPd2o6A7yYKV1N6oANPazsMzGCVXLccrXhQe"
    , e "drop"                "45ARChFpRYWa1o1n4jxYbqVvBmkWzxYQxCFPNkcnE1d8"
    , e "take"                "2gABh4WVzRnyRtdM49QGP4GTUGNidMercKgxaFDonvfP"
    , e "unfoldr"             "AqALs9y4dSE4nUxpGv6vYjqtq4eJL9ZXQSeMb9n7a2vK"
    , e "_SizedListToRow"     "12AFprhZ7HSa4451hkhveQTVCmuds6kvmLYmjQZsDUNf"
    , e "_SizedListToRowRev"  "EXQjpxzxxiLqW3xMwKqRYyUyFfxbEpbvfhRMsUBDmxtE"
    , e "bsearch"             "2WAc1bC8vumCPGRFDWwXxxyMBc3W6K38NaqNq3mgU4sy"
    , e "isDigit"             "A55EjKavUf9P9wSg7np7yqng75XWmcSURmmbAAaLTnBN"
    , e "implode"             "J2Zd3oMTR6Apap2kSBxntwwT4Gog4zbXKEsDv3XMpL5N"
    , e "_MkSet"              "4RUfSzoWUyiYQ5omEL7XWABd3bCpp6Q92Gx62fdDvrVu"
    , e "_SetToRow"           "3GGGFTFjPGAJp75fDidHT2ir5gjRY96P9qhPAEi37L2R"
    , e "setSing"             "2S8yfCmRiecvoxgLJVJg5L6MxLm5avjoGqGJmnbukt27"
    , e "setIsEmpty"          "BDgQFXd9VbEEdoBcaqMys39ucwLS4WFhQLAxypncGj3W"
    , e "setLen"              "4yZ1gLVWzH46af8cUrqhJiAZdSNTpRMPw97JVZH3Z6Ya"
    , e "setHas"              "H54z14ukWmmsPWW1hVvtmg9LgHXXqL3CDAxat7hxHqZg"
    , e "setMin"              "99RzLRXhcLMYwmi3Z92BzxyP7ByJepiSNbMhH3tYThxr"
    , e "setIns"              "7XTSXHE3H5naGmKM1pHkN5K8Y5YCW2DXGKNmUZwSHSjW"
    , e "setDel"              "DRtJToYcbs4CeWJbsim6AC1wfQAvhawXaXBTRf9j15em"
    , e "setWeld"             "DiwkKspYbznDoaAB4hTMTeqhB9CWSDsxnAYU8jaXrb2v"
    , e "setCatRowAsc"        "3FxDUjMRkbL5YcH2VKrEcQrKqt1rxYjpE7jLVULvC4du"
    , e "isSet"               "Cg5qw1nsmxJZvrxyDcQkxvibHgARN5kwy8KHLqY2p8JR"
    , e "setDrop"             "59s1CVBM1sVCsu8goeVRtcZgo3MfSpsNpURxcugnJz5S"
    , e "setTake"             "6gHtpymH8Dr9mKh3ZVWK131L2xYXcn29PHUWokh3BBoC"
    , e "setSplitAt"          "8bHiTR25SEwWF44Jm4jxPb3dPpYjDobmjDYiJAkH3uXS"
    , e "setIntersect"        "6tABCWy2zvjbCbFyEFvrHBkJ54SWVpEGCymgZauuCeAf"
    , e "setSub"              "H4X1JaHYoS8DUbSnsevKrqu8LuXebWiARUEoppmYL174"
    , e "setSplitLT"          "C2ZGZw6xkURRqrDEVTyhYRCx31GRWKuDWJVj6KP91ACn"
    , e "_MkTab"              "5hFXwrFMAS1ETGYZDANKd5Eo9xkjNnxPRKTL7H3Td4yv"
    , e "tabSing"             "7AfouhmiExP9KSySjrw4k4VZ5JY2TE4cW53xo1EhHsfz"
    , e "isTab"               "3NVgkveHYdmX8AHVLhwB7qpKhtNmFanxv4c6z3QuJqEe"
    , e "_TabKeys"            "8b5KfdDnpHdUNavaYcVkyY4UWvgkmzhboa2WtHq8hvE8"
    , e "_TabVals"            "ChipTE8EqGJx8mKh7vKk3uLCoe87uzr57puknUVwGMGL"
    , e "_TabKeysRow"         "AbTABFk1h4qcAoW2nzQ37JtnzPfVgapUSXLBcZqid4rH"
    , e "_TabKeysList"        "HJCWTAgwp23pzpsfcFtztNa8uaNit6cSdYN19CqmejyZ"
    , e "_TabLen"             "4zBRGTdC8o8VqyVbae2RH7HhCXjkzahcyMvBKPegci3Z"
    , e "tabIdx"              "3TWf15DfN9xE991EbwQooXnD1wMQ1snqJBragTQ7vqai"
    , e "_TabIsEmpty"         "2pqKJoYtYzxGuojhjcD7APfxGauDgrJr6frjcEKY75Cr"
    , e "_TabHas"             "3CmdokqP7UZsYmUibrN8ZYenxxYNeg7pbujqL87uf7Yh"
    , e "_TabLookup"          "4ZU8AC43kfUmio84VTUWxZzkRFLd9PUcn3FYhacStXmn"
    , e "tabIns"              "6DntBfKNxnzojW5jz2HMm3qKMtMhTg5rzVyQ51zXc5uy"
    , e "tabSwitch"           "GAEvH1zJXuT4fpX9rhnonBPtQ1wxcDmgXgyz53vZxmSa"
    , e "tabToPairs"          "7XNsmym7XdJvy2GqXPqLnFDgHaNbsh2adrFZBQrunB3c"
    , e "tabFromPairs"        "9vDNgbSC1eKiVfti8GnjiuBLYptbW8Ct6NY48y14sE7"
    , e "tabToPairList"       "5BRqPnTopXM4gu9fTnrnxoDVWYZLiMK847eRjxn3tUj4"
    , e "tabElemIdx"          "EKW4vsYbgp825gXmTmbuZHdJXvJJg5ZgMZLDhhTd5zQr"
    , e "tabSplitAt"          "HSJeEynaytuz6Har7pzrD38a9fERYycpCfn9DdWbsoww"
    , e "tabSplitLT"          "9YdjLVV4RVRWNEd7WqFppRKrnT2A5yYZCzrJYKwQyFos"
    , e "tabAlter"            "GCz4BwUrQEWZpsprfRNJdrb342iSywauUKiGQ8usMsLB"
    , e "tabMapWithKey"       "C5KJzWkVKbKe3BkxrE5MKEMPywwqjtS2Wg3uYAdhWxuX"
    , e "tabMap"              "9d5ZFLjQTJiJWZ5nVu2HPERX87JE6useFs5hTLBxsEud"
    , e "tabUnionWith"        "GwJdmG4nEoqdyQk59881fJEyXohj1FSRYUtJvtNUZb7t"
    , e "tabWeld"             "5b6a45ecNGARY4eMDgoLBPR7rS4pLgXkYjSBsBeEdeHS"
    , e "tabMinKey"           "5e9RqAsaYgy7w8yt1DmgLN6e2nPUXqBT7ixPNvnG6MuW"
    , e "tabFoldlWithKey"     "DZ1uBEGqnQoVcC1sYiPSsSvZhfR4EZfghZAanHsag96M"
    , e "_TabFilterWithKey"   "LmKep1jfDMA1wRJUtMa4vQsotSPsrxfQeJySXapCzt8"
    , e "padWeld"             "41LTsmvWgNvSNaQiTVKSJneVPvJ5yTBuQZeS7PDDNsCq"
    , e "padCat"              "6NHqGZj1Jc71V5325khmt92eCDGy7Lw6R3GRDnhhuQaX"
    , e "padFlat"             "H7VTQKkjahf8pRm3DV3GqpmhXXZDprFLCzexQyLmvK6c"
    , e "isBar"               "FqCBMbru2EMfpYQRqctbubyKwrMjrimE4B8TqqV9vUPi"
    , e "_Bar"                "3rEdAKuWvV2VjaZoXae5fD1H3mAqkLJjZhqSp2djP9ez"
    , e "natBar"              "Grich8r5L7Fgsh6goMNkPFqEaXBPn9vd11qckWJcUdZD"
    , e "barNat"              "6n4mhXwfMVzNaDQvz235SZ7CmzSxtkQYtWH4wRB976Pw"
    , e "barLen"              "EHfmWosxFz8fsdrCj2ioV7DpLcYExrXHYJSAcxc9q8Ks"
    , e "barIsEmpty"          "8VoyMabPUNASM5PvxrUUVca6gk4Gf6HRvUHiFkMHuMw6"
    , e "_NatToSizedBar"      "CKuuEeyrhLLJqabF1zT2jEt9VjZPk2uKrHcPwRQL4Tt2"
    , e "barIdx"              "GbWSreSXoJCuuKsB8i6j1dKQoeh5TeR7kzg6h7RH5hQn"
    , e "barWeld"             "3JVkX2SZaRxftcDasegcTXRKtA6sgC4H2NSLj66NKwoZ"
    , e "barCat"              "DiRPCs6VVvuYH9iBru1v7HpZLDg5S4gCWxngCHQRXWMb"
    , e "barTake"             "3yAJY5En8J4RP6Cunwd69CSR8shuDyFckJt9MU1g355P"
    , e "barDrop"             "E2X36NvDHYwtvt4xn2jKZ1hnbsUbk3LuRwp6SazYq3kj"
    , e "_BarSliceToNat"      "8NzxDkMqffNqNWMzNd4VCRjgmjX7mLY7ve23YvLzPckW"
    , e "barElemIndexEnd"     "GAC2h1sXDBF7Vr23fc58YBNo7aWAKPa5sP4cLZ73zcP1"
    , e "barFlat"             "B3Wt6JQTYpKo8DKzkmyLB4AJf6Qgb5e8qt6AwdDJjAxj"
    , e "barElemIndexOff"     "64Lg8NDX7jjUHew4qbfNxEyNKHkfVcAbgHXzffTZVdA3"
    , e "_DataTag"            "EDQm1rudzF71qQPiAERwabdVg3drR91iWvZ5wswYBqpP"
    , e "_LoadSeed"           "9XQgyapLqjSW3kE3kBd9fmXt8yAoQQCxcA9yjWaLKpvb"
    , e "_SaveSeed"           "3SVwf8ZJZGfKDQFwMin8KWXYePgGVYupnhpjxjhWh4Qw"
    , e "_LoadGerm"           "6YupW2Gdv8aSBeMt2p2Gr3jR6dxsY9CfsLM5wKpckaHg"
    , e "_SaveGerm"           "J51XMeqFUmNAnMHdxAKuY8BdAk69BMbodqtx3zXZwSzT"
    , e "_TypeTag"            "D3CzbHoZ1hhYGksHiWvamUq9yKCwkzB4Vz2t2pkLc2vt"
    , e "w32"                 "EQHmXXbt6Cuc3MSPHFZKfVTMHAR5x9rbbQr1YjTaeXim"
    , e "add32"               "3qYxKU3YkUsX4SQ6g3cdhJ6Znfg3bg1vWdZns55YD88J"
    , e "mul32"               "7RJLAqf3pob25f4bnKpEvUKye2CWF4t9YzEhX8nYXf9r"
    , e "div32"               "FWUUBP7AxqeLdS9A2c42VdACCeZA6EwYtaxBEf6TwuYh"
    , e "and32"               "EuGQdB6fJo8XzC9ikrXyn4VVLEn5pgd8xzaax2yTQCzG"
    , e "or32"                "6VpayxGQDakHeZwR2KjrRDFNyZePjV3MjtD6YWBA3713"
    , e "xor32"               "3hSRBo8CvXXH3gTSqx5sumSzpodvHZcwAB8jVwJZXDHH"
    , e "lsh32"               "J5B3qU9p6DrQPYb6rXpW4vP96uPcXdZqwnYUDVijQy9a"
    , e "rsh32"               "GxdgkdtLi5y8A4bJ6SNCAD5c1kAmynQ525NvMDoNwVKm"
    , e "sub32"               "73L22gzKsw2NhBLgeUaZJpxgWaAR8Earq8pTnRUEbSrw"
    , e "ror32"               "CDHrzX7dRZsCjhkWoA4gPucos76Xj6xEnfH4ymtzNMsg"
    , e "rol32"               "97zzSCBKCFZ6FK64gyz1xfSgo8RhG4YFfoWAieoYdZzJ"
    , e "w64"                 "81PEAAK29fy7kAodpzDsgroSYtywYWXzUuMNpzyELkmL"
    , e "add64"               "5TiQxKr3q8oS9eZE1Mq92x5DpdJYHJgLPcmxWNUnjvaw"
    , e "mul64"               "CMuR8runTgusm75hDmsGyP58Ag7eTQbrvrBP4eBtAa2m"
    , e "div64"               "FecCJC5VAKJCBrq4XHJcGFnoPRtA5JixBwSw4TFMmEy"
    , e "and64"               "A8CwNJL6At1Wn6mt6yW915ThcLtjTeHrN5kCmfgKCsvm"
    , e "or64"                "CnX4AJPKS3izcJ8C7iUs1GNvrKTRxnXmwseostZ7tdET"
    , e "xor64"               "4qzsWv9R8dDjwwTRtz45BWSQSGoLZjKECWE33rPPXk6T"
    , e "lsh64"               "BJu5BACH6awRB3LsW9MiH3NdqfV9yqNnsQrWrC3CGCBC"
    , e "rsh64"               "9pvdzhPYxbRxjnFvNwhq7gSYog6zshc12VLk6Lnn4xV9"
    , e "sub64"               "AXQU9DWbkxY75u3W9TQAb5fNVXgHp6e93bR9AEunR1Uu"
    , e "ror64"               "6F4YyudaaHb9siSxo2DcenYZSsfe3LMYCPV3wd3CuMyU"
    , e "rol64"               "AiUXtP84ssfMrjGPnsj1n6cmGvVWJG6CWTSHYvNXwZ9M"
    , e "iDiv64"              "HC3iUa6rHfp1JqwhcZryFW6freFwFZFwDMobU3jmZe4y"
    , e "_TryExp"             "FpKGptdncF6G5z68HwVzwgHNQCBUB6fMvq451ASsRAcW"
    , e "_Try"                "3UkUWS7GRBMmhqMx2fokCLCv3SgsRZoxCUtoiv7RA7Qr"
    , e "_Blake3"             "4ZvKx5j6VWesVTq3rPW9xwzTvEbxRQHt573DTtdUeXeg"
    , e "par"                 "BfENjJMJwCJxcXUrfMkv2Z5Mpyt4npKH7aZaQ2BCDMEM"
    , e "pseq"                "3oUDABdakHWyyZyPQVBjRtGG3dXrLG3FdB1P1QR9V4os"
    , e "_PlanHash"           "4bqDkbxyPdaTcmysJBgXk8KuvQ78dJUGrf1Ptn92LwxF"
    , e "_PinHash"            "2iQ57621GVFbN8zXboARGJyxKn9zRuBYb15V3qCpUftM"
    ]
