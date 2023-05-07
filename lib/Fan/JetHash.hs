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
        [ ( "if"        , ifHash        )
        , ( "ifNot"     , ifNotHash     )
        , ( "switch"    , switchHash    )
        , ( "seq"       , seqHash       )
        , ( "trk"       , trkHash       )
        , ( "tabSwitch" , tabSwitchHash )
        , ( "idx"       , idxHash       )
        , ( "get"       , getHash       )
        , ( "add"       , addHash       )
        , ( "sub"       , subHash       )
        , ( "mul"       , mulHash       )
        , ( "eql"       , eqlHash       )
        , ( "eqlNat"    , eqlNatHash    )
        , ( "lteNat"    , lteNatHash    )
        , ( "lthNat"    , lthNatHash    )
        , ( "gteNat"    , gteNatHash    )
        , ( "gthNat"    , gthNatHash    )
        ]

jetHashes :: Map Text Hash256
jetHashes
    = validate $ tabulate
    [ e "seq"                 "sb965zsyHBkHaowAjiXcTMt89e3kvnq617Hsf8DRi6S"
    , e "trk"                 "3EhMetnuFhx9zeMRJS2g71PybJ38btaan8pfwfQicdNJ"
    , e "isNat"               "EVzz1ZxHmjHemGqNnw2rPj4RPBnhRPj5MHiCjpFz44C5"
    , e "pinItem"             "BMEtyvmeFgBQm5fM2rPwtK6kQiraKtUw2pB3BqWdCt7t"
    , e "if"                  "9cFk4oxzbH92Bi8jgw7k5LYJNkhTLBxAiQfJ3x3baf6e"
    , e "not"                 "AhoArogkDqhsucCZH4seXXmLXr1a6b4z16gWm3jRiPLX"
    , e "bit"                 "GYgw6LH6KVRiYLFhcWrVpfAWK4PBuWWbiNYCvfx1FLFm"
    , e "and"                 "GiRoy1SVANxdk4cDQn6HkQYVVWZea5kovi8knYysadAX"
    , e "or"                  "63SSHPno9FG8uTkcSq5WGuu5uZgD7X9AoydRz5NhMzjv"
    , e "ifNot"               "GuMQnrCGNsTTj75eQ9UNpdEcj4GCTF8UqR1Ja69iufbc"
    , e "dec"                 "BKm4tM8GRUdHC26Vi4NRaxVVQrHtAy49om6wjB4kkD2Y"
    , e "add"                 "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"
    , e "mul"                 "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"
    , e "sub"                 "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"
    , e "lteNat"              "AhChaxy3yaRdMSLWzuhVkWb5sRHMtdUL6gV4FNNafE6g"
    , e "lthNat"              "2BDyomgtS4AkZutt77LiZtmipwPuKEYkPcdz6tr9URkD"
    , e "gteNat"              "BbKRjpR2Xs1XgGUb8ZSC1u4VceveHtyEkbo5t8tyRsJL"
    , e "gthNat"              "8itNV8d9SwGrkLb9VYE1oS9L1BsnxM1JtSehYvbcL57C"
    , e "eqlNat"              "HinQSPoGNvnL6DG3mkeKKtiyCTnCYaS3oNzRsGSw3Ccx"
    , e "cmpNat"              "VPtpxVf3w6zEogUcVdermsoD7kNzVea59TedJLwsse5"
    , e "div"                 "De42z4xU8sRe5uP8DmngkCR2RKSXUX6nUNvyGcCDLbyb"
    , e "mod"                 "HwFtJwQqtfdLsveAKoFQyD4sAt21S7pgYY2LYnrLqyM8"
    , e "bex"                 "DdmJnDFakbPhNoNnQuYRarPXMt3aaej5VLarUaja1PE6"
    , e "lsh"                 "27LCe5bqqWsF8CZBSYvHHK5wirqPueTbtJpaFiYdM4da"
    , e "rsh"                 "DC71Care5w6UeJ9cRMisCqX5DW15mNpY6QPR65HhaotT"
    , e "met"                 "2Xkc8mkHBNGrWK3ZW9AVPTADpVg8Y9jTXahyzpdWMj3v"
    , e "isZero"              "3TsXB7wm4ykMStjVZ6Tk6rVusvUZBrs14rqhwVyLtpaL"
    , e "con"                 "FkxnGVWAdGZnfNMsq2AuDB6S4GFxewcejBiCjed7Ehez"
    , e "dis"                 "Gyo9HAvR5tU3fkPocqogjWyG5VyEvYgSRXATgbfZKhiy"
    , e "mix"                 "5ZAoD4dCjE4HLwb51C4dm1MG91x1dwkeo1t4ZHhSNHw3"
    , e "len"                 "B1gjnDNrVrLzdB7CsFPkYRukiTY8WUWX7HoEJpgHiin2"
    , e "put"                 "G5nvMctQWMvYfczmoRczc6DTkCTT6bjU8XmpMw1KPm52"
    , e "get"                 "CC4gCjuhJc9bXhiLHSdWkUe84c95bowZac1FkUQqQtDw"
    , e "idx"                 "AbxuJzL7R9fA5xDrvRPoaDQziFceN8hiLTaDWov6bcuc"
    , e "mut"                 "6qnajRQCZDJeYttfLZU8ZB8tRbRoqy7zWA9FU5gx1c3a"
    , e "eql"                 "4AaF13zqQmE32NwSRFyg5w6mUt8cF6vQ3tTBvXfzGLGB"
    , e "neq"                 "Cq2MNChKYooxaEb5jeEAcoZj7wAaoAhx8h3NXA1kbH7m"
    , e "cmp"                 "3SQ51YwjVwzHdAMhbdGd8cpXhF2wTPpnGAb3TVN3rCs4"
    , e "weld"                "CHTjJUgY13hW8AY17vdvyCFKFzfgq5EH4aBKx9QW8Anv"
    , e "map"                 "Ehsb8JEn2weFh7YQWiyeefG9GveixiNfp9JBuz6LiYcy"
    , e "rev"                 "7BsJua6gUM5mzZ6CM1kdLh1MPtyDNSGMbaikAz2vBXtF"
    , e "rowCons"             "8r7XuT5a22K4p4kvVNfyhHjkkYHdMKzA5BEStCmsnbtG"
    , e "rowSnoc"             "6DfjbqQugmm6kK4Q3rLMCJuUeyuKDuJLmRL3r6n1mj1t"
    , e "sum"                 "Cv3L3XZtHM9N4HkU5eDqUeW1KL5k3nNciDyYUsW7AVeU"
    , e "cat"                 "BFMYYBMASb9areQ6vMcsKZJecrXHCNXm52Rby67x81E9"
    , e "zip"                 "CdzYP12RqJBYY1vPRitzPhAFSzs71caJpZXYfT4yg1Mr"
    , e "take"                "BefNozLDA1VAMyNCfHAxjiD1EupeRrJDQRnbw7ETTVec"
    , e "drop"                "wQ8VYmeUKvqH9zCPoxWq8Aqx7cu3rciFCBWpN8n3vTk"
    , e "unfoldr"             "CuiPfmycgu92f1nFgqXYZLMD5QAeYJFpwJruFcckF11d"
    , e "switch"              "At3b6gCssS7MRqh2imQVsjbn5mBQsMe9hnqvS6MiZ9PX"
    , e "listToRow"           "H1G16rpgTZTEJ1ucTfv93Cqsby7acX81dTXVPMPQTsKD"
    , e "listToRowReversed"   "BQBZCKHhCQExLdSE6qsNwXmQdxRqix39RgPU6UnuSSDL"
    , e "isDigit"             "Auob9kWUEZr5JfLU9CgqzSU16E66X9GuBHRNmuRQJRdW"
    , e "implode"             "ExCvhhsfMjpMax5EUQYn1V9SwMrTRWLA4TUbp8iLS78a"
    , e "cabSingleton"        "DWcASbyJYUEEgvdkWN1vghQ9jmBMSPYkxCt7sAnC4qd8"
    , e "cabIsEmpty"          "AUzh3b7pZowxhu8wAPVhWnyn1n1DKDzXYVHSgH6CJqP8"
    , e "cabLen"              "CrqLogZMVTKDzoRK25X6BxPckXfxKrZAVKAwrchjE87"
    , e "cabIns"              "2mqBMGQeMvkNZYyKK8LvpJonK69omaQAvPxw6zJbf2D9"
    , e "cabDel"              "Aa2PbU2mLzSwtPBasofvmQMxtwt9TKkPhaUVpjMCVphh"
    , e "cabUnion"            "2k1hqZ7RrT6aXHzorGkWXYYJSCLjxgN8ZybhJYbcv6J4"
    , e "cabAscUnions"        "B7b3thiDt8ds1nCMtimWr7Un72cJw1AjP7DTLgucLtrV"
    , e "cabHas"              "5H7sMgRyakmBVFCBbnbufcmjDrx1rqNZLzSWmb6uvEbU"
    , e "cabMin"              "6r4HGTyq2jx1Yf3WoZaJ8jCpYyq7d429n7sST4ZuGQ6e"
    , e "cabDrop"             "FVy4F3UUwQDH8acW3C1YKrLc4pmqQW8gg42iKV7QZmVh"
    , e "cabTake"             "Ah3B6q7Q26kkM5nTiyw8Mc8v6ZcaxGDU4pYtEeRSGQWd"
    , e "cabSplitAt"          "BYLZk5ftf8v58t5qA7sykp5PFcSuQsbkcrcaLFBz5AM3"
    , e "cabSplitLT"          "DdR1jeuTQozcyqmrs9jYFvvay92GPkHxC5e462FULSYw"
    , e "cabIntersection"     "Ag6nRVG689zaKEGsUaQXCe8paJ8DBc1pGoB4oViPWb1E"
    , e "cabDifference"       "8T6Z5Xix4qDAJkbsuaVLsfkZF6mPkA2VXpf2VEVU7jAe"
    , e "tabSingleton"        "B3AbFuGPj8DH2mUAorZvS6f2dVmhfzHwFWo7eHd5HR6f"
    , e "isTab"               "5Zs5SPcRD2sxvjZ2j7k6THEAEPnk3mfo76GnHqDeioxq"
    , e "tabIdx"              "Qy2tjdjfGrNc67TzsZjAemufb6gztxuEQHipEW69Nyj"
    , e "tabLen"              "BRqWvk2DMMaVM1QComPknq872J4G1mS9ckBECSoPuCb9"
    , e "tabKeys"             "4KKnTkup93bXVUVJdt1vtQYU84Pprf2Qziwts5fnyZLF"
    , e "tabVals"             "Z25h4Xv1QFPd3LbB1wSjissbut9sWBnwTNdk2We6uST"
    , e "hasKey"              "5jcfqfZxRuDcQ98iNM9yAjWgP7rbJCXes4TT29gevk2V"
    , e "tabLookup"           "9HPaQGWkGMg1BiMadA2jctBws5LJnvNVi2QGdfUP33Qb"
    , e "tabMut"              "FJmNt6mnGgXaDXJGRLtZ4mxNZ16ZeV85EnrUSKVUDPAh"
    , e "tabSwitch"           "D98pz89ixyMaPP9N1deoPTyv2AKMW4PYFiP2VRYeMCfN"
    , e "tabToPairs"          "EKTLxwcuMYbaQoobaTVY9bQjKKScEpnEWVVU8JcmPQ5a"
    , e "tabFromPairs"        "DhiWqq6rpsBNM8jvTDjFwwR31TLMgKqY1k2XznBMnkvK"
    , e "tabToPairList"       "FsKG9e34FfbqYtSZy99LbysKJuDAtqicNxpGCL1n7Ybq"
    , e "tabElem"             "6PDDHZdwaW4j5NyK9xUHbR56fazmK7udiKHzvu6xuhWK"
    , e "tabSplitAt"          "97f128F2xGn4ySKn28ameTjJcpz4VSLAV4MNG3cakVT2"
    , e "tabSplitLT"          "2YHHTNTKFqfqJuzC12qWJpKkRg32zJ9byZuu5bAs4NSU"
    , e "tabAlter"            "A4F8FKReXtVNdQbSnSZNBAHfipeZShAAzApfXzwknrhV"
    , e "tabMap"              "CCL9or3zzYbwsoJ3GrpLnYCxPuGjHZFjoVmngmBTdjez"
    , e "tabUnionWith"        "EdoQkqyKoRtkayjnRUgZCTHGzNo751P8BVCgBZtmjYzG"
    , e "tabMinKey"           "HgCPbZnesoioTthV2sbqRH7QSPZ2y1LABBQJXG8FT9dS"
    , e "tabFoldlWithKey"     "9C8EEN6QGkMNXmUbgVneXpCwU88sQo149iNPAUTMdVY9"
    , e "padWeld"             "9NszwBkmH6VrJJhGqBJZRn7BRsKk2LPDExaHPi1JpsCy"
    , e "padCat"              "3osqZooGXGpb92bb6QFqCBR894Zx3QoDdPmGeSmi1CXE"
    , e "padFlat"             "CrFrgVGQGG9v8N16NzosBfU9ucUjjqxnV94Ue8rtorsn"
    , e "isBar"               "8osqro4gQHZavfhcXasJ46MaZJht4aZREVDE6o3Fd9sZ"
    , e "natBar"              "ALcXDsGhJ3bDvZQ6CB7yTvhZ5w3x7jzNkcXC5wNcydNq"
    , e "barNat"              "Hi5wWvhdEAVGRiKxWAzuSUEFKxv62HRUeoBjrsrxnCBu"
    , e "barLen"              "9XSGwSGydrmZxErnVrTvChcuz8tK1uHc8paCfWjCDuW1"
    , e "barNull"             "5FUHQoEMfFE26d21Sxs62U7yhhw4EwgRnJys5ZbRCRkM"
    , e "barIdx"              "7wkVzDkFxBS4F38nu1A2B633ZkBtnpJA3EW6EzShgnk9"
    , e "barWeld"             "29optB4xLxSEHh1NoymtHXdUJ5vLYUyKW6UJMaMNECbq"
    , e "barCat"              "Agkpk7gYbPJ93CwX9jpKBuh4jmSdfMC2MkX3VEM6aoG1"
    , e "barTake"             "2Bn96tZftAmFUiG4maTUFtggbvBo9gt2mrUo4E7HhGQ7"
    , e "barDrop"             "4WUB2ujTn6suCUXx7q8ufD31fSxWU8m53fWWKdBVSqx8"
    , e "barElemIndexEnd"     "7ywQigYZjnxLqTyMtB3SWZdJFxRyNpHZftGmcRrGfe4"
    , e "barFlat"             "FvjgU5U8aMHKAZxSPviMhhQwVSdz331D9fd1zU6yUVdS"
    , e "barElemIndexOff"     "GxUxUqomZsptSWQCLhNDma3rXRkK2ut4ytAPtcQiAnvY"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "CCK8Q3MBjJtmdinWmrcDsrwKsXnpQMZf3RiHi8KKCfLQ"
    , e "w32"                 "7wSdWvgXnoi4fwLqbfsdacVUs2yWRPxu5UECjbLPHHnJ"
    , e "add32"               "26B2743MLXb7ZMGxZjczUM9sH2vETq6Zb8DfPUU8D4SB"
    , e "mul32"               "4cFK6HPZ112WbMFSigMD2Tucr92h3njbjV1KfVSzGfZw"
    , e "div32"               "BDSFTfF9SZ6EHTrzMFLYVtv6HAxwpdo6Yz4Hj7VTnAcz"
    , e "and32"               "DnUkhsnVpbSD1UKnm2HsaScAkDJnKTerMJ5pmU4bwQjk"
    , e "or32"                "BvBqu7pVDB6HZF5iqtr6JHms6Jx9z4fK2aw89zPWrF2U"
    , e "xor32"               "4G7XBfaC7JjE2xFsZfG8oUCAEA84NacVPP5LAyDygZ1i"
    , e "lsh32"               "Gq2qr3SH8RcUqNCjE6d7qWcEzvfPFomjBkyChoL7xQW3"
    , e "rsh32"               "HNiqkEt5tLSjxdjSPNKKUaz9ditnJHZhJo43Kg6gEFsc"
    , e "sub32"               "9sTCbDhiqGnah3s45iAoj7YPqzg9GsLnXxXTWw4At3zj"
    , e "ror32"               "A5UjMPw5fkEPnryzJKB8oC1YqPLAgwwx2NCAvBwUEFVU"
    , e "rol32"               "2EZfb6m4Mf3Dyhj2NjypvFZxPJNQtCAQujwkazmtFfCk"
    , e "blake3"              "2umws6iSfRGgpcxQphKc5uf4C7cwMdbg9vLwWYbejsc6"
    ]
