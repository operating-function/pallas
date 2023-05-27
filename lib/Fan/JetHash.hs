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
        , ( "ifNot"     , ifNotHash     ) -- use inlining instead of jetting.
        , ( "switch"    , switchHash    )
        , ( "seq"       , seqHash       )
        , ( "trk"       , trkHash       )
        , ( "tabSwitch" , tabSwitchHash )
        , ( "idx"       , idxHash       ) -- use inlining instead of jetting.
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
    , e "dis"                 "9ngqkcS5wzmXdb3uZv5Bh5w4yprTnGfqoBWZJx8YnazQ"
    , e "con"                 "ETD2ii3tZaobMSXt4EMw64inZJc1VVh1hhtV25gn1WVB"
    , e "mix"                 "5ZAoD4dCjE4HLwb51C4dm1MG91x1dwkeo1t4ZHhSNHw3"
    , e "len"                 "B1gjnDNrVrLzdB7CsFPkYRukiTY8WUWX7HoEJpgHiin2"
    , e "put"                 "G5nvMctQWMvYfczmoRczc6DTkCTT6bjU8XmpMw1KPm52"
    , e "get"                 "CC4gCjuhJc9bXhiLHSdWkUe84c95bowZac1FkUQqQtDw"
    , e "idx"                 "AbxuJzL7R9fA5xDrvRPoaDQziFceN8hiLTaDWov6bcuc"
    , e "mut"                 "6qnajRQCZDJeYttfLZU8ZB8tRbRoqy7zWA9FU5gx1c3a"
    , e "cmp"                 "ChWZZiHvtCmhUv3JNAvR1XpQpiLNAC5hpmk2Q9Czs5Mo"
    , e "eql"                 "B6grd7w9xZqFvKGnBZGf5g24JLerQLNS3tqXPFAyWuTX"
    , e "neq"                 "AA5WkRFCXdJTQC1m2HEY1VQizT2bUd7KRz1GFscyBcmS"
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
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "GL36jXMVJwebiPuiAmmbhky7j3pcaNS9Z3vhaWEK6F9V"
    , e "cabLen"              "Dz9K9g51PUKMk2aeFR14zQapS4EmrN1gMaoJojsBfQsa"
    , e "cabHas"              "Df9cHPyev7XgeywuYf1knYBf53AjK1m2cmNtpdSkh5t1"
    , e "cabMin"              "FAmEZdaWUYE3TkSjc8RRJWBHe9u8EzPxi4QmBptnUTiP"
    , e "cabIns"              "2VomER3Td4WaCGQeuZFPuAcDbBxChjtA4spije7Qba2c"
    , e "cabDel"              "5ZcaupXR5g1tYc8PkzarUpyruQJKCF9w4jJd2RUV6xFC"
    , e "cabUnion"            "C3ZeECN8DJKKNGvuqFhRMdA3mySwhooevBvGjhnqRRai"
    , e "cabAscUnions"        "62rQcUhTRTR6i3ynAvMbk5kDrqCUv83GAtAR8eQnsP3Q"
    , e "cabDrop"             "HG5EQCzHkCW5uDjD7EbBBQHoDEaHs5m4PAnvXzben9GF"
    , e "cabTake"             "GwHsi4PBcB38wyZhDvcqhZozGKVFkTbJB6F7ucTVykjv"
    , e "cabSplitAt"          "GeQnDHu3p3jYxLNg9kGUgifoAV1yys3xZW34ffDvQGdH"
    , e "cabIntersect"        "6M7pSz6ZCm5dNjgVtUMqBtVAc5vqeimLjuUqx49rmeMK"
    , e "cabDiff"             "VARsDgksDdWqK5N7CRPPeszith8jxJHvYenDjWEFZMk"
    , e "cabSplitLT"          "2QUtLYCwCarmyGkAxPcKVtss3LWBtm72N9QEd9kpr8LQ"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "Hve6HYn3m1sywHf4jTEGfYPsUi37XdEvT3N1rZxNdNZ3"
    , e "tabIdx"              "8s4Uj5YQWKusJwT1DVmzTQnuMVKCR1YtSNfMuj1hhxmC"
    , e "tabLen"              "BRqWvk2DMMaVM1QComPknq872J4G1mS9ckBECSoPuCb9"
    , e "tabKeys"             "4KKnTkup93bXVUVJdt1vtQYU84Pprf2Qziwts5fnyZLF"
    , e "tabVals"             "ARnxntHSmHx4ExjLJkkVUHrektWsGiiC61VEbJjYJGSP"
    , e "hasKey"              "BCicJLiUkc27N4PvKNvpYPvzy6BNvACQrEaMe7hLNWgz"
    , e "tabLookup"           "GJqMTUjV2V1TZN6uCwaCME7cksb33oA5Wq7nBfro24j1"
    , e "tabMut"              "Cosc98JbMHgMix8LKUMU8KuqTtfAhjUdzAnyyMS3xTsm"
    , e "tabSwitch"           "DiZBoWc4XWv6riFKCTPgHK6cAAiLEv72VXWiJEcNxbr"
    , e "tabToPairs"          "EKTLxwcuMYbaQoobaTVY9bQjKKScEpnEWVVU8JcmPQ5a"
    , e "tabFromPairs"        "EcZSZUchtzLWWMAHTVPUYHXdJx6TckiJYG4NbHkaBkuC"
    , e "tabToPairList"       "9YtKLZubjvo3NXC2n4VnLXse4v1S2B5FicmJB6cTRgGs"
    , e "tabElem"             "AAmF73nRstkV86m7wzr3akBMcxjzHbcimUvwCpyb6yxq"
    , e "tabSplitAt"          "6DoKTPQ1rECAe72SjAczp6w7Tr1aViiRFzGuyBdRLLzW"
    , e "tabSplitLT"          "FLYZfwn4K8s8yY6YfbTQXCcBPenQivQrGz1vPbvPD9ih"
    , e "tabAlter"            "9aeoapWz3JWfa3HXpif6xuWNyAPzUJ48jWXRe7MTXjK5"
    , e "tabMap"              "Es5dC8Yy2wW8FxngELZoR6mXsShepLfqx6XA13CJUhLn"
    , e "tabUnionWith"        "EEuPTJZto3ZnBgHL3hBh7Tz8BCV1FWmpC3ZDEHy9idah"
    , e "tabMinKey"           "HgCPbZnesoioTthV2sbqRH7QSPZ2y1LABBQJXG8FT9dS"
    , e "tabFoldlWithKey"     "D5UUGwmtjy8LSoFZoh9XNgztVZZnq9uCXo7pEtQgMLry"
    , e "padWeld"             "9NszwBkmH6VrJJhGqBJZRn7BRsKk2LPDExaHPi1JpsCy"
    , e "padCat"              "3osqZooGXGpb92bb6QFqCBR894Zx3QoDdPmGeSmi1CXE"
    , e "padFlat"             "CrFrgVGQGG9v8N16NzosBfU9ucUjjqxnV94Ue8rtorsn"
    , e "isBar"               "GVs46LqLUSAEYzTHKPfLnMkaBdtjkxnkpGf9zV1ubbzL"
    , e "natBar"              "ALcXDsGhJ3bDvZQ6CB7yTvhZ5w3x7jzNkcXC5wNcydNq"
    , e "barNat"              "Hi5wWvhdEAVGRiKxWAzuSUEFKxv62HRUeoBjrsrxnCBu"
    , e "barLen"              "9XSGwSGydrmZxErnVrTvChcuz8tK1uHc8paCfWjCDuW1"
    , e "barIsEmpty"          "X5YhkmoqfVomcjaPAaPu6Yys1TmNfkGweKZ4AoWALB5"
    , e "barIdx"              "7wkVzDkFxBS4F38nu1A2B633ZkBtnpJA3EW6EzShgnk9"
    , e "barWeld"             "29optB4xLxSEHh1NoymtHXdUJ5vLYUyKW6UJMaMNECbq"
    , e "barCat"              "Agkpk7gYbPJ93CwX9jpKBuh4jmSdfMC2MkX3VEM6aoG1"
    , e "barTake"             "2Bn96tZftAmFUiG4maTUFtggbvBo9gt2mrUo4E7HhGQ7"
    , e "barDrop"             "4WUB2ujTn6suCUXx7q8ufD31fSxWU8m53fWWKdBVSqx8"
    , e "barElemIndexEnd"     "2v9MABVNjyxXhedLY9kQVsH3LdXHYcJxG9gowhazivbA"
    , e "barFlat"             "G5YXQ9KMXSDN414BdqSxyEtF8TNdYr294jSwpFZHiYV"
    , e "barElemIndexOff"     "4iGNfZiEhBDU5tuCnK9mLbh3B4pe1kwLEcM3GnWL14wZ"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "CCK8Q3MBjJtmdinWmrcDsrwKsXnpQMZf3RiHi8KKCfLQ"
    , e "w32"                 "7wSdWvgXnoi4fwLqbfsdacVUs2yWRPxu5UECjbLPHHnJ"
    , e "add32"               "26B2743MLXb7ZMGxZjczUM9sH2vETq6Zb8DfPUU8D4SB"
    , e "mul32"               "4cFK6HPZ112WbMFSigMD2Tucr92h3njbjV1KfVSzGfZw"
    , e "div32"               "BDSFTfF9SZ6EHTrzMFLYVtv6HAxwpdo6Yz4Hj7VTnAcz"
    , e "and32"               "53xPmuZqshEA8byJoVbMcCfavb4GvSMzELARPXzCaKUv"
    , e "or32"                "Dp5VR4rRgXRqQfAVYwpw1vnSWoyLkzztXsBJhjqWeUn5"
    , e "xor32"               "4G7XBfaC7JjE2xFsZfG8oUCAEA84NacVPP5LAyDygZ1i"
    , e "lsh32"               "4kMGoUTyTqrCojBy38qBTV1kphNCdFHv3uqgK9EjNaMK"
    , e "rsh32"               "FLRmaZCA2bFnG3Xh8vnGeLNhGabDsoJWQHs1fA1dWwqf"
    , e "sub32"               "9sTCbDhiqGnah3s45iAoj7YPqzg9GsLnXxXTWw4At3zj"
    , e "ror32"               "G8MgK6Pujnh14cMgsfgj4tHAdXvh8yzPCP8zweEyqksd"
    , e "rol32"               "CAPHd8HQw5wc4jWrEoZmDEGkjG4czQnGr8YV5sBcfNMt"
    , e "blake3"              "DM4sSysvJRngLDYjxh4EhkRmtKvmJVmuta5ar1dortej"
    ]
