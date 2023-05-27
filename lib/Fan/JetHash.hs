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
    , e "ifNot"               "66DQ83azQBPKg3E5zy8TFyr7X9jcbNW3acZUtgJnhfdC"
    , e "not"                 "AhoArogkDqhsucCZH4seXXmLXr1a6b4z16gWm3jRiPLX"
    , e "bit"                 "GYgw6LH6KVRiYLFhcWrVpfAWK4PBuWWbiNYCvfx1FLFm"
    , e "and"                 "GiRoy1SVANxdk4cDQn6HkQYVVWZea5kovi8knYysadAX"
    , e "or"                  "63SSHPno9FG8uTkcSq5WGuu5uZgD7X9AoydRz5NhMzjv"
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
    , e "met"                 "GNijNSbqXEqQ9kvm9TTV1NinQncx5yDk5imGCCicDwwU"
    , e "isZero"              "3TsXB7wm4ykMStjVZ6Tk6rVusvUZBrs14rqhwVyLtpaL"
    , e "dis"                 "7q7SCh1ypkgA5F36sNzgtce5Lm8onEGJ83egtnsPhY4F"
    , e "con"                 "7ts3j8F93enfHtFQcq5KoCeLqAy6Eh9ym7Vfbd6rYDeu"
    , e "mix"                 "994vNPvoBxsujorfmhFtwxUASxE2eJPDDjgya1CjZm6K"
    , e "len"                 "5yhLK4x5Fy5NZVqZwmgawDQVYUrYPH2DpUNRaEaaoMNi"
    , e "put"                 "G5nvMctQWMvYfczmoRczc6DTkCTT6bjU8XmpMw1KPm52"
    , e "get"                 "CC4gCjuhJc9bXhiLHSdWkUe84c95bowZac1FkUQqQtDw"
    , e "idx"                 "AbxuJzL7R9fA5xDrvRPoaDQziFceN8hiLTaDWov6bcuc"
    , e "mut"                 "6qnajRQCZDJeYttfLZU8ZB8tRbRoqy7zWA9FU5gx1c3a"
    , e "cmp"                 "DcxajSs5A5fSoGi2WruG2mmXj7itHgNfzb9wD9GFhvcE"
    , e "eql"                 "2yS1bsYtv1JMQjACU4yjDLKAx5QTHHTSfbwoTsiCCUAe"
    , e "neq"                 "AmrtH4QRerbunstDTYnGgKGBidWxaNRKYwLU71Vs256Q"
    , e "weld"                "BgA1iXSmxcdcfNJTwBDLK2mB2uYtAW2rQjBDU7t2S9tE"
    , e "map"                 "E16GGFt7wtPf5EDqTw53pxbis9xRHCmiNWRDJU4JSW78"
    , e "rev"                 "5fZFF5jscYdgFKPN7S5NDmKjG2668RN9JTgFhNcpxjtj"
    , e "rowCons"             "CEVNcwZSmLwHyCQohmE9mLAaR8iS9TSPL6M5eLwjJDmr"
    , e "rowSnoc"             "6FxtisNgL3bBuWnjD5QZdZXXjXhnJ6NVJR7qGfK9aGBp"
    , e "sum"                 "AmR2cZRFydRAnnZr8B2XS2gsnzK2wZWpcvb13EGbntzC"
    , e "cat"                 "58h59WHsZTipbsaHStS5RAqjkfLnQcu3RFpmvWJQqdki"
    , e "zip"                 "9dnYNug3foj8suVEvYm7ZsSna8NmDDFdfJgZgj7eQWE"
    , e "take"                "Asq9VLJaRHqDknsbML8JRyCfVv7rWa8HUvP4sDwwmUGv"
    , e "drop"                "Hp68SRtLWEwZiLQJazLnaDwqymmNr98QT6XNy3xVYTqu"
    , e "unfoldr"             "CuiPfmycgu92f1nFgqXYZLMD5QAeYJFpwJruFcckF11d"
    , e "switch"              "3jB28Bxn47KDKB7qMV3N5rnwL9wEd6b13h6kUZYAmHhj"
    , e "listToRow"           "H1G16rpgTZTEJ1ucTfv93Cqsby7acX81dTXVPMPQTsKD"
    , e "listToRowReversed"   "BQBZCKHhCQExLdSE6qsNwXmQdxRqix39RgPU6UnuSSDL"
    , e "isDigit"             "Auob9kWUEZr5JfLU9CgqzSU16E66X9GuBHRNmuRQJRdW"
    , e "implode"             "2iwo6oEvZdJkg44uFZMezMkwHo9sdPLD7t8fqkZ9o9Hp"
    , e "cabSing"             "FpTzS3jHy4yV3pRfQTMLbGQEVHNafciAvMSjBHWPTiYa"
    , e "cabIsEmpty"          "AAJX38smBYvWGRWjwZZP74W7nhKqwT973bxphT8N1Eg8"
    , e "cabLen"              "C9j5TamD3BrFo7yGbrnabC61ux1U5J64KS8Tmt8HqmjU"
    , e "cabHas"              "EGREYgpF9DSGFSLas5ubvTqoM6YQDCyP5gzu2ZDze4N"
    , e "cabMin"              "FAmEZdaWUYE3TkSjc8RRJWBHe9u8EzPxi4QmBptnUTiP"
    , e "cabIns"              "D7zDL9imgvubvULpkecheYtyquhST3Je4B6NCGKubdc"
    , e "cabDel"              "BksF1gCj8oYj6BxzqgJ8MrGGaxLzPeg5shd354EW8CQP"
    , e "cabUnion"            "49QuhkjSxfNK1WcP2EYmQvqdAmsEusQX2fnHAp1PzuTa"
    , e "cabAscUnions"        "xFcd7cUVaMc5kbJVNedM9hMVbSAXUe2gTxtUDGYY7X1"
    , e "cabDrop"             "9oS31HLs8p91Xpivvrxwu1DHZKFPxGJoUVTZ5bwzWpPV"
    , e "cabTake"             "9DCYVyQPU5DpwdFNRy6LheU3n9AD8t86hUfbYF9VfDKQ"
    , e "cabSplitAt"          "B1R1Tyqy6X6WuVGbTWehoZvDMmGZ9dnYsEuSboqzFK71"
    , e "cabIntersect"        "DdCgdvoNvy711TQsDZ8QU1aFobbBRBaGm5zoNdJ3zmhF"
    , e "cabDiff"             "8x4YaBWYwuhR1tpxHRhd35SNv4hpUu3XMMcagghCQtYn"
    , e "cabSplitLT"          "GzWXEsUFDdyjrPz8whEx5NCFKya5S14HLVzPy875xNxz"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "27SLQuvpmuv9DhQ41QSt3AfuWaRxRiEEGoFGqpzVkBjo"
    , e "tabIdx"              "72niRv61BEvqibioucKVXQ2t8oc1xfvJDjm17B9x4kwC"
    , e "tabLen"              "CZXzhS9u2LCfush6zKuwy9mszEKnTMb3oexGaYmqS28W"
    , e "tabKeys"             "4KKnTkup93bXVUVJdt1vtQYU84Pprf2Qziwts5fnyZLF"
    , e "tabVals"             "44VXJ7hFUBvcYYWknfG6ZJVF7aFQjUJ8HUS9SMVU6bm3"
    , e "hasKey"              "Cnt6AWYrsLMwUmL2f3v8ZE5VKKULRnwDmkcmKbmdrR2y"
    , e "tabLookup"           "9f9B6fpTjyrtAdLVCpx62Kc4oBbU7sbi2BKc1AQc4K6Y"
    , e "tabMut"              "DyVgopAbZ6Pgv763MXYYr4F7kAqpL1REz6abyV39a6LP"
    , e "tabSwitch"           "82cuAPkaqRztvSEfMpcNJAPCA36B38DcEkvom7WEctVq"
    , e "tabToPairs"          "8Jk1Yfw75wK5912HickzBLGbst32QXfiAfkS9WzAagVL"
    , e "tabFromPairs"        "DyuevsvMFWsSsUWgP2Kg7wF64s4BTT7XFr3PF7rmqEuE"
    , e "tabToPairList"       "7TLxJyQH4VNxf1K9sonEL4mW4p5Fiu5nzpMMq89TvWg4"
    , e "tabElem"             "2S4DR1CLyix3kWH38QVjHjQiSgrEK4tboYZnN6dLQHm1"
    , e "tabSplitAt"          "6YPws41iwft5gYHX7cw3o1W6Tng1MADcXuMURdYo3jiY"
    , e "tabSplitLT"          "8pcYGPKY1fNDu4LKmw8qcP5cgq7FcPBxLxwAb2UUPSEe"
    , e "tabAlter"            "Fi5Ar3oSFEoRtocjC5G9VRPP7cpnXoN4ySm2qByi4B1r"
    , e "tabMap"              "F9TspVA4j6jQNAF9dWzcdmrKrC9TZReo7nhmJzT3dPSe"
    , e "tabUnionWith"        "FeAcUJqZBmMrCYBf5xtXbSRsDwcjMQotBzc1pVssTZTH"
    , e "tabMinKey"           "5u78ezpvk2KR6FsvPikWi29QgG6JkCDpgkQgaZaVCeVA"
    , e "tabFoldlWithKey"     "4mejQdx9MW6jhuVWycuqGtqFUnqsFzLwB8nNmHQUb2GV"
    , e "padWeld"             "7qBvAvKzrvomcPbDYxTVYoUauXad1ctjJ5bh5Rq8uyHz"
    , e "padCat"              "BaSRsRhQdj5DcuNi6ur4hxT3e4EEh7ifCUeej8ijDV6Y"
    , e "padFlat"             "E25gGr9M3osAh7caNCxa36McfoCtX8NzhN5jpMHghcwV"
    , e "isBar"               "Chszk9dCypn1nfv6pGSddMGmJe9aPhVJrdTwy312pXVj"
    , e "natBar"              "89CwXxb4uSafp3XCRhvZtQK3SKJfSGEwQJotY97nyecd"
    , e "barNat"              "6gkh815zLdxynXFyQZWJc9JnQJSAZdDW9xxQBbT6W2kh"
    , e "barLen"              "4DRuBZuYxvZzoz1cTQjpaFeRqGDCNtBxWeFJBdYEeH3w"
    , e "barIsEmpty"          "BUw2wHu5x9sWXqpCX8XEifRsZXgPnpyrN7S7dX3e2FQr"
    , e "barIdx"              "91jgwzZ75UHnMckc85y3LWnYJ69aLMgGMskqwDYBykve"
    , e "barWeld"             "5W9AhYQXaap5iT4EnmDxbuLotsjADuQgkbN7SUDsn8rj"
    , e "barCat"              "Dh3Xdu6Ays5vipUkntbkSR3ARxfRguQBBQsxXtYBACfH"
    , e "barTake"             "CDunAKTkULhJhNPtS64YhjnsZht6V1YqErRY8DBzWH32"
    , e "barDrop"             "G3f5j7w3uWU5ZxsQy7oRA96z3W4Wms4AWkefKXKHUQHS"
    , e "barElemIndexEnd"     "5nDkEiCyfrL9truS4xBCDgUV5pMkwpaghyxK4omCdaLf"
    , e "barFlat"             "9XYwXkzt19hYCEKeDkbQWtY1qtnSM4ERXzRDVz2jad7M"
    , e "barElemIndexOff"     "4TSCj7mvbEq9YYX7WkdvJoUjeUfEimEtTc2XV2zFFjJG"
    , e "par"                 "3y5eZF1uGwffhJmYcYyHFdHhZgJTygwTbVm38W1nrEVu"
    , e "pseq"                "CCK8Q3MBjJtmdinWmrcDsrwKsXnpQMZf3RiHi8KKCfLQ"
    , e "w32"                 "7wSdWvgXnoi4fwLqbfsdacVUs2yWRPxu5UECjbLPHHnJ"
    , e "add32"               "26B2743MLXb7ZMGxZjczUM9sH2vETq6Zb8DfPUU8D4SB"
    , e "mul32"               "4cFK6HPZ112WbMFSigMD2Tucr92h3njbjV1KfVSzGfZw"
    , e "div32"               "BDSFTfF9SZ6EHTrzMFLYVtv6HAxwpdo6Yz4Hj7VTnAcz"
    , e "and32"               "8SXhLKPkYzfcQeib2vZZt233Cabf8vwEh6iDUrNsHA7v"
    , e "or32"                "G9k6d6RE7t1dodJ8HQSEH7vt2NWNiogX6qcJYKAXREZM"
    , e "xor32"               "9bNsbDhn4YQzmoC3SYLoKbuNe2H7A7zaHuHxbUCUTxYa"
    , e "lsh32"               "71C1QbsrCTtZkd7mLjeJSkuYyS9MVZddi9xTJqkb18RS"
    , e "rsh32"               "CYwt88X3VBVWBbTjwC9D1UbXs6W6J7fWwvTDyGPPQWBW"
    , e "sub32"               "9sTCbDhiqGnah3s45iAoj7YPqzg9GsLnXxXTWw4At3zj"
    , e "ror32"               "6PpBXitcMLBz7opDT5UvJaQH5TrHWaAgvK1hJSvL37uw"
    , e "rol32"               "6VBwDDucdVbTeay9hzJxPnLf4Yz9xXXCw3H6yhoMGRcu"
    , e "blake3"              "8ig6u7EGrtuytcqiqRZ75nWK4P17mJ8cUyHssHTcyMR4"
    ]
