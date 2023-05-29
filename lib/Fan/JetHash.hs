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
    , e "cmp"                 "FuG1VXgz3J7b5YpY3kear88Unc4EAim2CcS5jF5TrbRB"
    , e "eql"                 "EcSYXjJCPD6vjDMb6o5HuCWshyGf97ToKn4UnyWMN6cW"
    , e "neq"                 "Eu7kUghvzuRheziEvMD6XypzZXTB1mo5oorkVHCDJy8h"
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
    , e "cabIsEmpty"          "EMTzp4YJ5xvzJab1885TMjAmagA2dUNN8ZTEAKvaVcGs"
    , e "cabLen"              "C9j5TamD3BrFo7yGbrnabC61ux1U5J64KS8Tmt8HqmjU"
    , e "cabHas"              "B1oxdwZmJooGCPWadidYZABWXuMXo6VsRC4pjve7XGsh"
    , e "cabMin"              "FAmEZdaWUYE3TkSjc8RRJWBHe9u8EzPxi4QmBptnUTiP"
    , e "cabIns"              "3ZNB1H4PtsEurm4Az4E5EdrKyXP82gM3K3rxF77NMYLC"
    , e "cabDel"              "Ei7MUeYeXE6xXzq43cZHdRDtRWWRR4b4V6J1a6G773z9"
    , e "cabWeld"             "F3JEpXfi6fBzuFkoeXNUAywWiukJh1RRLCU51KLvzQh2"
    , e "cabCatRowAsc"        "GWQoGFM7r3MGr2MtdMgawZtuywX8ZK1bkKP46oGxv2yy"
    , e "cabDrop"             "BZmCzcYKCjKnuwiAEq2zEHxrCgzx2T4S4j5er4yen3QN"
    , e "cabTake"             "BtM5hAsU3LYJu6qBa4QbErBBVWUJCaNGmzAj9Ek8eS45"
    , e "cabSplitAt"          "5VN8RTbSGVYgBqqkW1eGeo5bpHtQ41gdBvz2pXthKfYy"
    , e "cabIntersect"        "4bqhUBvhrF7GTiGqAHwRxAxHXEd9ZmEoACcXdv4GaVZA"
    , e "cabSub"              "5KHw7uPQ9jchCamQX5zGoZ7Y8znCQp7EkuAqT6Zk2GMA"
    , e "cabSplitLT"          "9o9vVFX8k94QNo9E3f57rVdzA4pXCbAB8rJ1fgRB3N9c"
    , e "tabSing"             "Eba4yH5d1M8cNBvyprvAdJNiuiLzJhkvzB2CNNLYAwYa"
    , e "isTab"               "G8p3862ZDbgPEqoJoKg6r1dGs8i9h9J8rbKdaY44nASn"
    , e "tabIdx"              "9Ld1AhHLYggGrXbsRSVwUn4YwhHURrRCTTvCgmQrALYr"
    , e "tabLen"              "CZXzhS9u2LCfush6zKuwy9mszEKnTMb3oexGaYmqS28W"
    , e "tabKeys"             "4KKnTkup93bXVUVJdt1vtQYU84Pprf2Qziwts5fnyZLF"
    , e "tabVals"             "E3veRjzuXcmi7hQQu544jNcGLzudpajyL6Me8WdZthYk"
    , e "hasKey"              "BV4iyBuWpfbZJWdaUvPR8hBLAbFrpMCvu6o3yALbHoYm"
    , e "tabLookup"           "7QHkt9y19pr1AGbuZZ4DHxo1tzeccTz233krYM3k6ACN"
    , e "tabMut"              "DcYqckCbCGBU7cCf3PupDHREfQNvBG1bmnh3bduQRt7"
    , e "tabSwitch"           "7PMZKnHZdYeJ6im2v4CJiXt1dPhp2w3cRnMrembB1cPg"
    , e "tabToPairs"          "8Jk1Yfw75wK5912HickzBLGbst32QXfiAfkS9WzAagVL"
    , e "tabFromPairs"        "FEseqFzeq9qUFmGdUc1DWexCT3SrA8sYrbWkcST8wiPs"
    , e "tabToPairList"       "DPZaGwRNfoLpFhCZDeZHmqcupSb9ed9669GGRAh6iYju"
    , e "tabElemIdx"          "DsAdWKrUjvS7Dgj4YEiMSyKX3dYzpQPRQb77Ju7i7AsY"
    , e "tabSplitAt"          "D6YWdoXq2W43J8K9W9Ap1M5SK5ujZ5ZaFurVmeLj1cWc"
    , e "tabSplitLT"          "CJyUusMh8hZZqzELy1LTKDNeFunXQpjYUGDp9rrm5sDV"
    , e "tabAlter"            "6E27jAjoEyMa7uwHJMQ8ZN6AAcFqd787hBz8XbbWcCKP"
    , e "tabMap"              "8mzZCkWgNFdQXKFJx67PyQyhP4HMnva4K7LKY1rduab4"
    , e "tabUnionWith"        "6KGtXsv2t6Grg1xWAwqVCeNrWoTNAg5ixJtK4F7kLypC"
    , e "tabMinKey"           "5u78ezpvk2KR6FsvPikWi29QgG6JkCDpgkQgaZaVCeVA"
    , e "tabFoldlWithKey"     "4mejQdx9MW6jhuVWycuqGtqFUnqsFzLwB8nNmHQUb2GV"
    , e "padWeld"             "7qBvAvKzrvomcPbDYxTVYoUauXad1ctjJ5bh5Rq8uyHz"
    , e "padCat"              "BaSRsRhQdj5DcuNi6ur4hxT3e4EEh7ifCUeej8ijDV6Y"
    , e "padFlat"             "E25gGr9M3osAh7caNCxa36McfoCtX8NzhN5jpMHghcwV"
    , e "isBar"               "DukYomwU5sDtCHqAMFQGWHL3wcMLKitzEBq8XNVVTzGX"
    , e "natBar"              "89CwXxb4uSafp3XCRhvZtQK3SKJfSGEwQJotY97nyecd"
    , e "barNat"              "6gkh815zLdxynXFyQZWJc9JnQJSAZdDW9xxQBbT6W2kh"
    , e "barLen"              "4DRuBZuYxvZzoz1cTQjpaFeRqGDCNtBxWeFJBdYEeH3w"
    , e "barIsEmpty"          "6TjSekpdYZonyzXxwL5hrgoE71BtJdHMHEEh5ZjcAQPu"
    , e "barIdx"              "91jgwzZ75UHnMckc85y3LWnYJ69aLMgGMskqwDYBykve"
    , e "barWeld"             "5W9AhYQXaap5iT4EnmDxbuLotsjADuQgkbN7SUDsn8rj"
    , e "barCat"              "Dh3Xdu6Ays5vipUkntbkSR3ARxfRguQBBQsxXtYBACfH"
    , e "barTake"             "CDunAKTkULhJhNPtS64YhjnsZht6V1YqErRY8DBzWH32"
    , e "barDrop"             "G3f5j7w3uWU5ZxsQy7oRA96z3W4Wms4AWkefKXKHUQHS"
    , e "barElemIndexEnd"     "AdtMdCjAYEooYwh6dkWA4DRhT81wUmbpEogaHSw3YSoQ"
    , e "barFlat"             "D39w9bZdbbzqwpEQgxn1K1qiTSZf6eHmiSDw6DirwQbh"
    , e "barElemIndexOff"     "CQpgAwiakHpSiXEqKvN51hTAskJtKSgALhTbXr4P8K4g"
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
    , e "blake3"              "F4UHaetkq94JmMfVJq4Sc4AJoDtqybXUJW9c4XQwtPuR"
    ]
