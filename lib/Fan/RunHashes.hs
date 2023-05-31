-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Fan.RunHashes where

import Fan.Print      (decodeBtc)
import PlunderPrelude (ByteString)

import Jelly.Types

--------------------------------------------------------------------------------

ifHash :: Hash256
ifHash = "9cFk4oxzbH92Bi8jgw7k5LYJNkhTLBxAiQfJ3x3baf6e"

ifNotHash :: Hash256
ifNotHash = "66DQ83azQBPKg3E5zy8TFyr7X9jcbNW3acZUtgJnhfdC"

switchHash :: Hash256
switchHash = "7PUddHfrSKjH19U4nEj8z5nwV35QqSNrt8AiFgkQ18GX"

tabSwitchHash :: Hash256
tabSwitchHash = "66LcDLd15GTe48kuN1CS5BfRrrZaQPPo4VkYP5nEWMCp"

seqHash :: Hash256
seqHash = "4bEt86gt6UWqH5zbjzCwD1ZsTUrQCma8Ur3e8KJAyNAq"

trkHash :: Hash256
trkHash = "AXuX8gnKrPy5MNN4C3G4ZrqyjeAzCm5K5PVhibnFXFwM"

idxHash :: Hash256
idxHash = "GkNafYAwRbzsipgsEWBrt9NUSgbQhAueCYa4J1mtCjUa"

getHash :: Hash256
getHash = "BJ3xwnsARKMVPxZSRckibgUMKKLd3Qo5XKvS8sK6BqU"

addHash :: Hash256
addHash = "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"

subHash :: Hash256
subHash = "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"

mulHash :: Hash256
mulHash = "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"

eqlHash :: Hash256
eqlHash = "FKLUVd4DncStLVn75NiXLqW5JpHaLoTdVWpUnZY4hatg"

eqlNatHash :: Hash256
eqlNatHash = "HinQSPoGNvnL6DG3mkeKKtiyCTnCYaS3oNzRsGSw3Ccx"

lteNatHash :: Hash256
lteNatHash = "AhChaxy3yaRdMSLWzuhVkWb5sRHMtdUL6gV4FNNafE6g"

lthNatHash :: Hash256
lthNatHash = "2BDyomgtS4AkZutt77LiZtmipwPuKEYkPcdz6tr9URkD"

gteNatHash :: Hash256
gteNatHash = "BbKRjpR2Xs1XgGUb8ZSC1u4VceveHtyEkbo5t8tyRsJL"

gthNatHash :: Hash256
gthNatHash = "8itNV8d9SwGrkLb9VYE1oS9L1BsnxM1JtSehYvbcL57C"
