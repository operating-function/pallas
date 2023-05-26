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
ifNotHash = "GuMQnrCGNsTTj75eQ9UNpdEcj4GCTF8UqR1Ja69iufbc"

switchHash :: Hash256
switchHash = "At3b6gCssS7MRqh2imQVsjbn5mBQsMe9hnqvS6MiZ9PX"

tabSwitchHash :: Hash256
tabSwitchHash = "GduoKPWLNEqZyGRDKTFXiNCRGZ2x5RDEMUtU8EFxhEs3"

seqHash :: Hash256
seqHash = "sb965zsyHBkHaowAjiXcTMt89e3kvnq617Hsf8DRi6S"

trkHash :: Hash256
trkHash = "3EhMetnuFhx9zeMRJS2g71PybJ38btaan8pfwfQicdNJ"

idxHash :: Hash256
idxHash = "AbxuJzL7R9fA5xDrvRPoaDQziFceN8hiLTaDWov6bcuc"

getHash :: Hash256
getHash = "CC4gCjuhJc9bXhiLHSdWkUe84c95bowZac1FkUQqQtDw"

addHash :: Hash256
addHash = "K8EH4S4R28TjcWYdzeuFgb5mUSycAnXesRJtFskrebR"

subHash :: Hash256
subHash = "G7tooJfYL2Jynj6gf2YmoE3ChKfPcn3Ly2LA8x7bUtB1"

mulHash :: Hash256
mulHash = "BWNGj7CzE1got1H5pnp4nSYG8jbW8xBBGvrcDq7mHW5n"

eqlHash :: Hash256
eqlHash = "4AaF13zqQmE32NwSRFyg5w6mUt8cF6vQ3tTBvXfzGLGB"

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
