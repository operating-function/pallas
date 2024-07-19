-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Main (main) where
import qualified Server.ConsoleExe
import Fan.JetHash (installJetHashes)
import Fan.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Server.ConsoleExe.main
