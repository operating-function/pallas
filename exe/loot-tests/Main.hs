-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Main (main) where
import           Fan.JetHash  (installJetHashes)
import           Fan.JetImpl  (installJetImpls)
import qualified Loot.TestExe
import           Prelude      ((>>))
main = installJetHashes >> installJetImpls >> Loot.TestExe.main
