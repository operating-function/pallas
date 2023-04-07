module Main (main) where
import qualified Loot.ReplExe
import Fan.JetHash (installJetHashes)
import Fan.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Loot.ReplExe.main
