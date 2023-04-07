module Main (main) where
import qualified Sire.ReplExe
import Fan.JetHash (installJetHashes)
import Fan.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Sire.ReplExe.main
