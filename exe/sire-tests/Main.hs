module Main (main) where
import qualified Sire.TestExe
import Fan.JetHash (installJetHashes)
import Fan.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Sire.TestExe.main
