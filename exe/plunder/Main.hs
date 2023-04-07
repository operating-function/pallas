module Main (main) where
import qualified Server.ConsoleExe
import Fan.JetHash (installJetHashes)
import Fan.JetImpl (installJetImpls)
import Prelude ((>>))
main = installJetHashes >> installJetImpls >> Server.ConsoleExe.main
