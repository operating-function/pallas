module Main (main) where
import           Fan.JetHash  (installJetHashes)
import           Fan.JetImpl  (installJetImpls)
import qualified Loot.TestExe
import           Prelude      ((>>))
main = installJetHashes >> installJetImpls >> Loot.TestExe.main
