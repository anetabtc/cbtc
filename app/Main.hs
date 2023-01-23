module Main (main) where

import Data.Default (
  def,
 )
import GuardianValidator qualified
import MintCBTC qualified
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "CBTC Minting Policy" "./mintCBTC.plutus" MintCBTC.policy
  writeTypedScript def "Guardian Validator" "./guardianValidator.plutus" GuardianValidator.validator
