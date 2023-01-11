module Main (main) where

import Data.Default (
  def,
 )
import MintCBTC qualified
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "CBTC Minting Policy" "./mintCBTC.plutus" MintCBTC.policy
