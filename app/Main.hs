module Main (main) where

import Data.Default (
  def,
 )
import GuardianValidator qualified
import MintCBTC qualified
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V2 (PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (PubKeyHash)
import Ply.Plutarch (
  writeTypedScript,
 )

samplePubKeyHash1 :: PubKeyHash
samplePubKeyHash1 = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"

pubKeyHashList :: forall {s :: S}. Term s (PBuiltinList PPubKeyHash)
pubKeyHashList = pcons # (pconstant samplePubKeyHash1) # pnil

main :: IO ()
main = do
  writeTypedScript def "CBTC Minting Policy" "./mintCBTC.plutus" MintCBTC.policy
  writeTypedScript (Config DoTracing) "Guardian Validator" "./guardianValidator.plutus" (GuardianValidator.validator)
  writeTypedScript (Config DoTracing) "Guardian Validator with Applied Params" "./guardianValidatorParams.plutus" (GuardianValidator.validator # pubKeyHashList)
