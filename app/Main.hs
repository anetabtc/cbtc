module Main (main) where

import GuardianValidator qualified
import MintCBTC qualified
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V2 (PPubKeyHash, scriptHash)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (PubKeyHash, ScriptHash, TokenName)
import Ply.Plutarch (
  writeTypedScript,
 )
import Utils (compileD, writePlutusScript)

samplePubKeyHash1 :: PubKeyHash
samplePubKeyHash1 = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"

pubKeyHashList :: forall {s :: S}. Term s (PBuiltinList PPubKeyHash)
pubKeyHashList = pcons # (pconstant samplePubKeyHash1) # pnil

main :: IO ()
main = do
  writeTypedScript
    (Config DoTracing)
    "Guardian Validator"
    "./compiled/guardianValidator.plutus"
    (GuardianValidator.validator)

  writePlutusScript
    "Guardian Validator with Applied Params"
    "./compiled/guardianValidatorParams.plutus"
    (GuardianValidator.validator # pubKeyHashList)

  writeTypedScript
    (Config DoTracing)
    "CBTC Minting Policy"
    "./compiled/mintCBTC.plutus"
    MintCBTC.policy

  writePlutusScript
    "CBTC Minting Policy with Applied Params"
    "./compiled/mintCBTCParam.plutus"
    (MintCBTC.policy # (pconstant cBTCTokenName) # (pconstant guardianHash))
  where
    guardianHash :: ScriptHash
    guardianHash = scriptHash $ compileD $ (GuardianValidator.validator # pubKeyHashList)

    cBTCTokenName :: TokenName
    cBTCTokenName = "cBTC"
