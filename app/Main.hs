module Main (main) where

import MultiSigMintPolicy qualified
import MultiSigValidator qualified
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
 )
import Utils (writePlutusScript)

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"

  writePlutusScript
    "Multisig Validator"
    "./compiled/multisigValidator.plutus"
    MultiSigValidator.validator

  writePlutusScript
    "Multisig Minting Policy"
    "./compiled/multisigMintingPolicy.plutus"
    MultiSigMintPolicy.policy
