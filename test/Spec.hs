module Main (main) where

import Test.Tasty (
  defaultMain,
  testGroup,
 )

import Spec.MintCBTCSpec qualified as MintCBTCSpec

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Test Minting Policy"
      [MintCBTCSpec.sampleTest]
