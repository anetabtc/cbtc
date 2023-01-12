module Main (main, mainEval) where

import Test.Tasty (
  defaultMain,
  testGroup,
 )

import Spec.MintCBTCSpec qualified as MintCBTCSpec
import Utils (evalT)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test"
      [MintCBTCSpec.sampleTest]

mainEval :: IO ()
mainEval = do
  case evalT MintCBTCSpec.sampleTestEval of
    Left _ -> putStrLn "Error"
    Right r -> putStrLn (show r)
