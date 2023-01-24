module Spec.GuardianValidatorSpec (
  sampleTest,
  samplePubKeyHash1,
  goodCtx1,
  pubKeyHashList,
  sampleTestEval,
) where

import "plutarch-context-builder" Plutarch.Context (
  Builder,
  SpendingBuilder,
  buildSpending,
  checkPhase1,
  input,
  mint,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withRedeemer,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefIdx,
  withValue,
 )
import Plutarch.Prelude
import Plutarch.Test.Precompiled (
  Expectation (..),
  testEvalCase,
  tryFromPTerm,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  PubKeyHash,
  ScriptContext (..),
  ScriptHash (..),
  TokenName (..),
  adaSymbol,
  adaToken,
  singleton,
 )

import GuardianValidator qualified
import Plutarch.Api.V2 (PPubKeyHash)
import PlutusTx qualified
import Test.Tasty (TestTree)

sampleScriptHash :: ScriptHash
sampleScriptHash = "395e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

samplePubKeyHash1 :: PubKeyHash
samplePubKeyHash1 = "6b846eaacc07c6d27285af01eb9851e1afcbb7786f06833e06ef11a7"

samplePubKeyHash2 :: PubKeyHash
samplePubKeyHash2 = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb"

inputScript :: Builder a => a
inputScript =
  input $
    mconcat
      [ script sampleScriptHash
      , withValue (singleton adaSymbol adaToken 1)
      , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (PlutusTx.toData ())
      ]

inputPubKey :: Builder a => a
inputPubKey =
  input $
    mconcat
      [ pubKey samplePubKeyHash1
      , withValue (singleton adaSymbol adaToken 1)
      , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
      , withRefIndex 2
      ]

commonPurpose :: SpendingBuilder
commonPurpose = withSpendingOutRefIdx 1

goodCtx1 :: ScriptContext
goodCtx1 =
  buildSpending checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
      , inputScript
      , inputPubKey
      , outputPubKey
      , signedWith samplePubKeyHash1
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]
  where
    outputPubKey :: Builder a => a
    outputPubKey =
      output $
        mconcat
          [ pubKey samplePubKeyHash2
          , withValue
              ( singleton adaSymbol adaToken 2
                  <> singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
              )
          ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test GuardianValidator" (GuardianValidator.validator # pubKeyHashList) $ do
  testEvalCase
    "Pass "
    Success
    [ PlutusTx.toData () -- Datum Unit
    , PlutusTx.toData () -- Redeemer Unit
    , PlutusTx.toData goodCtx1 -- ScriptContext
    ]

pubKeyHashList :: forall {s :: S}. Term s (PBuiltinList PPubKeyHash)
pubKeyHashList = pcons # (pconstant samplePubKeyHash1) # pnil

sampleTestEval :: Term s POpaque
sampleTestEval =
  GuardianValidator.validator
    # pubKeyHashList
    # (pconstant $ PlutusTx.toData ())
    # (pconstant $ PlutusTx.toData ())
    # (pconstant goodCtx1)
