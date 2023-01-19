module Spec.MintCBTCSpec (
  sampleTest,
  goodCtx1,
  goodCtx2,
  badCtx1,
  badCtx2,
  badCtx3,
  sampleScriptHash,
  sampleTestEval,
) where

import MintCBTC qualified
import Plutarch (Term)
import "plutarch-context-builder" Plutarch.Context (
  Builder,
  MintingBuilder,
  buildMinting,
  checkPhase1,
  input,
  mint,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withMinting,
  withRedeemer,
  withRefIndex,
  withRefTxId,
  withValue,
 )
import Plutarch.Lift (pconstant)
import Plutarch.Prelude (POpaque, pconstantData, (#))
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
import PlutusTx qualified
import Test.Tasty (
  TestTree,
 )

sampleScriptHash :: ScriptHash
sampleScriptHash = "395e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

samplePubKeyHash1 :: PubKeyHash
samplePubKeyHash1 = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

samplePubKeyHash2 :: PubKeyHash
samplePubKeyHash2 = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb"

cBTCTokenName :: TokenName
cBTCTokenName = "cBTC"

falseTokenName :: TokenName
falseTokenName = "False"

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

commonPurpose :: MintingBuilder
commonPurpose = withMinting $ CurrencySymbol "currency-symbol-one"

goodCtx1 :: ScriptContext
goodCtx1 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
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
                  <> singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
              )
          ]

goodCtx2 :: ScriptContext
goodCtx2 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 10
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
                  <> singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 10
              )
          ]

-- | One CurrencySymbol and Two TokenName
badCtx1 :: ScriptContext
badCtx1 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ oneCurrencySymboldualTokenName
      , inputScript
      , inputPubKey
      , outputPubKey
      , signedWith samplePubKeyHash1
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]
  where
    oneCurrencySymboldualTokenName =
      singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 2
        <> singleton (CurrencySymbol "currency-symbol-one") falseTokenName 2

    outputPubKey :: Builder a => a
    outputPubKey =
      output $
        mconcat
          [ pubKey samplePubKeyHash2
          , withValue
              ( singleton adaSymbol adaToken 2
                  <> oneCurrencySymboldualTokenName
              )
          ]

-- | Two CurrencySymbol and Two TokenName
badCtx2 :: ScriptContext
badCtx2 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ dualCurrencySymboldualTokenName
      , inputScript
      , inputPubKey
      , outputPubKey
      , signedWith samplePubKeyHash1
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]
  where
    dualCurrencySymboldualTokenName =
      singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 2
        <> singleton (CurrencySymbol "currency-symbol-two") falseTokenName 2

    outputPubKey :: Builder a => a
    outputPubKey =
      output $
        mconcat
          [ pubKey samplePubKeyHash2
          , withValue
              ( singleton adaSymbol adaToken 2
                  <> dualCurrencySymboldualTokenName
              )
          ]

-- | Wrong ScriptHash
badCtx3 :: ScriptContext
badCtx3 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
      , wrongScriptInput
      , inputPubKey
      , outputPubKey
      , signedWith samplePubKeyHash1
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]
  where
    wrongScriptInput =
      input $
        mconcat
          [ script "8005f297da8c474354b9fecb481220ea7c76aea178ff7906bfdfb74f"
          , withValue (singleton adaSymbol adaToken 1)
          , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
          , withRefIndex 1
          , withRedeemer (PlutusTx.toData ())
          ]
    outputPubKey :: Builder a => a
    outputPubKey =
      output $
        mconcat
          [ pubKey samplePubKeyHash2
          , withValue
              ( singleton adaSymbol adaToken 2
                  <> singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
              )
          ]

-- | Missing Script Input
badCtx4 :: ScriptContext
badCtx4 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
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
              ( singleton adaSymbol adaToken 1
                  <> singleton (CurrencySymbol "currency-symbol-one") cBTCTokenName 1
              )
          ]

-- | Using wrong TokenName
badCtx5 :: ScriptContext
badCtx5 =
  buildMinting checkPhase1 $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") falseTokenName 1
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
                  <> singleton (CurrencySymbol "currency-symbol-one") falseTokenName 1
              )
          ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test MintCBTC" MintCBTC.policy $ do
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 1 Integer"
    Success
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData () -- Unit
    , PlutusTx.toData goodCtx1 -- ScriptContext
    ]
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 10 Integer"
    Success
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData () -- Unit
    , PlutusTx.toData goodCtx2 -- ScriptContext
    ]
  testEvalCase
    "Fail - 1 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx1
    ]
  testEvalCase
    "Fail - 2 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx2
    ]
  testEvalCase
    "Fail - Wrong ScriptHash"
    Failure
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx3
    ]
  testEvalCase
    "Fail - Missing Script Input"
    Failure
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx4
    ]
  testEvalCase
    "Fail - Using wrong TokenName"
    Failure
    [ PlutusTx.toData cBTCTokenName
    , PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx5
    ]

sampleTestEval :: Term s POpaque
sampleTestEval =
  MintCBTC.policy
    # (pconstantData cBTCTokenName)
    # (pconstantData sampleScriptHash)
    # (pconstant $ PlutusTx.toData ())
    # (pconstant badCtx5)
