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
import "plutarch-context-builder" Plutarch.Context (Builder, MintingBuilder, buildMinting', input, mint, pubKey, script, withMinting, withRefIndex, withRefTxId, withValue)
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
  TxId,
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

samplePubKeyHash :: PubKeyHash
samplePubKeyHash = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

sampleTxId :: TxId
sampleTxId = "376efc6f1e85da82e082eeeb795fc34733991c1c190707722f23e9bf"

inputScript :: Builder a => a
inputScript =
  input $
    mconcat
      [ script sampleScriptHash
      , withValue (singleton adaSymbol adaToken 1)
      , withRefTxId sampleTxId
      , withRefIndex 1
      ]

inputPubKey :: Builder a => a
inputPubKey =
  input $
    mconcat
      [ pubKey samplePubKeyHash
      , withValue (singleton adaSymbol adaToken 1)
      , withRefTxId sampleTxId
      , withRefIndex 1
      ]

commonCS :: MintingBuilder
commonCS = withMinting $ CurrencySymbol "currency-symbol-one"

goodCtx1 :: ScriptContext
goodCtx1 =
  buildMinting' $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
      , inputScript
      , inputPubKey
      , commonCS
      ]

goodCtx2 :: ScriptContext
goodCtx2 =
  buildMinting' $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 10
      , inputScript
      , inputPubKey
      , commonCS
      ]

badCtx1 :: ScriptContext
badCtx1 =
  buildMinting' $
    mconcat
      [ mint $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 2
            <> singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-two") 2
      , inputScript
      , inputPubKey
      , commonCS
      ]

badCtx2 :: ScriptContext
badCtx2 =
  buildMinting' $
    mconcat
      [ mint $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 2
            <> singleton (CurrencySymbol "currency-symbol-two") (TokenName "token-two") 2
      , inputScript
      , inputPubKey
      , commonCS
      ]

badCtx3 :: ScriptContext
badCtx3 =
  buildMinting' $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
      , input $
          mconcat
            [ script "8005f297da8c474354b9fecb481220ea7c76aea178ff7906bfdfb74f"
            , withValue (singleton adaSymbol adaToken 1)
            , withRefTxId sampleTxId
            , withRefIndex 1
            ]
      , inputPubKey
      , commonCS
      ]

badCtx4 :: ScriptContext
badCtx4 =
  buildMinting' $
    mconcat
      [ mint $ singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
      , inputPubKey
      , commonCS
      ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test MintCBTC" MintCBTC.policy $ do
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 1 Integer"
    Success
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData () -- Unit
    , PlutusTx.toData goodCtx1 -- ScriptContext
    ]
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 10 Integer"
    Success
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData () -- Unit
    , PlutusTx.toData goodCtx2 -- ScriptContext
    ]
  testEvalCase
    "Fail - 1 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx1
    ]
  testEvalCase
    "Fail - 2 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx2
    ]
  testEvalCase
    "Fail - Wrong ScriptHash"
    Failure
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx3
    ]
  testEvalCase
    "Fail - Missing ScriptHash"
    Failure
    [ PlutusTx.toData sampleScriptHash
    , PlutusTx.toData ()
    , PlutusTx.toData badCtx4
    ]

sampleTestEval :: Term s POpaque
sampleTestEval =
  MintCBTC.policy
    # (pconstantData sampleScriptHash)
    # (pconstant $ PlutusTx.toData ())
    # (pconstant goodCtx1)
