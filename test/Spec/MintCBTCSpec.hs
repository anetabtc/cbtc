module Spec.MintCBTCSpec (sampleTest, mockCtx1, sampleTestEval, mockCtx2) where

import MintCBTC qualified
import Plutarch (Term)
import "plutarch-context-builder" Plutarch.Context (MintingBuilder, buildMinting', mint, withMinting)
import Plutarch.Lift (pconstant)
import Plutarch.Prelude (POpaque, (#))
import Plutarch.Test.Precompiled (
  Expectation (..),
  testEvalCase,
  tryFromPTerm,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  ScriptContext (..),
  TokenName (..),
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (
  TestTree,
 )

mockCtx1 :: ScriptContext
mockCtx1 =
  buildMinting' $
    mconcat
      [ mint @MintingBuilder $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 1
      , withMinting $ CurrencySymbol "currency-symbol-one"
      ]

mockCtx2 :: ScriptContext
mockCtx2 =
  buildMinting' $
    mconcat
      [ mint @MintingBuilder $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 10
      , withMinting $ CurrencySymbol "currency-symbol-one"
      ]

mockCtx3 :: ScriptContext
mockCtx3 =
  buildMinting' $
    mconcat
      [ mint @MintingBuilder $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 2
            <> singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-two") 2
      , withMinting $ CurrencySymbol "currency-symbol-one"
      ]

mockCtx4 :: ScriptContext
mockCtx4 =
  buildMinting' $
    mconcat
      [ mint @MintingBuilder $
          singleton (CurrencySymbol "currency-symbol-one") (TokenName "token-one") 2
            <> singleton (CurrencySymbol "currency-symbol-two") (TokenName "token-two") 2
      , withMinting $ CurrencySymbol "currency-symbol-one"
      ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "Test MintCBTC" MintCBTC.policy $ do
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 1 Integer"
    Success
    [ PlutusTx.toData () -- Unit
    , PlutusTx.toData mockCtx1 -- ScriptContext
    ]
  testEvalCase
    "Pass - 1 CurrencySymbol , 1 TokenName, 10 Integer"
    Success
    [ PlutusTx.toData () -- Unit
    , PlutusTx.toData mockCtx2 -- ScriptContext
    ]
  testEvalCase
    "Fail - 1 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData mockCtx3
    ]
  testEvalCase
    "Fail - 2 CurrencySymbol , 2 TokenName, 2 Integer per TokenName"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData mockCtx4
    ]

sampleTestEval :: Term s POpaque
sampleTestEval =
  MintCBTC.policy
    # (pconstant $ PlutusTx.toData ())
    # (pconstant mockCtx2)
