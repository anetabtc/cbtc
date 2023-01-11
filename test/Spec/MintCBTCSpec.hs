module Spec.MintCBTCSpec (sampleTest, mockCtx) where

import MintCBTC qualified
import Plutarch.Test.Precompiled (
  Expectation (..),
  testEvalCase,
  tryFromPTerm,
 )
import PlutusLedgerApi.V1.Interval (
  interval,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  POSIXTime (..),
  ScriptContext (..),
  ScriptPurpose (..),
  TokenName (..),
  TxId (..),
  TxInfo (..),
  TxOutRef (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.AssocMap (
  empty,
 )
import Test.Tasty (
  TestTree,
 )

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    ( TxInfo
        mempty -- txInfoInputs
        mempty -- txInfoReferenceInputs
        mempty -- txInfoOutputs
        mempty -- txInfoFee
        (singleton (CurrencySymbol "test") (TokenName "test") 1) -- txInfoMint
        mempty -- txInfoDCert
        empty -- txInfoWdrl
        (interval (POSIXTime 1) (POSIXTime 2)) -- txInfoValidRange
        ["f013", "ab45"] -- txInfoSignatories
        empty -- txInfoRedeemers
        empty -- txInfoData
        "" -- txInfoId
    )
    (Minting (CurrencySymbol "test"))

sampleTest :: TestTree
sampleTest = tryFromPTerm "sample validator" MintCBTC.policy $ do
  testEvalCase
    "it should pass"
    Success
    [ PlutusTx.toData (TxOutRef {txOutRefId = TxId "a2c20c77887ace1cd986193e4e75babd", txOutRefIdx = 0}) -- TxOutRef
    , PlutusTx.toData mockCtx -- ScriptContext
    ]
