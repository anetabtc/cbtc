{-# LANGUAGE TemplateHaskell #-}

module GuardianValidator (validator) where

import Plutarch.Api.V2 (PAddress, PPubKeyHash, PValidator)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Prelude
import PlutusTx qualified

import Plutarch.Extra.ScriptContext (ptxSignedBy)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, ptraceC)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (Address)

data GuardianDatum = GuardianDatum
  { deposit :: Integer
  , address :: Address
  }
  deriving stock (Show, Generic, Eq, Ord)

PlutusTx.makeIsDataIndexed ''GuardianDatum [('GuardianDatum, 0)]

newtype PGuardianDatum (s :: S)
  = PGuardianDatum
      ( Term
          s
          ( PDataRecord
              '[ "deposit" ':= PInteger
               , "address" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow, PTryFrom PData)

instance DerivePlutusType PGuardianDatum where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PGuardianDatum where type PLifted PGuardianDatum = GuardianDatum
deriving via (DerivePConstantViaData GuardianDatum PGuardianDatum) instance (PConstantDecl GuardianDatum)
instance PTryFrom PData (PAsData PGuardianDatum)

validator :: ClosedTerm ((PBuiltinList PPubKeyHash) :--> PValidator)
validator = plam $ \pkhList _datum' _redeemer' ctx' ->
  unTermCont $ do
    ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
    txInfo <- pletFieldsC @["inputs", "outputs", "signatories"] ctx.txInfo
    cosigners <- pletC $ pmap # plam ((ptxSignedBy # txInfo.signatories #) . pdata) # pkhList
    let validate = pall # plam (#== pcon PTrue) # cosigners
    ptraceC $ pshow validate
    pure $
      popaque $
        pif
          validate
          (popaque $ pconstant ())
          perror
