{-# LANGUAGE TemplateHaskell #-}
module GuardianValidator (validator) where

import Plutarch.Api.V2 (PValidator, PAddress, PPubKeyHash)
import Plutarch.Prelude
import PlutusTx qualified
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )

import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (Address)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (ptraceC, pletFieldsC, pletC)

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

validator :: ClosedTerm ((PAsData (PBuiltinList (PAsData PPubKeyHash))) :--> PValidator)
validator = plam $ \pkhList _datum' _redeemer' ctx' ->
  unTermCont $ do
    ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
    txInfo <- pletFieldsC @["inputs", "outputs", "signatories"] ctx.txInfo
    cosigners <- pletC $ pmap # (ptxSignedBy # txInfo.signatories) # (pfromData pkhList)
    let validate = pall # plam (#== pcon PTrue) # cosigners
    ptraceC $ pshow validate
    pure $
      popaque $
        pif
          validate
          (popaque $ pconstant ())
          perror
