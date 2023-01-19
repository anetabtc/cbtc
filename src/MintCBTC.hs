module MintCBTC (policy) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptHash,
  PTokenName,
 )
import Plutarch.Api.V2.Contexts (PScriptPurpose (PMinting))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Prelude

import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Extra.AssetClass (passetClass)
import "liqwid-plutarch-extra" Plutarch.Extra.Value (passetClassValueOf)
import Utils (phasOneCurrecySymbolOneTokenName, phasScriptHash)

-- | cBTC Minting Policy:
policy :: ClosedTerm ((PAsData PTokenName) :--> (PAsData PScriptHash) :--> PMintingPolicy)
policy = plam $ \cBTCTokenName guardianScriptHash _redeemer ctx' -> unTermCont $ do
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
  PMinting ownCurrencySymbol' <- pmatchC ctx.purpose
  ownCurrencySymbol <- pletC $ pfield @"_0" # ownCurrencySymbol'
  txInfo <- pletFieldsC @["inputs", "outputs", "mint"] ctx.txInfo
  assetClassCBTC <- pletC $ passetClass # ownCurrencySymbol # (pfromData cBTCTokenName)
  amnt <- pletC $ passetClassValueOf # assetClassCBTC # txInfo.mint
  ptraceC $ pshow amnt
  ptraceC $ pshow guardianScriptHash
  ptraceC $ pshow $ (pany # (phasScriptHash # pfromData guardianScriptHash) # txInfo.inputs)
  let validate =
        (phasOneCurrecySymbolOneTokenName #$ pnormalize # txInfo.mint)
          #&& (pany # (phasScriptHash # pfromData guardianScriptHash) # txInfo.inputs)
          #&& (0 #< amnt)
          #|| (amnt #< 0)
  pure $
    popaque $
      pif
        validate
        (popaque $ pconstant ())
        perror
