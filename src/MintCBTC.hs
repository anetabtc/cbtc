module MintCBTC (policy) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptHash,
 )
import Plutarch.Api.V2.Contexts (PScriptPurpose (PMinting))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Prelude

import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Extra.Maybe (pfromJust)
import "liqwid-plutarch-extra" Plutarch.Extra.Value (psymbolValueOf')
import Utils (phasOneCurrecySymbolOneTokenName, phasScriptHash)

-- | cBTC Minting Policy:
policy :: ClosedTerm (PAsData PScriptHash :--> PMintingPolicy)
policy = plam $ \scHash _redeemer ctx' -> unTermCont $ do
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
  PMinting ownCurrencySymbol' <- pmatchC ctx.purpose
  ownCurrencySymbol <- pletC $ pfield @"_0" # ownCurrencySymbol'
  txInfo <- pletFieldsC @["inputs", "outputs", "mint"] ctx.txInfo
  PPair mintedAmnt _burntAmnt <- pmatchC $ pfromJust #$ psymbolValueOf' # ownCurrencySymbol # txInfo.mint
  ptraceC $ pshow scHash
  ptraceC $ pshow $ (pany # (phasScriptHash # pfromData scHash) # txInfo.inputs)
  let validate =
        (phasOneCurrecySymbolOneTokenName #$ pnormalize # txInfo.mint)
          #&& (pany # (phasScriptHash # pfromData scHash) # txInfo.inputs)
          #&& (0 #< mintedAmnt)
          #|| (_burntAmnt #< 0)
  pure $
    popaque $
      pif
        validate
        (popaque $ pconstant ())
        perror
