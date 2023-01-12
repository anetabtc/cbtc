module MintCBTC (policy) where

import Plutarch.Api.V2 (
  PMintingPolicy,
 )
import Plutarch.Api.V2.Contexts (PScriptPurpose (PMinting))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Prelude

import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Extra.Maybe (pfromJust)
import "liqwid-plutarch-extra" Plutarch.Extra.Value (psymbolValueOf')
import Utils (phasOneCurrecySymbolOneTokenName)

{- | cBTC Minting Policy:
    * Validates;
    * PValue from txInfo.mint is singleton;
    * minted token are positives;
-}
policy :: ClosedTerm PMintingPolicy
policy = plam $ \_redeemer ctx' -> unTermCont $ do
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'
  PMinting ownCurrencySymbol' <- pmatchC ctx.purpose
  ownCurrencySymbol <- pletC $ pfield @"_0" # ownCurrencySymbol'
  txInfo <- pletFieldsC @["inputs", "outputs", "mint"] ctx.txInfo
  PPair mintedAmnt _burntAmnt <- pmatchC $ pfromJust #$ psymbolValueOf' # ownCurrencySymbol # txInfo.mint
  ptraceC $ pshow mintedAmnt
  ptraceC $ pshow _burntAmnt
  ptraceC $ pshow $ phasOneCurrecySymbolOneTokenName # (pnormalize # txInfo.mint)
  let validate =
        ( phasOneCurrecySymbolOneTokenName
            # (pnormalize # txInfo.mint)
            #&& 0
            #< mintedAmnt
            #|| _burntAmnt
            #< 0
        )
  pure $
    popaque $
      pif
        validate
        (popaque $ pconstant ())
        perror
