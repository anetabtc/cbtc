module MintCBTC (policy) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptHash,
  PTxInInfo,
 )
import Plutarch.Api.V2.Contexts (PScriptPurpose (PMinting))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.Prelude

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Extra.Maybe (pfromJust)
import "liqwid-plutarch-extra" Plutarch.Extra.Value (psymbolValueOf')
import Utils (phasOneCurrecySymbolOneTokenName)

-- import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pscriptHashFromAddress)

{- | cBTC Minting Policy:
    * Validates;
    * PValue from txInfo.mint is singleton;
    * minted token are positives;
-}
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

phasScriptHash :: Term s (PScriptHash :--> PTxInInfo :--> PBool)
phasScriptHash = plam $ \scriptHash info -> unTermCont $ do
  credential <- pletC $ pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # info
  pure $
    pmatch credential $ \case
      PScriptCredential sh' -> unTermCont $ do
        sh <- pletC $ pfield @"_0" # sh'
        ptraceC $ pshow sh
        pure (sh #== scriptHash)
      PPubKeyCredential _ ->
        pcon PFalse
