module Utils (
  evalT,
  phasOneCurrecySymbolOneTokenName,
  phasScriptHash,
) where

import Data.Bifunctor (
  first,
 )
import Data.Text (
  Text,
  pack,
 )
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V2 (AmountGuarantees, KeyGuarantees, PMap (PMap), PScriptHash, PTxInInfo, PValue (PValue))
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.List (pisSingleton)
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Prelude
import Plutarch.Script (
  Script,
 )
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

{- Note:
plength $ pto $ pto $ pto txInfo.mint == 1
only checks length of CurrencySymbol, and not TokenName length
mockCtx3 fails
-}

{- | Returns 'PTrue' if the argument 'PValue' has one 'PCurrencySymbol'
  and one 'PTokenName', if PValue is not /normalized/ ('PValue' ''Sorted' ''NonZero') it will return 'PFalse'
-}
phasOneCurrecySymbolOneTokenName ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PValue keys amounts :--> PBool)
phasOneCurrecySymbolOneTokenName = plam $ \value' ->
  unTermCont $ do
    PValue mapValue <- pmatchC $ value'
    PMap listCS <- pmatchC mapValue
    PMap listTokenAndAmnt <- pmatchC $ pfromData $ psndBuiltin #$ phead # listCS
    pure $
      (pisSingleton # listCS) #&& (pisSingleton # listTokenAndAmnt)

phasScriptHash :: Term s (PScriptHash :--> PTxInInfo :--> PBool)
phasScriptHash = plam $ \scriptHash info -> unTermCont $ do
  credential <- pletC $ pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # info
  pure $
    pmatch credential $ \case
      PScriptCredential sh' -> unTermCont $ do
        sh <- pletC $ pfield @"_0" # sh'
        pure (sh #== scriptHash)
      PPubKeyCredential _ ->
        pcon PFalse
