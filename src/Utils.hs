module Utils (
  evalT,
  phasOneCurrecySymbolOneTokenName,
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
import Plutarch.Api.V2 (AmountGuarantees, KeyGuarantees, PMap (PMap), PValue (PValue))
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.List (pisSingleton)
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
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
      ( pisSingleton
          # listCS
          #&& pisSingleton
          # listTokenAndAmnt
      )
