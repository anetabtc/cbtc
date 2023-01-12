module GuardianValidator (validator) where

import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude

validator :: ClosedTerm PValidator
validator = plam $ \_ _ _ ->
  unTermCont $ do
    pure $
      popaque $
        pconstant True
