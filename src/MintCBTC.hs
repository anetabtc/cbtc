module MintCBTC (policy) where

import Plutarch.Api.V2 (
  PMintingPolicy,
 )
import Plutarch.Prelude (ClosedTerm, pconstant, plam, popaque)

policy :: ClosedTerm PMintingPolicy
policy = plam $ \_ _ -> popaque $ pconstant ()
