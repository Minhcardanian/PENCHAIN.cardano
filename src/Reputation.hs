module Reputation where

import           Plutus.V3.Ledger.Contexts (PubKeyHash)
import           SLA

-- | Simple reputation model for a provider.
data ProviderReputation = ProviderReputation
    { repProvider :: PubKeyHash
    , repScore    :: Integer
    } deriving Show

-- | Update the reputation using information from the SLA datum.
--   This is a very naive model used only for the MVP demonstration.
updateReputation :: ProviderReputation -> SLADatum -> ProviderReputation
updateReputation rep datum =
    rep { repScore = repScore rep + ruleAbidingRate datum }
