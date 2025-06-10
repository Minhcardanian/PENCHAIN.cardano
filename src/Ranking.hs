module Ranking (SLAObjective(..), rankSLAs) where

import           Data.List (sortBy)
import           Data.Function (on)
import           ServiceAggregation

-- | Objectives that can be used when ranking SLAs.
data SLAObjective = RuleAbidingRate | Satisfaction | Cost
    deriving (Show, Eq)

-- | Rank a list of aggregated SLAs according to the given objectives.
--   Higher 'ruleAbidingRate' and 'satisfaction' are considered better while
--   lower 'cost' is preferred.
rankSLAs :: [SLAObjective] -> [AggregatedSLA] -> [AggregatedSLA]
rankSLAs objectives = sortBy (compareBy objectives)
  where
    score RuleAbidingRate = ruleAbidingRate
    score Satisfaction    = satisfaction
    score Cost            = negate . cost

    compareBy [] _ _ = EQ
    compareBy (o:os) a b =
        case compare `on` score o $ a b of
            EQ  -> compareBy os a b
            ord -> ord
