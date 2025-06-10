module ServiceAggregation where

-- | Aggregated representation of an SLA used for ranking services.
data AggregatedSLA = AggregatedSLA
    { ruleAbidingRate :: Integer
    , satisfaction    :: Integer
    , cost            :: Integer
    } deriving (Show, Eq)
