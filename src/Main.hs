{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           PlutusTx.Prelude
import qualified PlutusTx
import           Plutus.V3.Ledger.Contexts
import           Plutus.V3.Ledger.Api
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints

import           SLA
import           Reputation
import           ServiceAggregation
import           Ranking

data PenChainState = PenChainState
    { registeredSLAs :: [SLAParams]
    , slaStates      :: [(SLAParams, SLADatum)]
    , serviceRankings :: [AggregatedSLA]
    }

initPenChainState :: PenChainState
initPenChainState = PenChainState [] [] []

registerSLA :: SLAParams -> PenChainState -> PenChainState
registerSLA slaParams state =
    state { registeredSLAs = slaParams : registeredSLAs state }

updateSLAState :: SLAParams -> SLADatum -> PenChainState -> PenChainState
updateSLAState slaParams slaDatum state =
    state { slaStates = (slaParams, slaDatum) : filter (\(params, _) -> params /= slaParams) (slaStates state) }

updateServiceRankings :: [SLAObjective] -> PenChainState -> PenChainState
updateServiceRankings objectives state =
    let aggregatedSLAs = map (\(params, datum) -> 
            AggregatedSLA 
                { ruleAbidingRate = ruleAbidingRate datum
                , satisfaction = serviceLevel datum
                , cost = penaltyAmount params
                }) (slaStates state)
        rankedSLAs = rankSLAs objectives aggregatedSLAs
    in state { serviceRankings = rankedSLAs }

exampleProvider :: PubKeyHash
exampleProvider = "providerPubKeyHash"

exampleCustomer :: PubKeyHash
exampleCustomer = "customerPubKeyHash"

exampleSLAParams :: SLAParams
exampleSLAParams = SLAParams
    { provider = exampleProvider
    , customer = exampleCustomer
    , serviceLevelThreshold = 80
    , penaltyAmount = 100
    , slaDeadline = 1000
    , ruleAbidingRateThreshold = 90
    }

exampleSLADatum :: SLADatum
exampleSLADatum = SLADatum
    { serviceLevel = 85
    , isPenaltyPaid = False
    , ruleAbidingRate = 95
    , transactionCount = 10
    }

main :: IO ()
main = do
    let initialState = initPenChainState
        stateWithSLA = registerSLA exampleSLAParams initialState
        stateWithUpdatedSLA = updateSLAState exampleSLAParams exampleSLADatum stateWithSLA
        finalState = updateServiceRankings [RuleAbidingRate, Satisfaction, Cost] stateWithUpdatedSLA
    
    putStrLn "Registered SLAs:"
    print $ registeredSLAs finalState
    
    putStrLn "SLA States:"
    print $ slaStates finalState
    
    putStrLn "Service Rankings:"
    print $ serviceRankings finalState