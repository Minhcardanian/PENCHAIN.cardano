{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module SLA where

import           PlutusTx.Prelude
import qualified PlutusTx
import           Plutus.V3.Ledger.Contexts
import           Plutus.V3.Ledger.Api
import           Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import           Plutus.V3.Ledger.Value
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints

data SLAParams = SLAParams
    { provider              :: PubKeyHash
    , customer              :: PubKeyHash
    , serviceLevelThreshold :: Integer
    , penaltyAmount         :: Integer
    , slaDeadline           :: POSIXTime
    , ruleAbidingRateThreshold :: Integer
    }

PlutusTx.makeLift ''SLAParams

data SLADatum = SLADatum
    { serviceLevel    :: Integer
    , isPenaltyPaid   :: Bool
    , ruleAbidingRate :: Integer
    , transactionCount :: Integer
    }

PlutusTx.unstableMakeIsData ''SLADatum

data SLARedeemer = Measure Integer | ClaimPenalty | UpdateReputation | Close
    deriving Show

PlutusTx.unstableMakeIsData ''SLARedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: SLAParams -> SLADatum -> SLARedeemer -> ScriptContext -> Bool
mkValidator params datum redeemer ctx =
    traceIfFalse "Invalid action" $ case redeemer of
        Measure newServiceLevel -> 
            traceIfFalse "Only provider can measure" signedByProvider &&
            traceIfFalse "Can't measure after deadline" beforeDeadline &&
            traceIfFalse "Invalid service level" (newServiceLevel >= 0 && newServiceLevel <= 100)
        ClaimPenalty -> 
            traceIfFalse "Only customer can claim penalty" signedByCustomer &&
            traceIfFalse "Service level not below threshold" (serviceLevel datum < serviceLevelThreshold params) &&
            traceIfFalse "Penalty already paid" (not $ isPenaltyPaid datum) &&
            traceIfFalse "Incorrect penalty amount" correctPenaltyAmount
        UpdateReputation ->
            traceIfFalse "Only provider can update reputation" signedByProvider &&
            traceIfFalse "Can't update reputation after deadline" beforeDeadline
        Close -> 
            traceIfFalse "Only provider can close" signedByProvider &&
            traceIfFalse "Too early to close" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByProvider :: Bool
    signedByProvider = txSignedBy info $ provider params

    signedByCustomer :: Bool
    signedByCustomer = txSignedBy info $ customer params

    beforeDeadline :: Bool
    beforeDeadline = contains (to $ slaDeadline params) $ txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = contains (from $ slaDeadline params) $ txInfoValidRange info

    correctPenaltyAmount :: Bool
    correctPenaltyAmount = 
        let val = valueSpent info
        in val == singleton (adaSymbol, adaToken) (penaltyAmount params)

typedValidator :: SLAParams -> Scripts.TypedValidator SLA
typedValidator params = Scripts.mkTypedValidator @SLA
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SLADatum @SLARedeemer

validator :: SLAParams -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: SLAParams -> ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: SLAParams -> Address
scrAddress = scriptAddress . validator

data SLA
instance Scripts.ValidatorTypes SLA where
    type instance DatumType SLA = SLADatum
    type instance RedeemerType SLA = SLARedeemer