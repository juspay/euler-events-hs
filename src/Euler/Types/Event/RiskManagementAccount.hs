{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.RiskManagementAccount where

import           Data.Text                               (Text)
import           Data.Time                               (UTCTime)
import           Euler.Class                             (Event (toProtoEvent))
import           Prelude                                 hiding (id)

import qualified Euler.Proto.Event                       as Proto
import qualified Euler.Proto.Event.RiskManagementAccount as Proto
import           Euler.Util                              (fromInt, fromSumType, fromText, fromUTCTime)

data RiskManagementAccount =
  RiskManagementAccount
    { id                 :: Int
    , version            :: Int
    , accountDetailsJson :: Text
    , merchantAccountId  :: Int
    , provider           :: Text
    , dateCreated        :: Maybe UTCTime
    , lastUpdated        :: Maybe UTCTime
    , riskDsl            :: Maybe Text
    -- extra info
    , eventType          :: RiskManagementAccountEventType
    }

data RiskManagementAccountEventType
  = RiskManagementAccountCreate
  | RiskManagementAccountUpdate

instance Event RiskManagementAccount where
  toProtoEvent RiskManagementAccount {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventRiskManagementAccount $
          Proto.RiskManagementAccount
            { Proto.riskManagementAccountId = fromInt id
            , Proto.riskManagementAccountVersion = fromInt version
            , Proto.riskManagementAccountAccountDetailsJson =
                fromText accountDetailsJson
            , Proto.riskManagementAccountMerchantAccountId =
                fromInt merchantAccountId
            , Proto.riskManagementAccountProvider = fromText provider
            , Proto.riskManagementAccountMaybeDateCreated =
                Proto.RiskManagementAccountMaybeDateCreatedDateCreated .
                fromUTCTime <$>
                dateCreated
            , Proto.riskManagementAccountMaybeLastUpdated =
                Proto.RiskManagementAccountMaybeLastUpdatedLastUpdated .
                fromUTCTime <$>
                lastUpdated
            , Proto.riskManagementAccountMaybeRiskDsl =
                Proto.RiskManagementAccountMaybeRiskDslRiskDsl . fromText <$>
                riskDsl
            , Proto.riskManagementAccountRiskManagementAccountEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromEventType ::
     RiskManagementAccountEventType -> Proto.RiskManagementAccountEventType
fromEventType eventType =
  case eventType of
    RiskManagementAccountCreate ->
      Proto.RiskManagementAccountEventTypeRISK_MANAGEMENT_ACCOUNT_EVENT_CREATE
    RiskManagementAccountUpdate ->
      Proto.RiskManagementAccountEventTypeRISK_MANAGEMENT_ACCOUNT_EVENT_UPDATE
