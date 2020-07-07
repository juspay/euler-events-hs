{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Chargeback where

import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import           Euler.Class                  (Event (toProtoEvent))
import           Prelude                      hiding (id)

import qualified Euler.Proto.Event            as Proto
import qualified Euler.Proto.Event.Chargeback as Proto
import           Euler.Util                   (fromInt, fromSumType, fromText, fromUTCTime)

data Chargeback =
  Chargeback
    { id                  :: Text
    , version             :: Int
    , amount              :: Double
    , chargebackStatus    :: ChargebackStatus
    , merchantAccountId   :: Int
    , transactionDetailId :: Maybe Int
    , objectReferenceId   :: Text
    -- extra info
    , eventType           :: ChargebackEventType
    }

data ChargebackStatus =
  ChargebackStatus
    { dateCreated       :: UTCTime
    , dateResolved      :: Maybe UTCTime
    , statusDescription :: Text
    , lastUpdated       :: UTCTime
    }

data ChargebackEventType
  = ChargebackCreate
  | ChargebackUpdate

instance Event Chargeback where
  toProtoEvent Chargeback {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventChargeback $
          Proto.Chargeback
            { Proto.chargebackId = fromText id
            , Proto.chargebackVersion = fromInt version
            , Proto.chargebackAmount = amount
            , Proto.chargebackChargebackStatus =
                Just . fromChargebackStatus $ chargebackStatus
            , Proto.chargebackMerchantAccountId = fromInt merchantAccountId
            , Proto.chargebackObjectReferenceId = fromText objectReferenceId
            , Proto.chargebackMaybeTransactionDetailId =
                Proto.ChargebackMaybeTransactionDetailIdTransactionDetailId .
                fromInt <$>
                transactionDetailId
            , Proto.chargebackChargebackEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromChargebackStatus :: ChargebackStatus -> Proto.ChargebackStatus
fromChargebackStatus ChargebackStatus {..} =
  Proto.ChargebackStatus
    { Proto.chargebackStatusDateCreated = fromUTCTime dateCreated
    , Proto.chargebackStatusStatusDescription = fromText statusDescription
    , Proto.chargebackStatusLastUpdated = fromUTCTime lastUpdated
    , Proto.chargebackStatusMaybeDateResolved =
        Proto.ChargebackStatusMaybeDateResolvedDateResolved . fromUTCTime <$>
        dateResolved
    }

fromEventType :: ChargebackEventType -> Proto.ChargebackEventType
fromEventType eventType =
  case eventType of
    ChargebackCreate -> Proto.ChargebackEventTypeCHARGEBACK_EVENT_CREATE
    ChargebackUpdate -> Proto.ChargebackEventTypeCHARGEBACK_EVENT_UPDATE
