{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Mandate where

import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           Euler.Class                   (Event (toProtoEvent))
import           Prelude                       hiding (id)

import qualified Euler.Proto.Event             as Proto
import qualified Euler.Proto.Event.Mandate     as Proto
import           Euler.Types.Event.Order       (Currency, fromCurrency)
import           Euler.Types.Event.TxnCardInfo (PaymentMethodType, fromPaymentMethodType)
import           Euler.Types.Event.TxnDetail   (Gateway, fromGateway)
import           Euler.Util                    (fromInt, fromSumType, fromText, fromUTCTime)

data Mandate =
  Mandate
    { id                       :: Int
    , merchantId               :: Text
    , endDate                  :: Maybe UTCTime
    , startDate                :: Maybe UTCTime
    , maxAmount                :: Maybe Double
    , merchantCustomerId       :: Maybe Text
    , paymentMethod            :: Maybe Text
    , paymentMethodType        :: Maybe PaymentMethodType
    , status                   :: MandateStatus
    , token                    :: Text
    , mandateId                :: Text
    , paymentMethodId          :: Maybe Text
    , gateway                  :: Maybe Gateway
    , gatewayParams            :: Maybe Text
    , authOrderId              :: Maybe Int
    , activatedAt              :: Maybe UTCTime
    , dateCreated              :: UTCTime
    , lastModified             :: UTCTime
    , authTransactionCardInfo  :: Maybe Text
    , currency                 :: Maybe Currency
    , merchantGatewayAccountId :: Maybe Int
    , metadata                 :: Maybe Text
    --extra info
    , eventType                :: MandateEventType
    }

data MandateStatus
  = Created
  | Active
  | Paused
  | Revoking
  | Failure
  | Pending

data MandateEventType
  = MandateCreate
  | MandateUpdate

instance Event Mandate where
  toProtoEvent Mandate {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventMandate $
          Proto.Mandate
            { Proto.mandateId = fromInt id
            , Proto.mandateMerchantId = fromText merchantId
            , Proto.mandateStatus = fromSumType . fromStatus $ status
            , Proto.mandateToken = fromText token
            , Proto.mandateMandateId = fromText mandateId
            , Proto.mandateDateCreated = fromUTCTime dateCreated
            , Proto.mandateLastModified = fromUTCTime lastModified
            , Proto.mandateMaybeEndDate =
                Proto.MandateMaybeEndDateEndDate . fromUTCTime <$> endDate
            , Proto.mandateMaybeStartDate =
                Proto.MandateMaybeStartDateStartDate . fromUTCTime <$> startDate
            , Proto.mandateMaybeMaxAmount =
                Proto.MandateMaybeMaxAmountMaxAmount <$> maxAmount
            , Proto.mandateMaybeMerchantCustomerId =
                Proto.MandateMaybeMerchantCustomerIdMerchantCustomerId .
                fromText <$>
                merchantCustomerId
            , Proto.mandateMaybePaymentMethod =
                Proto.MandateMaybePaymentMethodPaymentMethod . fromText <$>
                paymentMethod
            , Proto.mandateMaybePaymentMethodType =
                Proto.MandateMaybePaymentMethodTypePaymentMethodType .
                fromSumType . fromPaymentMethodType <$>
                paymentMethodType
            , Proto.mandateMaybePaymentMethodId =
                Proto.MandateMaybePaymentMethodIdPaymentMethodId . fromText <$>
                paymentMethodId
            , Proto.mandateMaybeGateway =
                Proto.MandateMaybeGatewayGateway . fromSumType . fromGateway <$>
                gateway
            , Proto.mandateMaybeGatewayParams =
                Proto.MandateMaybeGatewayParamsGatewayParams . fromText <$>
                gatewayParams
            , Proto.mandateMaybeAuthOrderId =
                Proto.MandateMaybeAuthOrderIdAuthOrderId . fromInt <$>
                authOrderId
            , Proto.mandateMaybeActivatedAt =
                Proto.MandateMaybeActivatedAtActivatedAt . fromUTCTime <$>
                activatedAt
            , Proto.mandateMaybeAuthTransactionCardInfo =
                Proto.MandateMaybeAuthTransactionCardInfoAuthTransactionCardInfo .
                fromText <$>
                authTransactionCardInfo
            , Proto.mandateMaybeCurrency =
                Proto.MandateMaybeCurrencyCurrency . fromSumType . fromCurrency <$>
                currency
            , Proto.mandateMaybeMerchantGatewayAccountId =
                Proto.MandateMaybeMerchantGatewayAccountIdMerchantGatewayAccountId .
                fromInt <$>
                merchantGatewayAccountId
            , Proto.mandateMaybeMetadata =
                Proto.MandateMaybeMetadataMetadata . fromText <$> metadata
            , Proto.mandateMandateEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromStatus :: MandateStatus -> Proto.MandateStatus
fromStatus status =
  case status of
    Created  -> Proto.MandateStatusCREATED
    Active   -> Proto.MandateStatusACTIVE
    Paused   -> Proto.MandateStatusPAUSED
    Revoking -> Proto.MandateStatusREVOKING
    Failure  -> Proto.MandateStatusFAILURE
    Pending  -> Proto.MandateStatusPENDING

fromEventType :: MandateEventType -> Proto.MandateEventType
fromEventType eventType =
  case eventType of
    MandateCreate -> Proto.MandateEventTypeMANDATE_EVENT_CREATE
    MandateUpdate -> Proto.MandateEventTypeMANDATE_EVENT_UPDATE
