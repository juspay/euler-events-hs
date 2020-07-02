{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Order where

import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Euler.Class             (Event (toProtoEvent))
import           Prelude                 hiding (id)

import qualified Euler.Proto.Event       as Proto
import qualified Euler.Proto.Event.Order as Proto
import           Euler.Util              (fromInt, fromMaybeInt, fromMaybeText, fromMaybeUTCTime, fromSumType, fromText,
                                          fromUTCTime)

data Order =
  Order
    { id                :: Int
    , version           :: Int
    , amount            :: Double
    , currency          :: Currency
    , merchantId        :: Text
    , orderId           :: Text
    , orderUuid         :: Text
    , orderType         :: OrderType
    , orderStatus       :: OrderStatus
    , customerId        :: Maybe Text
    , customerEmail     :: Maybe Text
    , customerPhone     :: Maybe Text
    , billingAddressId  :: Maybe Int
    , shippingAddressId :: Maybe Int
    , udf               :: UDF
    , description       :: Maybe Text
    , returnUrl         :: Maybe Text
    , amountRefunded    :: Double
    , refundedEntirely  :: Bool
    , productId         :: Maybe Text
    , mandate           :: MandateFeature
    , lastSynced        :: Maybe UTCTime
    , dateCreated       :: UTCTime
    , lastModified      :: UTCTime
    -- extra info
    , eventType         :: OrderEventType
    }

-- defaultOrder :: Order
-- defaultOrder = Order { id = 1,
--   version = 1
--     , amount = 1.0
--     , currency   = INR
--     , merchantId     = "abc"
--     , orderId           = "abc"
--     , orderUuid         = "abc"
--     , orderType         = ORDER_PAYMENT
--     , orderStatus       = OrderStatusNew
--     , customerId        = Nothing
--     , customerEmail     = Nothing
--     , customerPhone     = Nothing
--     , billingAddressId  = Nothing
--     , shippingAddressId = Nothing
--     , udf               = UDF Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
--     , description       = Nothing
--     , returnUrl         = Nothing
--     , amountRefunded    = 0.0
--     , refundedEntirely  = False
--     , productId         = Nothing
--     , mandate           = DISABLED
--     , lastSynced        = Nothing
--     , dateCreated       :: UTCTime
--     , lastModified      :: UTCTime
--     -- extra info
--     , eventType         :: OrderEventType
--                      }
data Currency
  = INR
  | USD
  | GBP
  | EUR
  | AED

data OrderType
  = MANDATE_REGISTER
  | MANDATE_PAYMENT
  | ORDER_PAYMENT

data OrderStatus
  = OrderStatusNew
  | OrderStatusSuccess
  | OrderStatusNotFound
  | OrderStatusError
  | OrderStatusJuspayDeclined
  | OrderStatusPendingAuthentication
  | OrderStatusAuthenticationFailed
  | OrderStatusAuthorizationFailed
  | OrderStatusAuthorizing
  | OrderStatusAuthorized
  | OrderStatusCreated
  | OrderStatusCodInitiated
  | OrderStatusVoided
  | OrderStatusVoidInitiated
  | OrderStatusCaptureInitiated
  | OrderStatusCaptureFailed
  | OrderStatusVoidFailed
  | OrderStatusAutoRefunded

data UDF =
  UDF
    { udf1  :: Maybe Text
    , udf2  :: Maybe Text
    , udf3  :: Maybe Text
    , udf4  :: Maybe Text
    , udf5  :: Maybe Text
    , udf6  :: Maybe Text
    , udf7  :: Maybe Text
    , udf8  :: Maybe Text
    , udf9  :: Maybe Text
    , udf10 :: Maybe Text
    }

data MandateFeature
  = DISABLED
  | REQUIRED
  | OPTIONAL

data OrderEventType
  = OrderCreate
  | OrderUpdate

instance Event Order where
  toProtoEvent Order {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventOrder $
          Proto.Order
            { Proto.orderId = fromInt id
            , Proto.orderVersion = fromInt version
            , Proto.orderAmount = amount
            , Proto.orderCurrency = fromSumType . fromCurrency $ currency
            , Proto.orderMerchantId = fromText merchantId
            , Proto.orderOrderId = fromText orderId
            , Proto.orderOrderUuid = fromText orderId
            , Proto.orderOrderType = fromSumType . fromOrderType $ orderType
            , Proto.orderOrderStatus =
                fromSumType . fromOrderStatus $ orderStatus
            , Proto.orderCustomerId = fromMaybeText customerId
            , Proto.orderCustomerEmail = fromMaybeText customerEmail
            , Proto.orderCustomerPhone = fromMaybeText customerPhone
            , Proto.orderBillingAddressId = fromMaybeInt billingAddressId
            , Proto.orderShippingAddressId = fromMaybeInt shippingAddressId
            , Proto.orderUdf = Just . fromUdf $ udf
            , Proto.orderDescription = fromMaybeText description
            , Proto.orderReturnUrl = fromMaybeText returnUrl
            , Proto.orderAmountRefunded = amountRefunded
            , Proto.orderRefundedEntirely = refundedEntirely
            , Proto.orderProductId = fromMaybeText productId
            , Proto.orderMandate = fromSumType . fromMandate $ mandate
            , Proto.orderLastSynced = fromMaybeUTCTime lastSynced
            , Proto.orderDateCreated = fromUTCTime dateCreated
            , Proto.orderLastModified = fromUTCTime lastModified
            , Proto.orderOrderEventType =
                fromSumType . fromOrderEventType $ eventType
            }
      }

fromCurrency :: Currency -> Proto.Currency
fromCurrency curr =
  case curr of
    INR -> Proto.CurrencyINR
    USD -> Proto.CurrencyUSD
    GBP -> Proto.CurrencyEUR
    EUR -> Proto.CurrencyEUR
    AED -> Proto.CurrencyAED

fromOrderType :: OrderType -> Proto.OrderType
fromOrderType orderType' =
  case orderType' of
    MANDATE_REGISTER -> Proto.OrderTypeMANDATE_REGISTER
    MANDATE_PAYMENT  -> Proto.OrderTypeMANDATE_PAYMENT
    ORDER_PAYMENT    -> Proto.OrderTypeORDER_PAYMENT

fromOrderStatus :: OrderStatus -> Proto.OrderStatus
fromOrderStatus orderStatus' =
  case orderStatus' of
    OrderStatusNew -> Proto.OrderStatusORDER_STATUS_NEW
    OrderStatusSuccess -> Proto.OrderStatusORDER_STATUS_SUCCESS
    OrderStatusNotFound -> Proto.OrderStatusORDER_STATUS_NOT_FOUND
    OrderStatusError -> Proto.OrderStatusORDER_STATUS_ERROR
    OrderStatusJuspayDeclined -> Proto.OrderStatusORDER_STATUS_JUSPAY_DECLINED
    OrderStatusPendingAuthentication ->
      Proto.OrderStatusORDER_STATUS_PENDING_AUTHENTICATION
    OrderStatusAuthenticationFailed ->
      Proto.OrderStatusORDER_STATUS_AUTHENTICATION_FAILED
    OrderStatusAuthorizationFailed ->
      Proto.OrderStatusORDER_STATUS_AUTHORIZATION_FAILED
    OrderStatusAuthorizing -> Proto.OrderStatusORDER_STATUS_AUTHORIZING
    OrderStatusAuthorized -> Proto.OrderStatusORDER_STATUS_AUTHORIZED
    OrderStatusCreated -> Proto.OrderStatusORDER_STATUS_CREATED
    OrderStatusCodInitiated -> Proto.OrderStatusORDER_STATUS_COD_INITIATED
    OrderStatusVoided -> Proto.OrderStatusORDER_STATUS_VOIDED
    OrderStatusVoidInitiated -> Proto.OrderStatusORDER_STATUS_VOID_INITIATED
    OrderStatusCaptureInitiated ->
      Proto.OrderStatusORDER_STATUS_CAPTURE_INITIATED
    OrderStatusCaptureFailed -> Proto.OrderStatusORDER_STATUS_CAPTURE_FAILED
    OrderStatusVoidFailed -> Proto.OrderStatusORDER_STATUS_VOID_FAILED
    OrderStatusAutoRefunded -> Proto.OrderStatusORDER_STATUS_AUTO_REFUNDED

fromUdf :: UDF -> Proto.UDF
fromUdf udf' =
  Proto.UDF
    { Proto.udfUdf1 = fromMaybeText $ udf1 udf'
    , Proto.udfUdf2 = fromMaybeText $ udf2 udf'
    , Proto.udfUdf3 = fromMaybeText $ udf3 udf'
    , Proto.udfUdf4 = fromMaybeText $ udf4 udf'
    , Proto.udfUdf5 = fromMaybeText $ udf5 udf'
    , Proto.udfUdf6 = fromMaybeText $ udf6 udf'
    , Proto.udfUdf7 = fromMaybeText $ udf7 udf'
    , Proto.udfUdf8 = fromMaybeText $ udf8 udf'
    , Proto.udfUdf9 = fromMaybeText $ udf9 udf'
    , Proto.udfUdf10 = fromMaybeText $ udf10 udf'
    }

fromMandate :: MandateFeature -> Proto.MandateFeature
fromMandate mandate' =
  case mandate' of
    DISABLED -> Proto.MandateFeatureDISABLED
    REQUIRED -> Proto.MandateFeatureREQUIRED
    OPTIONAL -> Proto.MandateFeatureOPTIONAL

fromOrderEventType :: OrderEventType -> Proto.OrderEventType
fromOrderEventType orderEventType =
  case orderEventType of
    OrderCreate -> Proto.OrderEventTypeORDER_EVENT_CREATE
    OrderUpdate -> Proto.OrderEventTypeORDER_EVENT_UPDATE
