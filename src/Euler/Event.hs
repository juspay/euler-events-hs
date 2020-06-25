module Euler.Event where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Text.Lazy             (fromStrict)
import           Data.Time.Clock            (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import qualified Euler.Proto.Event          as P
import qualified Euler.Proto.Event.Order    as P
import qualified Euler.Sink.Kafka           as K
import qualified Euler.Types.Event          as E
import qualified Euler.Types.Event.Order    as E
import qualified Proto3.Suite               as P

data Receiver
  = Stdout
  | Kafka

-- mkMsg :: Receiver -> (E.Event -> IO ())
-- init - strLogger = mkMsg Kafka
send :: Receiver -> E.Event -> IO ()
send receiver event =
  let msg = encode event
   in case receiver of
        Stdout -> BSLC.putStrLn msg
        Kafka  -> K.send $ BSL.toStrict msg

encode :: E.Event -> BSL.ByteString
encode = (P.toLazyByteString :: P.Event -> BSL.ByteString) . toProtobufType

toProtobufType :: E.Event -> P.Event
toProtobufType (E.EventOrder order) =
  P.Event
    { P.eventEvent =
        Just $
        P.EventEventOrder $
        P.Order
          { P.orderId = fromIntegral $ E.id order
          , P.orderVersion = fromIntegral $ E.version order
          , P.orderAmount = E.amount order
          , P.orderCurrency = toProtobufCurrency $ E.currency order --make is as `makeSum . toProtobufCurrency $ ...`
          , P.orderMerchantId = fromStrict $ E.merchantId order
          , P.orderOrderId = fromStrict $ E.orderId order
          , P.orderOrderUuid = fromStrict $ E.orderId order
          , P.orderOrderType = toProtobufOrderType $ E.orderType order
          , P.orderOrderStatus = toProtobufOrderStatus $ E.orderStatus order
          , P.orderCustomerId = maybe "" fromStrict $ E.customerId order
          , P.orderCustomerEmail = maybe "" fromStrict $ E.customerEmail order
          , P.orderCustomerPhone = maybe "" fromStrict $ E.customerPhone order
          , P.orderBillingAddressId =
              maybe 0 fromIntegral $ E.billingAddressId order
          , P.orderShippingAddressId =
              maybe 0 fromIntegral $ E.shippingAddressId order
          , P.orderUdf = Just $ toProtobufUDF (E.udf order)
          , P.orderDescription = maybe "" fromStrict $ E.description order
          , P.orderReturnUrl = maybe "" fromStrict $ E.returnUrl order
          , P.orderAmountRefunded = E.amountRefunded order
          , P.orderRefundedEntirely = E.refundedEntirely order
          , P.orderProductId = maybe "" fromStrict $ E.productId order
          , P.orderMandate = toProtobufMandate $ E.mandate order
          , P.orderLastSynced =
              maybe 0 (fromIntegral . utcTimeToNanos) $ E.lastSynced order
          , P.orderDateCreated =
              fromIntegral . utcTimeToNanos $ E.dateCreated order
          , P.orderLastModified =
              fromIntegral . utcTimeToNanos $ E.lastModified order
          , P.orderOrderEventType = toProtobufOrderEventType $ E.eventType order
          }
    }

makeSum :: a -> P.Enumerated a
makeSum = P.Enumerated . Right

-- make it as E.Currency -> P.Currency
toProtobufCurrency :: E.Currency -> P.Enumerated P.Currency
toProtobufCurrency curr =
  case curr of
    E.INR -> makeSum P.CurrencyINR
    E.USD -> makeSum P.CurrencyUSD
    E.GBP -> makeSum P.CurrencyEUR
    E.EUR -> makeSum P.CurrencyEUR
    E.AED -> makeSum P.CurrencyAED

toProtobufOrderType :: E.OrderType -> P.Enumerated P.OrderType
toProtobufOrderType orderType =
  case orderType of
    E.MANDATE_REGISTER -> makeSum P.OrderTypeMANDATE_REGISTER
    E.MANDATE_PAYMENT  -> makeSum P.OrderTypeMANDATE_PAYMENT
    E.ORDER_PAYMENT    -> makeSum P.OrderTypeORDER_PAYMENT

toProtobufOrderStatus :: E.OrderStatus -> P.Enumerated P.OrderStatus
toProtobufOrderStatus orderStatus =
  case orderStatus of
    E.OrderStatusNew -> makeSum P.OrderStatusORDER_STATUS_NEW
    E.OrderStatusSuccess -> makeSum P.OrderStatusORDER_STATUS_SUCCESS
    E.OrderStatusNotFound -> makeSum P.OrderStatusORDER_STATUS_NOT_FOUND
    E.OrderStatusError -> makeSum P.OrderStatusORDER_STATUS_ERROR
    E.OrderStatusJuspayDeclined ->
      makeSum P.OrderStatusORDER_STATUS_JUSPAY_DECLINED
    E.OrderStatusPendingAuthentication ->
      makeSum P.OrderStatusORDER_STATUS_PENDING_AUTHENTICATION
    E.OrderStatusAuthenticationFailed ->
      makeSum P.OrderStatusORDER_STATUS_AUTHENTICATION_FAILED
    E.OrderStatusAuthorizationFailed ->
      makeSum P.OrderStatusORDER_STATUS_AUTHORIZATION_FAILED
    E.OrderStatusAuthorizing -> makeSum P.OrderStatusORDER_STATUS_AUTHORIZING
    E.OrderStatusAuthorized -> makeSum P.OrderStatusORDER_STATUS_AUTHORIZED
    E.OrderStatusCreated -> makeSum P.OrderStatusORDER_STATUS_CREATED
    E.OrderStatusCodInitiated -> makeSum P.OrderStatusORDER_STATUS_COD_INITIATED
    E.OrderStatusVoided -> makeSum P.OrderStatusORDER_STATUS_VOIDED
    E.OrderStatusVoidInitiated ->
      makeSum P.OrderStatusORDER_STATUS_VOID_INITIATED
    E.OrderStatusCaptureInitiated ->
      makeSum P.OrderStatusORDER_STATUS_CAPTURE_INITIATED
    E.OrderStatusCaptureFailed ->
      makeSum P.OrderStatusORDER_STATUS_CAPTURE_FAILED
    E.OrderStatusVoidFailed -> makeSum P.OrderStatusORDER_STATUS_VOID_FAILED
    E.OrderStatusAutoRefunded -> makeSum P.OrderStatusORDER_STATUS_AUTO_REFUNDED

toProtobufUDF :: E.UDF -> P.UDF
toProtobufUDF udf =
  P.UDF
    { P.udfUdf1 = maybe "" fromStrict $ E.udf1 udf
    , P.udfUdf2 = maybe "" fromStrict $ E.udf2 udf
    , P.udfUdf3 = maybe "" fromStrict $ E.udf3 udf
    , P.udfUdf4 = maybe "" fromStrict $ E.udf4 udf
    , P.udfUdf5 = maybe "" fromStrict $ E.udf5 udf
    , P.udfUdf6 = maybe "" fromStrict $ E.udf6 udf
    , P.udfUdf7 = maybe "" fromStrict $ E.udf7 udf
    , P.udfUdf8 = maybe "" fromStrict $ E.udf8 udf
    , P.udfUdf9 = maybe "" fromStrict $ E.udf9 udf
    , P.udfUdf10 = maybe "" fromStrict $ E.udf10 udf
    }

toProtobufMandate :: E.MandateFeature -> P.Enumerated P.MandateFeature
toProtobufMandate mandate =
  case mandate of
    E.DISABLED -> makeSum P.MandateFeatureDISABLED
    E.REQUIRED -> makeSum P.MandateFeatureREQUIRED
    E.OPTIONAL -> makeSum P.MandateFeatureOPTIONAL

toProtobufOrderEventType :: E.OrderEventType -> P.Enumerated P.OrderEventType
toProtobufOrderEventType orderEventType =
  case orderEventType of
    E.OrderCreate -> makeSum P.OrderEventTypeORDER_EVENT_CREATE
    E.OrderUpdate -> makeSum P.OrderEventTypeORDER_EVENT_UPDATE

utcTimeToNanos :: UTCTime -> Integer
utcTimeToNanos t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t (posixSecondsToUTCTime 0))) `div`
  1000
