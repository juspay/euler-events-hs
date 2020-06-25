module Euler.Types.Event.Order where

import           Data.Text (Text)
import           Data.Time (UTCTime)

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
  deriving (Show)

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
  deriving (Show)

data OrderType
  = MANDATE_REGISTER
  | MANDATE_PAYMENT
  | ORDER_PAYMENT
  deriving (Show)

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
  deriving (Show)

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
  deriving (Show)

data MandateFeature
  = DISABLED
  | REQUIRED
  | OPTIONAL
  deriving (Show)

data OrderEventType
  = OrderCreate
  | OrderUpdate
  deriving (Show)
