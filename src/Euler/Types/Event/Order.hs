module Euler.Types.Event.Order where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           Euler.Class  (Event)
import           GHC.Generics (Generic)

data Order =
  Order
    { orderId      :: Text
    , version      :: Int
    , amount       :: Double
    , currency     :: Currency
    , orderStatus  :: OrderStatus
    , hostname     :: Text
    , merchantId   :: Text
    , dateCreated  :: UTCTime
    , lastModified :: UTCTime
    -- extra info
    , eventType    :: OrderEventType
    , timestamp    :: UTCTime -- can we use lastModified for this
    }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data Currency
  = INR
  | USD
  | GBP
  | EUR
  | AED
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderEventType
  = OrderCreate
  | OrderUpdate
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Event Order
