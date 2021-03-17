module Euler.Events.Types.Order where

{-
This module depricated.
Please, use euler-types and make custom type in your api.
See src/Euler/Common/ExtraLog.hs from order-api for example.
This module left untouched for tests.
-}


import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (OrderEvent))
import GHC.Generics (Generic)

data Order = Order
  { orderId :: Text,
    version :: Int,
    amount :: Double,
    status :: OrderStatus,
    merchantId :: Text,
    dateCreated :: UTCTime,
    lastModified :: UTCTime,
    -- extra info
    eventType :: OrderEventType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderStatus
  = NEW
  | SUCCESS
  | NOT_FOUND
  | ERROR
  | JUSPAY_DECLINED
  | PENDING_AUTHENTICATION
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | AUTHORIZING
  | AUTHORIZED
  | CREATED
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  | CAPTURE_INITIATED
  | CAPTURE_FAILED
  | VOID_FAILED
  | AUTO_REFUNDED
  | PARTIAL_CHARGED
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload Order where
  toEvent :: EventMetadata -> Order -> Event Order
  toEvent = toEvent' OrderEvent
