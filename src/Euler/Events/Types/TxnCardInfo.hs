module Euler.Events.Types.TxnCardInfo where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (TxnCardInfoEvent))
import GHC.Generics (Generic)

data TxnCardInfo = TxnCardInfo
  { orderId :: Text,
    txnUuid :: Text,
    dateCreated :: Maybe UTCTime,
    cardType :: Maybe Text,
    cardIssuerBankName :: Maybe Text,
    paymentMethodType :: Maybe PaymentMethodType,
    paymentMethod :: Maybe Text,
    -- extra info
    eventType :: TxnCardInfoEventType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PaymentMethodType
  = WALLET
  | UPI
  | NB
  | CARD
  | PAYLATER
  | CONSUMER_FINANCE
  | REWARD
  | CASH
  | UNKNOWN
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnCardInfoEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload TxnCardInfo where
  toEvent :: EventMetadata -> TxnCardInfo -> Event TxnCardInfo
  toEvent = toEvent' TxnCardInfoEvent
