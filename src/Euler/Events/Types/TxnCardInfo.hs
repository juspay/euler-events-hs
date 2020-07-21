module Euler.Events.Types.TxnCardInfo where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Euler.Events.Class       (EventPayload (toEvent, toEvent'))
import           Euler.Events.Types.Event (EventType (TxnCardInfoEvent))
import           GHC.Generics             (Generic)

data TxnCardInfo =
  TxnCardInfo
    { orderId            :: Text
    , txnUuid            :: Text
    , dateCreated        :: Maybe UTCTime
    , cardType           :: Maybe Text
    , cardIssuerBankName :: Maybe Text
    , paymentMethodType  :: Maybe PaymentMethodType
    , paymentMethod      :: Maybe Text
    -- extra info
    , eventType          :: TxnCardInfoEventType
    }
  deriving (Generic)
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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnCardInfoEventType
  = CREATE
  | UPDATE
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload TxnCardInfo where
  toEvent = toEvent' TxnCardInfoEvent
