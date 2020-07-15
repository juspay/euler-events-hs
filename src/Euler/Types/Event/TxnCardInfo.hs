module Euler.Types.Event.TxnCardInfo where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           Euler.Class  (Event)
import           GHC.Generics (Generic)

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
    , timestamp          :: UTCTime
    }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data PaymentMethodType
  = Wallet
  | UPI
  | NB
  | Card
  | Paylater
  | ConsumerFinance
  | Reward
  | Cash
  | Unknown
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnCardInfoEventType
  = TxnCardInfoCreate
  | TxnCardInfoUpdate
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Event TxnCardInfo
