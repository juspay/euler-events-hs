module Euler.Events.Types.TxnCardInfo where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text, toUpper)
import           Data.Time                  (UTCTime)
import           Euler.Events.Class         (Event (toEventPS))
import qualified Euler.Events.Types.EventPS as EventPS
import           Euler.Events.Util          (tshow)
import           GHC.Generics               (Generic)

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
    , xRequestId         :: Text
    , merchantId         :: Maybe Text
    , hostname           :: Text
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
  = Create
  | Update
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Event TxnCardInfo where
  toEventPS txnCardInfo@TxnCardInfo {..} =
    let action' = toUpper . tshow $ eventType
     in EventPS.EventPS
          { EventPS.timestamp = timestamp
          , EventPS.hostname = hostname
          , EventPS.xRequestId = xRequestId
          , EventPS.txnUuid = Just txnUuid
          , EventPS.orderId = Just orderId
          , EventPS.merchantId = merchantId
          , EventPS.action = action'
          , message =
              EventPS.Message
                { EventPS.model = "txn_card_info"
                , EventPS.action' = action'
                , EventPS.data' = txnCardInfo
                }
          }
