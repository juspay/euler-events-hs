module Euler.Events.Types.Txn where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Euler.Events.Class       (EventPayload (toEvent, toEvent'))
import           Euler.Events.Types.Event (Event, EventMetadata, EventType (TxnEvent))
import           GHC.Generics             (Generic)

data Txn =
  Txn
    { version      :: Int
    , orderId      :: Text
    , txnUuid      :: Maybe Text
    , txnId        :: Text
    , txnAmount    :: Maybe Double
    , status       :: TxnStatus
    , dateCreated  :: Maybe UTCTime
    , lastModified :: Maybe UTCTime
    , merchantId   :: Maybe Text
    , gateway      :: Maybe Text
    -- extra info
    , eventType    :: TxnEventType
    }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnStatus
  = STARTED
  | AUTHENTICATION_FAILED
  | JUSPAY_DECLINED
  | PENDING_VBV
  | VBV_SUCCESSFUL
  | AUTHORIZED
  | AUTHORIZATION_FAILED
  | CHARGED
  | AUTHORIZING
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  | NOP
  | CAPTURE_INITIATED
  | CAPTURE_FAILED
  | VOID_FAILED
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload Txn where
  toEvent :: EventMetadata -> Txn -> Event Txn
  toEvent = toEvent' TxnEvent
