module Euler.Events.Types.TxnDetail where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Euler.Events.Class (Event)
import           GHC.Generics       (Generic)

data Txn =
  Txn
    { version      :: Int
    , orderId      :: Text
    , txnUuid      :: Maybe Text
    , txnId        :: Text
    , txnAmount    :: Maybe Double
    , currency     :: Maybe Text
    , status       :: TxnStatus
    , hostname     :: Text
    , errorMessage :: Maybe Text
    , dateCreated  :: Maybe UTCTime
    , lastModified :: Maybe UTCTime
    , merchantId   :: Maybe Text
    , gateway      :: Maybe Text
    -- extra info
    , eventType    :: TxnEventType
    , timestamp    :: UTCTime -- can we use lastModified for this
    }
  deriving (Generic)
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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnEventType
  = TxnCreate
  | TxnUpdate
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Event Txn
