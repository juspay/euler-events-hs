module Euler.Events.Types.TxnDetail where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text, toUpper)
import           Data.Time                  (UTCTime)
import           Euler.Events.Class         (Event (toEventPS))
import qualified Euler.Events.Types.EventPS as EventPS
import           Euler.Events.Util          (tshow)
import           GHC.Generics               (Generic)

data TxnDetail =
  TxnDetail
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
    , timestamp    :: UTCTime
    , xRequestId   :: Text
    , hostname     :: Text
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
  = Create
  | Update
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Event TxnDetail where
  toEventPS txnDetail@TxnDetail {..} =
    let action' = toUpper . tshow $ eventType
     in EventPS.EventPS
          { EventPS.timestamp = timestamp
          , EventPS.hostname = hostname
          , EventPS.xRequestId = xRequestId
          , EventPS.txnUuid = txnUuid
          , EventPS.orderId = Just orderId
          , EventPS.merchantId = merchantId
          , EventPS.action = action'
          , message =
              EventPS.Message
                { EventPS.model = "txn_detail"
                , EventPS.action' = action'
                , EventPS.data' = txnDetail
                }
          }
