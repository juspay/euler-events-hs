module Euler.Events.Types.Order where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text, toUpper)
import           Data.Time                  (UTCTime)
import           Euler.Events.Class         (Event (toEventPS))
import qualified Euler.Events.Types.EventPS as EventPS
import           Euler.Events.Util          (tshow)
import           GHC.Generics               (Generic)

data Order =
  Order
    { orderId      :: Text
    , version      :: Int
    , amount       :: Double
    , status       :: OrderStatus
    , hostname     :: Text
    , merchantId   :: Text
    , dateCreated  :: UTCTime
    , lastModified :: UTCTime
    -- extra info
    , eventType    :: OrderEventType
    , timestamp    :: UTCTime
    , xRequestId   :: Text
    , txnUuid      :: Maybe Text -- can we get this
    }
  deriving (Generic)
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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderEventType
  = Create
  | Update
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Event Order where
  toEventPS order@Order {..} =
    let action' = toUpper . tshow $ eventType
     in EventPS.EventPS
          { EventPS.timestamp = timestamp
          , EventPS.hostname = hostname
          , EventPS.xRequestId = xRequestId
          , EventPS.txnUuid = txnUuid
          , EventPS.orderId = Just orderId
          , EventPS.merchantId = Just merchantId
          , EventPS.action = action'
          , message =
              EventPS.Message
                { EventPS.model = "order_reference"
                , EventPS.action' = action'
                , EventPS.data' = order
                }
          }
