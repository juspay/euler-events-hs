module Euler.Events.Types.Event where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

data Event a =
  Event
    { metadata       :: EventMetadata
    , libraryVersion :: Text
    , event          :: EventType
    , message        :: a
    }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventMetadata =
  EventMetadata
    { timestamp  :: UTCTime
    , hostname   :: Text
    , xRequestId :: Text
    , txnUuid    :: Maybe Text
    , orderId    :: Maybe Text
    , merchantId :: Maybe Text
    }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventType
  = OrderEvent
  | TxnEvent
  | TxnCardInfoEvent
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
