module Euler.Events.Types.Event where

import           Data.Aeson            (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format      (defaultTimeLocale, formatTime, parseTimeM)
import           Data.Time.LocalTime   (TimeZone (TimeZone), localTimeToUTC, utcToLocalTime)
import           GHC.Generics          (Generic)

data Event a =
  Event
    { metadata            :: EventMetadata
    , eventLibraryVersion :: Text
    , event               :: EventType
    , message             :: a
    }
  deriving (Show, Eq, Generic)

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

instance (ToJSON a) => ToJSON (Event a) where
  toJSON Event {..} =
    let EventMetadata {..} = metadata
     in object
          [ "timestamp" .=
            formatTime
              defaultTimeLocale
              "%d-%m-%Y %H:%M:%S%03Q"
              (utcToLocalTime (TimeZone 330 False "IST") timestamp)
          , "hostname" .= hostname
          , "x-request-id" .= xRequestId
          , "txn_uuid" .= txnUuid
          , "order_id" .= orderId
          , "merchant_id" .= merchantId
          , "event" .= event
          , "event_library_version" .= eventLibraryVersion
          , "message" .= message
          ]

instance (FromJSON a) => FromJSON (Event a) where
  parseJSON =
    withObject "event" $ \e -> do
      timestampIST <- e .: "timestamp"
      let timestamp =
            fromMaybe
              (posixSecondsToUTCTime 1)
              (localTimeToUTC (TimeZone 330 False "IST") <$>
               parseTimeM
                 False
                 defaultTimeLocale
                 "%d-%m-%Y %H:%M:%S%Q"
                 timestampIST)
      hostname <- e .: "hostname"
      xRequestId <- e .: "x-request-id"
      txnUuid <- e .: "txn_uuid"
      orderId <- e .: "order_id"
      merchantId <- e .: "merchant_id"
      event <- e .: "event"
      eventLibraryVersion <- e .: "event_library_version"
      message <- e .: "message"
      let metadata = EventMetadata {..}
      return Event {..}
