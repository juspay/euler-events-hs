module Euler.Events.Types.EventPS where

import           Data.Aeson          (ToJSON, Value (Object), object, (.=))
import qualified Data.Aeson          as Aeson
import           Data.HashMap.Strict (delete, empty, insert)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (foldl')
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (TimeZone (TimeZone), utcToLocalTime)

data EventPS a =
  EventPS
    { timestamp  :: UTCTime
    , hostname   :: Text
    , xRequestId :: Text
    , txnUuid    :: Maybe Text
    , orderId    :: Maybe Text
    , merchantId :: Maybe Text
    , action     :: Text
    , message    :: Message a
    }

instance (ToJSON a) => ToJSON (EventPS a) where
  toJSON EventPS {..} =
    object
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
      , "message_type" .= ("json" :: Text)
      , "action" .= action
      , "message" .= message
      ]

data Message a =
  Message
    { model   :: Text
    , data'   :: a
    , action' :: Text
    }

instance (ToJSON a) => ToJSON (Message a) where
  toJSON Message {..} =
    let message =
          object
            [ "log_type" .= ("DB" :: Text)
            , "data" .= data'
            , "action" .= action'
            , "model" .= model
            ]
     in Object $
        case message of
          Object obj ->
            insert
              "data"
              (Object $
               case HashMap.lookup "data" obj of
                 Just (Object dataDict) ->
                   foldl'
                     (flip delete)
                     dataDict
                     ["timestamp", "xRequestId", "eventType", "hostname"]
                 _ -> empty)
              obj
          _ -> empty
