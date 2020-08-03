-- is using this okay?
{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Events.Class where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.ByteString.Lazy     (ByteString)
import           Data.Text                (Text)
import           Euler.Events.Constants   (eventLibraryVersion)
import           Euler.Events.Types.Event (Event (Event), EventMetadata, EventType)
import qualified Euler.Events.Types.Event as Event

type ErrorText = Text

class (ToJSON a, FromJSON a) =>
      EventPayload a
  where
  toEvent :: EventMetadata -> a -> Event a
  toEvent' :: EventType -> EventMetadata -> a -> Event a
  toEvent' eventType metadata eventPayload =
    Event
      { Event.metadata = metadata
      , Event.eventLibraryVersion = eventLibraryVersion
      , Event.event = eventType
      , Event.message = eventPayload
      }

class Logger config logger where
  initLogger :: config -> IO (Either ErrorText logger)
  toLazyByteString ::
       EventPayload a => config -> logger -> EventMetadata -> a -> ByteString
  logEvent ::
       EventPayload a
    => config
    -> logger
    -> EventMetadata
    -> a
    -> IO (Maybe ErrorText)
  closeLogger :: logger -> IO (Maybe ErrorText)

class MetricsLogger config logger operation result
  | config -> logger
  , config -> operation
  , config -> result
  , logger -> config
  , logger -> operation
  , logger -> result
  where
  initMetricsLogger :: config -> IO (Either ErrorText logger)
  metricsEvent :: logger -> operation -> IO result
