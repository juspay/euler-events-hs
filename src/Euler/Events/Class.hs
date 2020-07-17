-- is using this okay?
{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Events.Class where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import           Euler.Events.Types.EventPS (EventPS)

type ErrorText = Text

class (ToJSON a, FromJSON a) =>
      Event a
  where
  toEventPS :: a -> EventPS a

class Logger config logger | config -> logger where
  initLogger :: config -> IO (Either ErrorText logger)
  logEvent :: Event e => Bool -> config -> logger -> e -> IO (Maybe ErrorText)
  closeLogger :: logger -> IO (Maybe ErrorText)
