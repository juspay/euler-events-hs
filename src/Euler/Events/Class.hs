-- is using this okay?
{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Events.Class where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text  (Text)

type ErrorText = Text

class (ToJSON a, FromJSON a) =>
      Event a


class Logger config logger | config -> logger where
  initLogger :: config -> IO (Either ErrorText logger)
  log :: Event e => config -> logger -> e -> IO (Maybe ErrorText)
  closeLogger :: logger -> IO (Maybe ErrorText)
