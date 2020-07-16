module Euler.Events.Sink.Stdout where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor               (($>))
import           Euler.Events.Class         (Logger (closeLogger, initLogger, log))

data StdoutConfig =
  StdoutConfig

instance Logger StdoutConfig () where
  initLogger _ = pure . Right $ ()
  log _ () event = (BSL.putStrLn . encode $ event) $> Nothing
  closeLogger () = pure Nothing
