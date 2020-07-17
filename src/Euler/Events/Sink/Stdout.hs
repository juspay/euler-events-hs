module Euler.Events.Sink.Stdout where

import           Data.Aeson                 (encode)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor               (($>))
import           Euler.Events.Class         (Event (toEventPS), Logger (closeLogger, initLogger, logEvent))

data StdoutConfig =
  StdoutConfig

instance Logger StdoutConfig () where
  initLogger _ = pure . Right $ ()
  logEvent isEventPs _ () event =
    (BSL.putStrLn . bool (encode event) (encode . toEventPS $ event) $ isEventPs) $>
    Nothing
  closeLogger () = pure Nothing
