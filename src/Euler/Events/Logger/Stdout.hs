module Euler.Events.Logger.Stdout where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor               (($>))
import           Euler.Events.Class         (EventPayload (toEvent),
                                             Logger (closeLogger, initLogger, logEvent, toLazyByteString))

data StdoutConfig =
  StdoutConfig

instance Logger StdoutConfig () where
  initLogger _config = pure . Right $ ()
  toLazyByteString _config _logger metadata = encode . toEvent metadata
  logEvent config logger metadata eventPayload =
    (BSL.putStrLn . toLazyByteString config logger metadata $ eventPayload) $>
    Nothing
  closeLogger () = pure Nothing
