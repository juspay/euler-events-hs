module Euler.Event where

import           Data.ByteString.Lazy       (ByteString, toStrict)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Euler.Class                (Event (toProtoEvent))
import           Euler.Sink.Kafka           (sendToKafka)
import           Proto3.Suite               (toLazyByteString)

data Sink
  = Stdout
  | Kafka -- add KafkaConfig

-- mkMsg :: Receiver -> (E.Event -> IO ())
-- init - strLogger = mkMsg Kafka
send :: Event a => Sink -> a -> IO ()
send sink event =
  let msg = encode sink event
   in case sink of
        Stdout -> BSLC.putStrLn msg
        Kafka  -> sendToKafka $ toStrict msg

encode :: Event a => Sink -> a -> ByteString
encode sink event =
  case sink of
    Kafka  -> toLazyByteString . toProtoEvent $ event
    Stdout -> pack . show . toProtoEvent $ event -- change to json
