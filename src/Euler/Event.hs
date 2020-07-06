module Euler.Event where

import           Data.ByteString.Lazy       (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Euler.Class                (Event (toProtoEvent))
import           Euler.Sink.Kafka           (KafkaConfig, sendToKafka)
import           Proto3.Suite               (toLazyByteString)
import qualified Proto3.Suite.JSONPB        as ProtoJson

data Sink
  = Stdout
  | Kafka KafkaConfig

-- mkMsg :: Sink -> (E.Event -> IO ())
-- init - strLogger = mkMsg Kafka
send :: Event a => Sink -> a -> IO ()
send sink event =
  let msg = encode sink event
   in case sink of
        Stdout            -> BSLC.putStrLn msg
        Kafka kafkaConfig -> sendToKafka kafkaConfig $ toStrict msg

encode :: Event a => Sink -> a -> ByteString
encode sink event =
  let msg = toProtoEvent event
   in case sink of
        Kafka _ -> toLazyByteString msg
        Stdout  -> ProtoJson.encode ProtoJson.defaultOptions msg
