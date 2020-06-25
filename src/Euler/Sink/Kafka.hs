module Euler.Sink.Kafka where

import           Data.ByteString (ByteString)
import           Kafka.Producer

-- Global producer properties
producerProps :: ProducerProperties
producerProps =
  brokersList [BrokerAddress "localhost:9092"] <>
  sendTimeout (Timeout 10000) <>
  setCallback (deliveryCallback print) <> logLevel KafkaLogDebug

-- Topic to send messages to
targetTopic :: TopicName
targetTopic = TopicName "test"

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v =
  ProducerRecord
    { prTopic = targetTopic
    , prPartition = UnassignedPartition
    , prKey = k
    , prValue = v
    }

send :: ByteString -> IO () --(Either KafkaError ())
send msg = do
  (Right prod) <- newProducer producerProps
  maybeError <- produceMessage prod (mkMessage Nothing (Just msg))
  print maybeError
  pure ()
