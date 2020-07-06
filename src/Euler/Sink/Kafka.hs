module Euler.Sink.Kafka where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Kafka.Producer  (BrokerAddress (BrokerAddress), KafkaLogLevel (KafkaLogDebug),
                                  ProducePartition (UnassignedPartition), ProducerProperties,
                                  ProducerRecord (ProducerRecord), Timeout (Timeout), TopicName (TopicName),
                                  brokersList, closeProducer, deliveryCallback, logLevel, newProducer, prKey,
                                  prPartition, prTopic, prValue, produceMessage, sendTimeout, setCallback)

-- TODO: This is a very early stage code written just to test things out. Strengthen it
data KafkaConfig =
  KafkaConfig
    { brokerAddress :: Text
    , timeout       :: Int
    , topic         :: Text
    }

-- Global producer properties
producerProps :: KafkaConfig -> ProducerProperties
producerProps config =
  brokersList [BrokerAddress (brokerAddress config)] <>
  sendTimeout (Timeout (timeout config)) <>
  setCallback (deliveryCallback print) <> logLevel KafkaLogDebug

mkMessage :: Maybe ByteString -> Maybe ByteString -> Text -> ProducerRecord
mkMessage k v topicName =
  ProducerRecord
    { prTopic = TopicName topicName
    , prPartition = UnassignedPartition
    , prKey = k
    , prValue = v
    }

sendToKafka :: KafkaConfig -> ByteString -> IO () --(Either KafkaError ())
sendToKafka config msg = do
  (Right prod) <- newProducer (producerProps config)
  maybeError <-
    produceMessage prod (mkMessage Nothing (Just msg) (topic config))
  closeProducer prod
  print maybeError
  pure ()
