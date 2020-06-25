module Euler.Sink.Kafka where

import           Data.ByteString (ByteString)
import           Kafka.Producer  (BrokerAddress (BrokerAddress), KafkaLogLevel (KafkaLogDebug),
                                  ProducePartition (UnassignedPartition), ProducerProperties,
                                  ProducerRecord (ProducerRecord), Timeout (Timeout), TopicName (TopicName),
                                  brokersList, closeProducer, deliveryCallback, logLevel, newProducer, prKey,
                                  prPartition, prTopic, prValue, produceMessage, sendTimeout, setCallback)

import           Euler.Constants (kafkaBrokerAddress, kafkaTargetTopic, kafkaTimeout)

-- TODO: This is a very early stage code written just to test things out. Strengthen it
-- Global producer properties
producerProps :: ProducerProperties
producerProps =
  brokersList [BrokerAddress kafkaBrokerAddress] <>
  sendTimeout (Timeout kafkaTimeout) <>
  setCallback (deliveryCallback print) <> logLevel KafkaLogDebug

-- Topic to send messages to
targetTopic :: TopicName
targetTopic = TopicName kafkaTargetTopic

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v =
  ProducerRecord
    { prTopic = targetTopic
    , prPartition = UnassignedPartition
    , prKey = k
    , prValue = v
    }

sendToKafka :: ByteString -> IO () --(Either KafkaError ())
sendToKafka msg = do
  (Right prod) <- newProducer producerProps
  maybeError <- produceMessage prod (mkMessage Nothing (Just msg))
  closeProducer prod
  print maybeError
  pure ()
