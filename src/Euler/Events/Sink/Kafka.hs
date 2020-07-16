module Euler.Events.Sink.Kafka where

import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (Text)
import           Euler.Events.Class   (Logger (closeLogger, initLogger, log))
import           Kafka.Producer       (BrokerAddress (BrokerAddress), KafkaLogLevel (KafkaLogDebug), KafkaProducer,
                                       ProducePartition (UnassignedPartition), ProducerProperties,
                                       ProducerRecord (ProducerRecord), Timeout (Timeout), TopicName (TopicName),
                                       brokersList, closeProducer, deliveryCallback, logLevel, newProducer, prKey,
                                       prPartition, prTopic, prValue, produceMessage, sendTimeout, setCallback)

import           Data.Bifunctor       (first)
import           Data.Functor         (($>))
import           Euler.Events.Util    (tshow)

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

instance Logger KafkaConfig KafkaProducer where
  initLogger config = first tshow <$> newProducer (producerProps config)
  log config kafkaProducer event =
    (fmap . fmap)
      tshow
      (produceMessage kafkaProducer $
       mkMessage Nothing (Just . toStrict . encode $ event) (topic config))
  closeLogger kafkaProducer = closeProducer kafkaProducer $> Nothing
