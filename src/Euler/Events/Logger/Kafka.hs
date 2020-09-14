module Euler.Events.Logger.Kafka where

import Data.Aeson (encode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Functor (($>))
import Data.Text (Text)
import Euler.Events.Class
  ( ErrorText,
    EventPayload (toEvent),
    Logger (closeLogger, initLogger, logEvent, toLazyByteString),
  )
import Euler.Events.Types.Event (EventMetadata)
import Euler.Events.Util (tshow)
import Kafka.Producer
  ( BrokerAddress (BrokerAddress),
    KafkaLogLevel (KafkaLogDebug),
    KafkaProducer,
    ProducePartition (UnassignedPartition),
    ProducerProperties,
    ProducerRecord (ProducerRecord),
    Timeout (Timeout),
    TopicName (TopicName),
    brokersList,
    closeProducer,
    deliveryCallback,
    logLevel,
    newProducer,
    prKey,
    prPartition,
    prTopic,
    prValue,
    produceMessage,
    sendTimeout,
    setCallback,
  )

-- TODO: This is a very early stage code written just to test things out. Strengthen it
data KafkaConfig = KafkaConfig
  { brokerAddress :: Text,
    timeout :: Int,
    topic :: Text
  }

-- Global producer properties
producerProps :: KafkaConfig -> ProducerProperties
producerProps config =
  brokersList [BrokerAddress (brokerAddress config)]
    <> sendTimeout (Timeout (timeout config))
    <> setCallback (deliveryCallback print)
    <> logLevel KafkaLogDebug

mkMessage :: Maybe ByteString -> Maybe ByteString -> Text -> ProducerRecord
mkMessage k v topicName =
  ProducerRecord
    { prTopic = TopicName topicName,
      prPartition = UnassignedPartition,
      prKey = k,
      prValue = v
    }

instance Logger KafkaConfig KafkaProducer where
  initLogger :: KafkaConfig -> IO (Either ErrorText KafkaProducer)
  initLogger config = first tshow <$> newProducer (producerProps config)
  toLazyByteString ::
    (EventPayload a) =>
    KafkaConfig ->
    KafkaProducer ->
    EventMetadata ->
    a ->
    ByteString
  toLazyByteString _config _logger metadata = encode . toEvent metadata
  logEvent ::
    EventPayload a =>
    KafkaConfig ->
    KafkaProducer ->
    EventMetadata ->
    a ->
    IO (Maybe ErrorText)
  logEvent config kafkaProducer metadata eventPayload =
    (fmap . fmap)
      tshow
      ( produceMessage kafkaProducer $
          mkMessage
            Nothing
            ( Just . toStrict . toLazyByteString config kafkaProducer metadata $
                eventPayload
            )
            (topic config)
      )
  closeLogger :: KafkaProducer -> IO (Maybe ErrorText)
  closeLogger kafkaProducer = closeProducer kafkaProducer $> Nothing
