module Euler.Constants where

import           Data.Text (Text)

kafkaBrokerAddress :: Text
kafkaBrokerAddress = "localhost:9092"

kafkaTimeout :: Int
kafkaTimeout = 10000

kafkaTargetTopic :: Text
kafkaTargetTopic = "test"
