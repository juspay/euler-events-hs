module Euler.Constants where

import           Data.Int       (Int64)
import           Data.Text      (Text)
import qualified Data.Text.Lazy as Text.Lazy

kafkaBrokerAddress :: Text
kafkaBrokerAddress = "localhost:9092"

kafkaTimeout :: Int
kafkaTimeout = 10000

kafkaTargetTopic :: Text
kafkaTargetTopic = "test"

defaultInt64 :: Int64
defaultInt64 = 0

defaultLazyText :: Text.Lazy.Text
defaultLazyText = ""

defaultBool :: Bool
defaultBool = False

defaultDouble :: Double
defaultDouble = 0.0
