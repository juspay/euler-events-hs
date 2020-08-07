module Euler.Events.Types.Metric where

import           Data.Text (Text)

data MetricOperation a
  = ReadyUp
  | ReadyDown (MetricResult a)
  | IncrementClientAuthTokenGenerated Text
  | IncrementOrderStatusCacheAdd Text
  | IncrementOrderStatusCacheHit Text
  | IncrementOrderStatusCacheMiss Text

data MetricResult a
  = ReadyUpResult a
  | ReadyDownResult
  | IncrementedClientAuthTokenGenerated
  | IncrementedOrderStatusCacheAdd
  | IncrementedOrderStatusCacheHit
  | IncrementedOrderStatusCacheMiss
  | MetricError Text
