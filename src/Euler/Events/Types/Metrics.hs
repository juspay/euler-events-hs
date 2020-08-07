module Euler.Events.Types.Metrics where

import           Data.Text (Text)

data MetricsOperation a
  = ReadyUp
  | ReadyDown (MetricsResult a)
  | IncrementClientAuthTokenGenerated Text
  | IncrementOrderStatusCacheAdd Text
  | IncrementOrderStatusCacheHit Text
  | IncrementOrderStatusCacheMiss Text

data MetricsResult a
  = ReadyUpResult a
  | ReadyDownResult
  | IncrementedClientAuthTokenGenerated
  | IncrementedOrderStatusCacheAdd
  | IncrementedOrderStatusCacheHit
  | IncrementedOrderStatusCacheMiss
  | MetricsError Text
