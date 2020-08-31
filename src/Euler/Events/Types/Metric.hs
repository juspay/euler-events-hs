module Euler.Events.Types.Metric where

import Data.Text (Text)

data MetricOperation a
  = ReadyUp
  | ReadyDown (MetricResult a)
  | Increment Text -- Increment <counterName>
  | Set Text Double -- Set <gaugeName> <value>
  | RegisterVector1Counter Text Text -- RegisterVector1Counter <vectorName> <labelName>
  | IncrementVector1Counter Text Text -- IncrementVector1Counter <vectorName> <labelValue>

data MetricResult a
  = ReadyUpResult a
  | ReadyDownResult
  | Incremented
  | Setted
  | RegisteredVector1Counter
  | IncrementedVector1Counter
  | MetricResultError
