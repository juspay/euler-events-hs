module Euler.Events.Types.PrometheusMetric where

import           Data.Text  (Text)
import qualified Prometheus as P

data PrometheusMetric
  = Counter P.Counter
  | Gauge P.Gauge

data WrappedPrometheusMetric
  = WrappedCounter (P.Metric P.Counter)
  | WrappedGauge (P.Metric P.Gauge)

-- how can we tie PrometheusOperation values to PrometheusResult
data PrometheusOperation
  = Register WrappedPrometheusMetric
  | MakeCounter Text Text
  | MakeGauge Text Text
  | Increment PrometheusMetric
  | Set PrometheusMetric Double

data PrometheusResult
  = Registered PrometheusMetric
  | Made WrappedPrometheusMetric
  | Incremented
  | Setted
  | PrometheusError Text
