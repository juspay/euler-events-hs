module Euler.Events.MetricsLogger.Prometheus
  ( PrometheusConfig(..)
  , PrometheusLogger
  ) where

import           Control.Concurrent                  (forkIO)
import           Data.Functor                        (($>))
import           Euler.Events.Class                  (MetricsLogger (..))
import           Euler.Events.Types.PrometheusMetric (PrometheusOperation, PrometheusResult)
import qualified Euler.Events.Types.PrometheusMetric as PM
import           Network.Wai.Handler.Warp            (run)
import qualified Network.Wai.Middleware.Prometheus   as P
import qualified Prometheus                          as P
import qualified Prometheus.Metric.GHC               as P
import qualified Prometheus.Metric.Proc              as P

type Port = Int

newtype PrometheusConfig =
  PrometheusConfig Port

newtype PrometheusLogger =
  PrometheusLogger ()

instance MetricsLogger PrometheusConfig PrometheusLogger PrometheusOperation PrometheusResult where
  initMetricsLogger (PrometheusConfig port) = do
    _ <- P.register P.procMetrics
    _ <- P.register P.ghcMetrics
    _ <- forkIO $ run port P.metricsApp
    pure . Right $ PrometheusLogger ()
  metricsEvent _ operation =
    case operation of
      PM.Register wrappedMetric ->
        case wrappedMetric of
          PM.WrappedCounter c -> PM.Registered . PM.Counter <$> P.register c
          PM.WrappedGauge g   -> PM.Registered . PM.Gauge <$> P.register g
      PM.MakeCounter name help ->
        pure . PM.Made . PM.WrappedCounter . P.counter . P.Info name $ help
      PM.MakeGauge name help ->
        pure . PM.Made . PM.WrappedGauge . P.gauge . P.Info name $ help
      PM.Increment (PM.Counter c) -> P.incCounter c $> PM.Incremented
      PM.Increment (PM.Gauge g) -> P.incGauge g $> PM.Incremented
      PM.Set (PM.Counter _) _ ->
        pure $ PM.PrometheusError "Cannot set a counter"
      PM.Set (PM.Gauge g) val -> P.setGauge g val $> PM.Setted
