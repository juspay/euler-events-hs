module Euler.Events.MetricLogger.Prometheus where

import           Control.Concurrent                (forkIO)
import           Data.Functor                      (($>))
import           Data.List                         (find)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8)
import           Euler.Events.Class                (MetricLogger (..))
import           Euler.Events.Types.Metric         (MetricOperation, MetricResult)
import qualified Euler.Events.Types.Metric         as M
import           Euler.Events.Util.Prometheus      (incrementClientAuthTokenGenerated, incrementOrderStatusCacheAdd,
                                                    incrementOrderStatusCacheHit, incrementOrderStatusCacheMiss,
                                                    observeSeconds)
import qualified Network.HTTP.Types                as HTTP
import           Network.Wai                       (Middleware)
import qualified Network.Wai                       as Wai
import           Network.Wai.Handler.Warp          (run)
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus
import qualified Prometheus.Metric.GHC             as Prometheus
import qualified Prometheus.Metric.Proc            as Prometheus
import           System.Clock                      (Clock (..), getTime)

data PrometheusMetric
  = Counter Prometheus.Counter
  | Gauge Prometheus.Gauge
  | PrometheusMiddleware

type Port = Int

newtype PrometheusConfig =
  PrometheusConfig Port

instance MetricLogger PrometheusConfig PrometheusMetric where
  initMetricLogger :: PrometheusConfig -> IO ()
  initMetricLogger (PrometheusConfig port) = do
    _ <- Prometheus.register Prometheus.procMetrics
    _ <- Prometheus.register Prometheus.ghcMetrics
    _ <- forkIO $ run port Prometheus.metricsApp
    pure ()
  metricEvent ::
       MetricOperation PrometheusMetric -> IO (MetricResult PrometheusMetric)
  metricEvent operation =
    case operation of
      M.ReadyUp -> do
        gauge <-
          Prometheus.register . Prometheus.gauge . Prometheus.Info "up" $
          "Describes whether this application is ready to receive connections."
        Prometheus.setGauge gauge 1
        pure (M.ReadyUpResult $ Gauge gauge)
      M.ReadyDown readyDown ->
        case readyDown of
          M.ReadyUpResult (Gauge gauge) ->
            Prometheus.setGauge gauge 0 $> M.ReadyDownResult
          _ -> pure $ M.MetricError "Cannot set any metric other that Gauge"
      M.IncrementClientAuthTokenGenerated merchantId ->
        incrementClientAuthTokenGenerated merchantId $>
        M.IncrementedClientAuthTokenGenerated
      M.IncrementOrderStatusCacheAdd merchantId ->
        incrementOrderStatusCacheAdd merchantId $>
        M.IncrementedOrderStatusCacheAdd
      M.IncrementOrderStatusCacheHit merchantId ->
        incrementOrderStatusCacheHit merchantId $>
        M.IncrementedOrderStatusCacheHit
      M.IncrementOrderStatusCacheMiss merchantId ->
        incrementOrderStatusCacheMiss merchantId $>
        M.IncrementedOrderStatusCacheMiss
  instrumentApp :: PrometheusMetric -> (Text -> Text) -> Middleware
  instrumentApp _ normalizer app req respond = do
    start <- getTime Monotonic
    app req $ \res -> do
      end <- getTime Monotonic
      let method = Just $ decodeUtf8 (Wai.requestMethod req)
      let status =
            Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res)))
      let path =
            normalizer <$>
            (Just $
             T.intercalate "/" $
             "" : filter (\e -> T.length e /= 0) (Wai.pathInfo req))
      -- TODO factor out headers from Server.hs get rid of the literal
      let merchantId =
            decodeUtf8 <$>
            (fmap snd <$> find (\(h, _) -> h == "x-jp-merchant-id") $
             Wai.responseHeaders res)
      observeSeconds status method path merchantId start end
      respond res
