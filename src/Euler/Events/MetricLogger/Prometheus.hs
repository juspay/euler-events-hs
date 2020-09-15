module Euler.Events.MetricLogger.Prometheus where

import Control.Concurrent (forkIO)
import Data.Functor (($>))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Euler.Events.Class (ErrorText, MetricLogger (..))
import Euler.Events.Types.Metric (MetricOperation, MetricResult)
import qualified Euler.Events.Types.Metric as M
import Euler.Events.Util.Prometheus
  ( PrometheusMetric (..),
    increment,
    incrementVector1Counter,
    observeSeconds,
    registerVector1Counter,
    set,
  )
import GHC.Conc (atomically)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus
import qualified Prometheus.Metric.GHC as Prometheus
import qualified Prometheus.Metric.Proc as Prometheus
import qualified StmContainers.Map as StmMap
import System.Clock (Clock (..), getTime)

type Port = Int

data PrometheusConfig
  = PrometheusConfig Port Text

data PrometheusLogger = PrometheusLogger
  { prefix :: Text,
    metricMap :: StmMap.Map Text PrometheusMetric
  }

instance Eq PrometheusLogger where
  (==) :: PrometheusLogger -> PrometheusLogger -> Bool
  x == y = prefix x == prefix y

instance Show PrometheusLogger where
  show :: PrometheusLogger -> String
  show x = "PrometheusLogger { prefix = " ++ (show . prefix $ x) ++ ", metricMap = <StmContainers.Map Text PrometheusMetric> }"

instance MetricLogger PrometheusConfig PrometheusLogger PrometheusMetric where
  initMetricLogger :: PrometheusConfig -> IO PrometheusLogger
  initMetricLogger (PrometheusConfig port prefix) = do
    _ <- Prometheus.register Prometheus.procMetrics
    _ <- Prometheus.register Prometheus.ghcMetrics
    _ <- forkIO $ run port Prometheus.metricsApp
    metricMap <- atomically StmMap.new
    pure $ PrometheusLogger {prefix = prefix, metricMap = metricMap}
  metricEvent ::
    PrometheusLogger ->
    MetricOperation PrometheusMetric ->
    IO (Either ErrorText (MetricResult PrometheusMetric))
  metricEvent PrometheusLogger {..} operation =
    case operation of
      M.ReadyUp -> do
        gauge <-
          Prometheus.register . Prometheus.gauge . Prometheus.Info "up" $
            "Describes whether this application is ready to receive connections."
        Prometheus.setGauge gauge 1
        pure (Right . M.ReadyUpResult $ Gauge gauge)
      M.ReadyDown readyDown ->
        case readyDown of
          M.ReadyUpResult (Gauge gauge) ->
            Prometheus.setGauge gauge 0 $> Right M.ReadyDownResult
          _ -> pure $ Left "Cannot set any metric other that Gauge"
      M.Increment counterName -> increment prefix counterName metricMap
      M.Set gaugeName value -> set prefix gaugeName value metricMap
      M.RegisterVector1Counter vectorName labelName ->
        registerVector1Counter prefix vectorName labelName metricMap
      M.IncrementVector1Counter vectorName labelValue ->
        incrementVector1Counter prefix vectorName labelValue metricMap
  instrumentApp :: PrometheusMetric -> (Text -> Text) -> Middleware
  instrumentApp _ normalizer app req respond = do
    start <- getTime Monotonic
    app req $ \res -> do
      end <- getTime Monotonic
      let method = Just $ decodeUtf8 (Wai.requestMethod req)
      let status =
            Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res)))
      let path =
            normalizer
              <$> ( Just $
                      T.intercalate "/" $
                        "" : filter (\e -> T.length e /= 0) (Wai.pathInfo req)
                  )
      -- TODO factor out headers from Server.hs get rid of the literal
      let merchantId =
            decodeUtf8
              <$> ( fmap snd <$> find (\(h, _) -> h == "x-jp-merchant-id") $
                      Wai.responseHeaders res
                  )
      observeSeconds status method path merchantId start end
      respond res
