module Euler.Events.MetricLogger.Prometheus where

import           Control.Concurrent                (forkIO)
import           Data.Functor                      (($>))
import           Data.List                         (find)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Ratio                        ((%))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8)
import           Euler.Events.Class                (MetricLogger (..))
import           Euler.Events.Types.Metric         (MetricOperation, MetricResult)
import qualified Euler.Events.Types.Metric         as M
import           Euler.Events.Util                 (tshow)
import qualified Network.HTTP.Types                as HTTP
import           Network.Wai                       (Middleware)
import qualified Network.Wai                       as Wai
import           Network.Wai.Handler.Warp          (run)
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus
import qualified Prometheus.Metric.GHC             as Prometheus
import qualified Prometheus.Metric.Proc            as Prometheus
import           System.Clock                      (Clock (..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import           System.Environment                (getEnvironment)
import           System.Posix.Process              (getProcessID)

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

withPrefix :: Text -> Text
withPrefix = ("euler_" <>)

clientAuthTokenGeneratedCounter :: Prometheus.Vector Text Prometheus.Counter
clientAuthTokenGeneratedCounter =
  Prometheus.unsafeRegister $
  Prometheus.vector "merchant_id" $
  Prometheus.counter $
  Prometheus.Info (withPrefix "client_auth_token_generated") ""

orderStatusCacheHitCounter :: Prometheus.Vector Text Prometheus.Counter
orderStatusCacheHitCounter =
  Prometheus.unsafeRegister $
  Prometheus.vector "merchant_id" $
  Prometheus.counter $ Prometheus.Info (withPrefix "order_status_cache_hit") ""

orderStatusCacheMissCounter :: Prometheus.Vector Text Prometheus.Counter
orderStatusCacheMissCounter =
  Prometheus.unsafeRegister $
  Prometheus.vector "merchant_id" $
  Prometheus.counter $ Prometheus.Info (withPrefix "order_status_cache_miss") ""

orderStatusCacheAddCounter :: Prometheus.Vector Text Prometheus.Counter
orderStatusCacheAddCounter =
  Prometheus.unsafeRegister $
  Prometheus.vector "merchant_id" $
  Prometheus.counter $ Prometheus.Info (withPrefix "order_status_cache_add") ""

incrementClientAuthTokenGenerated :: Text -> IO ()
incrementClientAuthTokenGenerated merchantId =
  Prometheus.withLabel
    clientAuthTokenGeneratedCounter
    merchantId
    Prometheus.incCounter

incrementOrderStatusCacheHit :: Text -> IO ()
incrementOrderStatusCacheHit merchantId =
  Prometheus.withLabel
    orderStatusCacheHitCounter
    merchantId
    Prometheus.incCounter

incrementOrderStatusCacheMiss :: Text -> IO ()
incrementOrderStatusCacheMiss merchantId =
  Prometheus.withLabel
    orderStatusCacheMissCounter
    merchantId
    Prometheus.incCounter

incrementOrderStatusCacheAdd :: Text -> IO ()
incrementOrderStatusCacheAdd merchantId =
  Prometheus.withLabel
    orderStatusCacheAddCounter
    merchantId
    Prometheus.incCounter

{-# NOINLINE requestLatency #-}
requestLatency :: Prometheus.Vector Prometheus.Label7 Prometheus.Histogram
requestLatency =
  Prometheus.unsafeRegister $
  Prometheus.vector
    ( "status_code"
    , "method"
    , "path"
    , "host"
    , "eulerInstance"
    , "pid"
    , "merchant_id") $
  Prometheus.histogram info buckets
  where
    info =
      Prometheus.Info
        "euler_http_request_duration"
        "duration histogram of http responses labeled with: status_code, method, path, host, eulerInstance, pid, merchant_id"
    buckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20, 30]

observeSeconds ::
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> TimeSpec
  -> TimeSpec
  -> IO ()
observeSeconds status method path merchantId start end = do
  let latency =
        fromRational $
        toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
  envVars <- Map.fromList <$> getEnvironment
  let eulerInstance = T.pack <$> Map.lookup "HOSTNAME" envVars
  let host = Nothing
  processId <- getProcessID
  let pid = tshow <$> Just processId
  Prometheus.withLabel
    requestLatency
    ( fromMaybe "" status
    , fromMaybe "" method
    , fromMaybe "" path
    , fromMaybe "" host
    , fromMaybe "" eulerInstance
    , fromMaybe "" pid
    , fromMaybe "" merchantId)
    (flip Prometheus.observe latency)
