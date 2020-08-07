module Euler.Events.MetricsLogger.Prometheus where

import           Control.Concurrent                (forkIO)
import           Data.Functor                      (($>))
import           Data.List                         (find)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Ratio                        ((%))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8)
import           Euler.Events.Class                (MetricsLogger (..))
import           Euler.Events.Types.Metrics        (MetricsOperation, MetricsResult)
import qualified Euler.Events.Types.Metrics        as M
import           Euler.Events.Util                 (tshow)
import qualified Network.HTTP.Types                as HTTP
import           Network.Wai                       (Middleware)
import qualified Network.Wai                       as Wai
import           Network.Wai.Handler.Warp          (run)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus                        as P
import qualified Prometheus.Metric.GHC             as P
import qualified Prometheus.Metric.Proc            as P
import           System.Clock                      (Clock (..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import           System.Environment                (getEnvironment)
import           System.Posix.Process              (getProcessID)

data PrometheusMetrics
  = Counter P.Counter
  | Gauge P.Gauge
  | PrometheusMiddleware

type Port = Int

newtype PrometheusConfig =
  PrometheusConfig Port

instance MetricsLogger PrometheusConfig PrometheusMetrics where
  initMetricsLogger :: PrometheusConfig -> IO ()
  initMetricsLogger (PrometheusConfig port) = do
    _ <- P.register P.procMetrics
    _ <- P.register P.ghcMetrics
    _ <- forkIO $ run port P.metricsApp
    pure ()
  metricsEvent ::
       MetricsOperation PrometheusMetrics
    -> IO (MetricsResult PrometheusMetrics)
  metricsEvent operation =
    case operation of
      M.ReadyUp -> do
        gauge <-
          P.register . P.gauge . P.Info "up" $
          "Describes whether this application is ready to receive connections."
        P.setGauge gauge 1
        pure (M.ReadyUpResult $ Gauge gauge)
      M.ReadyDown readyDown ->
        case readyDown of
          M.ReadyUpResult (Gauge gauge) ->
            P.setGauge gauge 0 $> M.ReadyDownResult
          _ -> pure $ M.MetricsError "Cannot set any metric other that Gauge"
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
  instrumentApp :: PrometheusMetrics -> (Text -> Text) -> Middleware
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

clientAuthTokenGeneratedCounter :: P.Vector Text P.Counter
clientAuthTokenGeneratedCounter =
  P.unsafeRegister $
  P.vector "merchant_id" $
  P.counter $ P.Info (withPrefix "client_auth_token_generated") ""

orderStatusCacheHitCounter :: P.Vector Text P.Counter
orderStatusCacheHitCounter =
  P.unsafeRegister $
  P.vector "merchant_id" $
  P.counter $ P.Info (withPrefix "order_status_cache_hit") ""

orderStatusCacheMissCounter :: P.Vector Text P.Counter
orderStatusCacheMissCounter =
  P.unsafeRegister $
  P.vector "merchant_id" $
  P.counter $ P.Info (withPrefix "order_status_cache_miss") ""

orderStatusCacheAddCounter :: P.Vector Text P.Counter
orderStatusCacheAddCounter =
  P.unsafeRegister $
  P.vector "merchant_id" $
  P.counter $ P.Info (withPrefix "order_status_cache_add") ""

incrementClientAuthTokenGenerated :: Text -> IO ()
incrementClientAuthTokenGenerated merchantId =
  P.withLabel clientAuthTokenGeneratedCounter merchantId P.incCounter

incrementOrderStatusCacheHit :: Text -> IO ()
incrementOrderStatusCacheHit merchantId =
  P.withLabel orderStatusCacheHitCounter merchantId P.incCounter

incrementOrderStatusCacheMiss :: Text -> IO ()
incrementOrderStatusCacheMiss merchantId =
  P.withLabel orderStatusCacheMissCounter merchantId P.incCounter

incrementOrderStatusCacheAdd :: Text -> IO ()
incrementOrderStatusCacheAdd merchantId =
  P.withLabel orderStatusCacheAddCounter merchantId P.incCounter

{-# NOINLINE requestLatency #-}
requestLatency :: P.Vector P.Label7 P.Histogram
requestLatency =
  P.unsafeRegister $
  P.vector
    ( "status_code"
    , "method"
    , "path"
    , "host"
    , "eulerInstance"
    , "pid"
    , "merchant_id") $
  P.histogram info buckets
  where
    info =
      P.Info
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
  P.withLabel
    requestLatency
    ( fromMaybe "" status
    , fromMaybe "" method
    , fromMaybe "" path
    , fromMaybe "" host
    , fromMaybe "" eulerInstance
    , fromMaybe "" pid
    , fromMaybe "" merchantId)
    (flip P.observe latency)
