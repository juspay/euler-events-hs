module Euler.Events.Util.Prometheus where

import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Ratio           ((%))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Euler.Events.Util    (tshow)
import qualified Prometheus
import           System.Clock         (TimeSpec, diffTimeSpec, toNanoSecs)
import           System.Environment   (getEnvironment)
import           System.Posix.Process (getProcessID)

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
