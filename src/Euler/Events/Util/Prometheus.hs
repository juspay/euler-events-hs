{-# LANGUAGE OverloadedLabels #-}

module Euler.Events.Util.Prometheus where

import Control.DeepSeq (force)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Euler.Events.Class (ErrorText)
import Euler.Events.Types.Metric (MetricResult)
import qualified Euler.Events.Types.Metric as M
import Euler.Events.Util (tshow)
import GHC.Conc (atomically)
import qualified Prometheus
import qualified StmContainers.Map as StmMap
import System.Clock (TimeSpec, diffTimeSpec, toNanoSecs)
import System.Environment (getEnvironment)
import System.Posix.Process (getProcessID)

data PrometheusMetric
  = Counter Prometheus.Counter
  | Gauge Prometheus.Gauge
  | Vector1Counter (Prometheus.Vector Prometheus.Label1 Prometheus.Counter)
  | PrometheusMiddleware

instance Eq PrometheusMetric where
  (==) :: PrometheusMetric -> PrometheusMetric -> Bool
  Counter _ == Counter _ = True
  Gauge _ == Gauge _ = True
  Vector1Counter _ == Vector1Counter _ = True
  PrometheusMiddleware == PrometheusMiddleware = True
  _ == _ = False

instance Show PrometheusMetric where
  show :: PrometheusMetric -> String
  show (Counter _)          = "Counter"
  show (Gauge _)            = "Gauge"
  show (Vector1Counter _)   = "Vector1Counter"
  show PrometheusMiddleware = "PrometheusMiddleware"

withPrefix :: Text -> Text -> Text
withPrefix prefix name = prefix <> name

{-# NOINLINE requestLatency #-}
requestLatency :: Prometheus.Vector Prometheus.Label7 Prometheus.Histogram
requestLatency =
  Prometheus.unsafeRegister $
    Prometheus.vector
      ( "status_code",
        "method",
        "path",
        "host",
        "eulerInstance",
        "pid",
        "merchant_id"
      )
      $ Prometheus.histogram info buckets
  where
    info =
      Prometheus.Info
        "euler_http_request_duration"
        "duration histogram of http responses labeled with: status_code, method, path, host, eulerInstance, pid, merchant_id"
    buckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20, 30]

observeSeconds ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  TimeSpec ->
  TimeSpec ->
  IO ()
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
    ( force
      ( fromMaybe "" status,
        fromMaybe "" method,
        fromMaybe "" path,
        fromMaybe "" host,
        fromMaybe "" eulerInstance,
        fromMaybe "" pid,
        fromMaybe "" merchantId
      )
    )
    (flip Prometheus.observe latency)

increment ::
  Text ->
  Text ->
  StmMap.Map Text PrometheusMetric ->
  IO (Either ErrorText (MetricResult PrometheusMetric))
increment prefix counterName metricMap = do
  let fullName = withPrefix prefix counterName
  mCounter <- atomically . StmMap.lookup fullName $ metricMap
  case mCounter of
    Nothing -> do
      counter <-
        Prometheus.register . Prometheus.counter $ Prometheus.Info fullName ""
      atomically $ StmMap.insert (Counter counter) fullName metricMap
      Prometheus.incCounter counter $> Right M.Incremented
    Just (Counter counter) ->
      Prometheus.incCounter counter $> Right M.Incremented
    _ -> pure . Left . alreadyRegistered $ "non-counter"

set ::
  Text ->
  Text ->
  Double ->
  StmMap.Map Text PrometheusMetric ->
  IO (Either ErrorText (MetricResult PrometheusMetric))
set prefix gaugeName val metricMap = do
  let fullName = withPrefix prefix gaugeName
  mGauge <- atomically . StmMap.lookup fullName $ metricMap
  case mGauge of
    Nothing -> do
      gauge <-
        Prometheus.register . Prometheus.gauge $ Prometheus.Info fullName ""
      atomically $ StmMap.insert (Gauge gauge) fullName metricMap
      Prometheus.setGauge gauge val $> Right M.Setted
    Just (Gauge gauge) -> Prometheus.setGauge gauge val $> Right M.Setted
    _ -> pure . Left . alreadyRegistered $ "non-gauge"

registerVector1Counter ::
  Text ->
  Text ->
  Text ->
  StmMap.Map Text PrometheusMetric ->
  IO (Either ErrorText (MetricResult PrometheusMetric))
registerVector1Counter prefix vectorName labelName metricMap = do
  let fullName = withPrefix prefix vectorName
  mVector <- atomically . StmMap.lookup fullName $ metricMap
  case mVector of
    Nothing -> do
      vector <-
        Prometheus.register $
          Prometheus.vector labelName $
            Prometheus.counter $ Prometheus.Info (withPrefix prefix vectorName) ""
      atomically $
        StmMap.insert (Vector1Counter vector) fullName metricMap
          $> Right M.RegisteredVector1Counter
    Just (Vector1Counter _) ->
      pure . Left . alreadyRegistered $ "vector1Counter"
    _ -> pure . Left . alreadyRegistered $ "non-vector1Counter"

incrementVector1Counter ::
  Text ->
  Text ->
  Text ->
  StmMap.Map Text PrometheusMetric ->
  IO (Either ErrorText (MetricResult PrometheusMetric))
incrementVector1Counter prefix vectorName labelValue metricMap = do
  let fullName = withPrefix prefix vectorName
  mVector <- atomically . StmMap.lookup fullName $ metricMap
  case mVector of
    Just (Vector1Counter vector) ->
      Prometheus.withLabel vector labelValue Prometheus.incCounter
        $> Right M.IncrementedVector1Counter
    Nothing -> pure $ Left notRegistered
    _ -> pure . Left . alreadyRegistered $ "non-vector1Counter"

alreadyRegistered :: Text -> Text
alreadyRegistered metricType =
  "A " <> metricType <> " metric is already registered with this name"

notRegistered :: Text
notRegistered = "No metric is registered with this name"
