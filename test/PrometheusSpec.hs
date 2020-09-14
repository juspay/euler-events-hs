{-# LANGUAGE ScopedTypeVariables #-}

module PrometheusSpec where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Either.Combinators (fromRight')
import Euler.Events.Class (emitMetricIO, initMetricLogger)
import Euler.Events.MetricLogger.Prometheus (PrometheusConfig (PrometheusConfig))
import Euler.Events.Types.Metric (MetricOperation (Increment, IncrementVector1Counter, RegisterVector1Counter))
import Network.HTTP.Simple
  ( Request,
    Response,
    defaultRequest,
    getResponseBody,
    httpBS,
    setRequestPort,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let port = 9999
  let req = setRequestPort port defaultRequest
  -- TODO: Cannot see these here but can see them if I initMetricLogger in ghci
  let _ghcMetrics =
        [ "ghc_gcs_total",
          "ghc_major_gcs_total",
          "ghc_allocated_bytes_total",
          "ghc_max_live_bytes",
          "ghc_max_large_objects_bytes",
          "ghc_max_compact_bytes",
          "ghc_max_slop_bytes",
          "ghc_max_mem_in_use_bytes",
          "ghc_cumulative_live_bytes_total",
          "ghc_copied_bytes_total",
          "ghc_par_copied_bytes_total",
          "ghc_cumulative_par_max_copied_bytes_total",
          "ghc_mutator_cpu_seconds_total",
          "ghc_mutator_elapsed_seconds_total",
          "ghc_gc_cpu_seconds_total",
          "ghc_gc_elapsed_seconds_total",
          "ghc_cpu_seconds_total",
          "ghc_elapsed_seconds_total",
          "ghc_gcdetails_gen",
          "ghc_gcdetails_threads",
          "ghc_gcdetails_allocated_bytes",
          "ghc_gcdetails_live_bytes",
          "ghc_gcdetails_large_objects_bytes",
          "ghc_gcdetails_compact_bytes",
          "ghc_gcdetails_slop_bytes",
          "ghc_gcdetails_mem_in_use_bytes",
          "ghc_gcdetails_copied_bytes",
          "ghc_gcdetails_par_max_copied_bytes",
          "ghc_gcdetails_sync_elapsed_seconds",
          "ghc_gcdetails_cpu_seconds",
          "ghc_gcdetails_elapsed_seconds"
        ]
  let procMetrics =
        [ "process_open_fds",
          "process_max_fds",
          "process_heap_size_bytes",
          "process_cpu_user_seconds_total",
          "process_cpu_system_seconds_total",
          "process_cpu_seconds_total",
          "process_start_time_seconds",
          "process_virtual_memory_bytes",
          "process_resident_memory_bytes"
        ]
  let _ghcProcMetrics = _ghcMetrics ++ procMetrics
  describe "Test a simple Prometheus metrics flow" $
    it "mimics a simple Prometheus metrics flow" $ do
      eResp <- makeReq req
      isLeft eResp `shouldBe` True
      logger <- initMetricLogger (PrometheusConfig port "prefix")
      let emitMetric = emitMetricIO logger
      emitMetric (Increment "counter")
      respBody <- getRespBody req
      checkInfixes procMetrics respBody `shouldBe` True
      "prefix_counter 1.0" `BS.isInfixOf` respBody `shouldBe` True
      emitMetric (IncrementVector1Counter "vector" "a")
      respBody2 <- getRespBody req
      "prefix_counter 1.0" `BS.isInfixOf` respBody2 `shouldBe` True
      "prefix_vector{merchant_id=\"a\"}" `BS.isInfixOf` respBody2
        `shouldBe` False
      emitMetric (RegisterVector1Counter "vector" "merchant_id")
      emitMetric (IncrementVector1Counter "vector" "a")
      respBody3 <- getRespBody req
      "prefix_vector{merchant_id=\"a\"} 1.0" `BS.isInfixOf` respBody3
        `shouldBe` True
      emitMetric (Increment "counter")
      emitMetric (Increment "counter")
      emitMetric (Increment "counter")
      emitMetric (Increment "counter2")
      emitMetric (Increment "counter2")
      emitMetric (Increment "counter2")
      emitMetric (IncrementVector1Counter "vector" "a")
      emitMetric (IncrementVector1Counter "vector" "b")
      emitMetric (IncrementVector1Counter "vector" "a")
      emitMetric (IncrementVector1Counter "vector" "c")
      emitMetric (IncrementVector1Counter "vector" "b")
      emitMetric (RegisterVector1Counter "vector2" "merchant_id")
      emitMetric (IncrementVector1Counter "vector2" "a")
      emitMetric (IncrementVector1Counter "vector2" "b")
      emitMetric (IncrementVector1Counter "vector2" "b")
      emitMetric (IncrementVector1Counter "vector2" "a")
      emitMetric (IncrementVector1Counter "vector2" "b")
      respBody4 <- getRespBody req
      "prefix_counter 4.0" `BS.isInfixOf` respBody4 `shouldBe` True
      "prefix_counter2 3.0" `BS.isInfixOf` respBody4 `shouldBe` True
      "prefix_vector{merchant_id=\"a\"} 3.0" `BS.isInfixOf` respBody4
        `shouldBe` True
      "prefix_vector{merchant_id=\"b\"} 2.0" `BS.isInfixOf` respBody4
        `shouldBe` True
      "prefix_vector{merchant_id=\"c\"} 1.0" `BS.isInfixOf` respBody4
        `shouldBe` True
      "prefix_vector2{merchant_id=\"a\"} 2.0" `BS.isInfixOf` respBody4
        `shouldBe` True
      "prefix_vector2{merchant_id=\"b\"} 3.0" `BS.isInfixOf` respBody4
        `shouldBe` True

makeReq :: Request -> IO (Either SomeException (Response BS.ByteString))
makeReq req = try $ httpBS req

checkInfixes :: [BS.ByteString] -> BS.ByteString -> Bool
checkInfixes subStrs str = and ((`BS.isInfixOf` str) <$> subStrs)

getRespBody :: Request -> IO BS.ByteString
getRespBody req = getResponseBody . fromRight' <$> makeReq req
