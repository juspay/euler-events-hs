{-# LANGUAGE ScopedTypeVariables #-}

module PrometheusSpec where

import Common
import Euler.Events.Class (emitMetricIO, initMetricLogger)
import Euler.Events.MetricLogger.Prometheus (PrometheusConfig (PrometheusConfig))
import Euler.Events.Types.Metric (MetricOperation (Increment, IncrementVector1Counter, RegisterVector1Counter))

import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldBe, beforeAll)

spec :: Spec
spec = runIO $ bracket (async runMetricServer) cancel $ \_ -> hspec $
  beforeAll (emitMetricIO <$> initMetricLogger (PrometheusConfig port "prefix_")) $ describe "Puneet's Prometheus metrics flow." $ do
    -- it "Check metrics are empty." $ \_ -> do
    --   eResp <- makeReq requestMetric
    --   print eResp
    --   isLeft eResp `shouldBe` True

    it "Increment counter" $ \emitMetric -> do
      emitMetric (Increment "counter")
      respBody <- getRespBody requestMetric
      checkInfixes procMetrics respBody `shouldBe` True
      "prefix_counter 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "mimics a simple Prometheus metrics flow" $ \emitMetric -> do
      emitMetric (IncrementVector1Counter "vector" "a")
      respBody2 <- getRespBody requestMetric
      -- "prefix_counter 1.0" `BS.isInfixOf` respBody2 `shouldBe` True
      -- "prefix_vector{merchant_id=\"a\"}" `BS.isInfixOf` respBody2
        -- `shouldBe` False
      emitMetric (RegisterVector1Counter "vector" "merchant_id")
      emitMetric (IncrementVector1Counter "vector" "a")
      respBody3 <- getRespBody requestMetric
      -- "prefix_vector{merchant_id=\"a\"} 1.0" `BS.isInfixOf` respBody3
        -- `shouldBe` True
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
      respBody4 <- getRespBody requestMetric
      -- "prefix_counter 4.0" `BS.isInfixOf` respBody4 `shouldBe` True
      -- "prefix_counter2 3.0" `BS.isInfixOf` respBody4 `shouldBe` True
      -- "prefix_vector{merchant_id=\"a\"} 3.0" `BS.isInfixOf` respBody4
      --   `shouldBe` True
      -- "prefix_vector{merchant_id=\"b\"} 2.0" `BS.isInfixOf` respBody4
      --   `shouldBe` True
      -- "prefix_vector{merchant_id=\"c\"} 1.0" `BS.isInfixOf` respBody4
      --   `shouldBe` True
      -- "prefix_vector2{merchant_id=\"a\"} 2.0" `BS.isInfixOf` respBody4
      --   `shouldBe` True
      -- "prefix_vector2{merchant_id=\"b\"} 3.0" `BS.isInfixOf` respBody4
        -- `shouldBe` True
      pure ()

