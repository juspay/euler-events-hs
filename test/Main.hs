module Main where

import qualified EventSpec
import qualified PrometheusSpec
import qualified MetricApiSpec
import Test.Hspec (describe, hspec)


main :: IO ()
main = hspec $ do
  describe "Event" EventSpec.spec
  describe "MetricApi" MetricApiSpec.spec
  describe "Prometheus" PrometheusSpec.spec
