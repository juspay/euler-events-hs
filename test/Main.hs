module Main where

import qualified EventSpec
import qualified PrometheusSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Event" EventSpec.spec
  describe "Prometheus" PrometheusSpec.spec
