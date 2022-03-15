{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module MetricApiSpec where

import Common
import Euler.Events.MetricAPI


import Control.Concurrent.Async (async, cancel)
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, hspec)




spec :: Spec
spec = runIO $ bracket (async runMetricServer) cancel $ \_ -> hspec $

  describe "Test a MetricApi." $ do
    -- it "Check metrics are empty" $ do
    --   eResp <- getRespBody requestMetric
    --   -- print eResp
    --   BS.null eResp `shouldBe` True
    it "Inc one vector metrics" $ do
      c1 <- reg coll c1def
      inc c1 42
      respBody <- getRespBody requestMetric
      putStrLn "c1 --------------"
      BS.putStrLn respBody
      "c1{foo=\"42\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Add two vector metrics" $ do
      c2 <- reg coll c2def
      add c2 2 True 3
      respBody <- getRespBody requestMetric
      putStrLn "c2 --------------"
      BS.putStrLn respBody
      "c2{bar=\"True\",foo=\"3\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Inc gauge metrics" $ do
      g1 <- reg coll g1def
      incG g1
      respBody <- getRespBody requestMetric
      putStrLn "g1 --------------"
      BS.putStrLn respBody
      "g1 1.0" `BS.isInfixOf` respBody `shouldBe` True



-- creates a counter @c1@ with one label @foo@ of type 'Int'
c1def = counter @"c1" .& lbl @"foo" @Int
-- another metric
g1def = gauge @"g1"

c2def = counter @"c2"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

-- collection of metrics, prevents from ambiguos metric names
coll = g1def :+: c1def :+: c2def :+: MNil
