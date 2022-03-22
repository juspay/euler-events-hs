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
-- import qualified Prometheus as P




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
      putStrLn "c2 add --------------"
      BS.putStrLn respBody
      "c2{bar=\"True\",foo=\"3\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Inc two vector metrics" $ do
      c3 <- reg coll c3def
      inc c3 True 3
      respBody <- getRespBody requestMetric
      putStrLn "c3 inc --------------"
      BS.putStrLn respBody
      "c3{barn=\"True\",foom=\"3\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Inc gauge metrics" $ do
      g1 <- reg coll g1def
      incG g1
      respBody <- getRespBody requestMetric
      putStrLn "g1 --------------"
      BS.putStrLn respBody
      "g1 1.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Inc two vector metrics 2 times" $ do
      c4 <- reg coll c4def
      inc c4 True 3
      inc c4 True 3
      respBody <- getRespBody requestMetric
      putStrLn "c4 inc --------------"
      BS.putStrLn respBody
      "c4{barn=\"True\",foom=\"3\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True
    it "Inc two vector metrics 2 times with different values" $ do
      c5 <- reg coll c5def
      inc c5 True 3
      inc c5 False 3
      respBody <- getRespBody requestMetric
      putStrLn "c5 inc --------------"
      BS.putStrLn respBody
      ("c5{barn=\"True\",foom=\"3\"} 1.0" `BS.isInfixOf` respBody)
        && ("c5{barn=\"False\",foom=\"3\"} 1.0" `BS.isInfixOf` respBody) `shouldBe` True
    it "Reg two vector metrics 2 times" $ do
      c6 <- reg coll c6def
      inc c6 True 3
      c6 <- reg coll c6def
      inc c6 True 3
      respBody <- getRespBody requestMetric
      putStrLn "c6 inc --------------"
      BS.putStrLn respBody
      "c6{bar=\"True\",foo=\"3\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True
{-
c6 inc --------------
# HELP c6 c6
# TYPE c6 counter
c6{bar="True",foo="3"} 1.0
# HELP c6 c6
# TYPE c6 counter
c6{bar="True",foo="3"} 1.0
-}
    it "Map reg of metrics and increment them" $ do
      c7 <- reg coll c7def
      c8 <- reg coll c8def
      c9 <- reg coll c9def
      -- mapM (reg col) [c7def,]
      inc c7 True 3
      -- inc c4 True 3
      respBody <- getRespBody requestMetric
      putStrLn "c7 inc --------------"
      BS.putStrLn respBody
      "c7{bar=\"True\",foo=\"3\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True


-- creates a counter @c1@ with one label @foo@ of type 'Int'
c1def = counter @"c1" .& lbl @"foo" @Int
-- another metric
g1def = gauge @"g1"

c2def = counter @"c2"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

c3def = counter @"c3"
      .& lbl @"foom" @Int
      .& lbl @"barn" @Bool

c4def = counter @"c4"
      .& lbl @"foom" @Int
      .& lbl @"barn" @Bool

c5def = counter @"c5"
      .& lbl @"foom" @Int
      .& lbl @"barn" @Bool

c6def = counter @"c6"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

c7def = counter @"c7"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

c8def = counter @"c8"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

c9def = counter @"c9"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

-- collection of metrics, prevents from ambiguos metric names
coll =
      g1def
  :+: c1def
  :+: c2def
  :+: c3def
  :+: c4def
  :+: c5def
  :+: c6def
  :+: c7def
  :+: c8def
  :+: c9def
  :+: MNil
