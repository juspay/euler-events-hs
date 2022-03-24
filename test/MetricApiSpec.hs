{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedLabels #-}

module MetricApiSpec where

import Euler.Events.Network
import Euler.Events.MetricAPI

import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
-- import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, hspec, beforeAll)



spec :: Spec
spec = runIO $ bracket (async runMetricServer) cancel $ \_ -> hspec $
 beforeAll (register collection) $

  describe "Test the MetricApi." $ do
    it "Check metrics are empty" $ \_ -> do
      respBody <- getRespBody requestMetric
      traceTest respBody "isEmpty ---------"
      "g1 0.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc one vector metrics" $ \coll -> do
      inc (coll </> #c1) 42
      respBody <- getRespBody requestMetric
      traceTest respBody "c1 --------------"
      "c1{foo=\"42\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Add two vector metrics" $ \coll -> do
      add (coll </> #c2) 2 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c2 add --------------"
      "c2{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics" $ \coll -> do
      inc (coll </> #c3) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c3 inc --------------"
      "c3{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc gauge metrics" $ \coll -> do
      incGauge (coll </> #g1)
      respBody <- getRespBody requestMetric
      traceTest respBody "g1 --------------"
      "g1 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times" $ \coll -> do
      inc (coll </> #c4) 3 True
      inc (coll </> #c4) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c4 inc --------------"
      "c4{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times with different values" $ \coll -> do
      inc (coll </> #c5) 3 True
      inc (coll </> #c5) 3 False
      respBody <- getRespBody requestMetric
      traceTest respBody "c5 inc --------------"
      and [ "c5{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody
          , "c5{foo=\"3\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Reg two vector metrics 2 times" $ \coll -> do
      inc (coll </> #c6) 3 True
      coll2 <- register collection
      inc (coll </> #c6) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c6 inc --------------"
      BS.putStrLn respBody
      "c6{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "increment many metrics" $ \coll -> do
      inc (coll </> #c7) 7 False
      inc (coll </> #c8) 8 False
      inc (coll </> #c9) 9 False
      respBody <- getRespBody requestMetric
      traceTest respBody "c7 inc --------------"
      and [ "c7{foo=\"7\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c8{foo=\"8\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c9{foo=\"9\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Register metrics sequentually" $ \coll -> do
      inc (coll </> #c10) 10 False
      coll2 <- register collection2
      inc (coll2 </> #c11) 11 False
      inc (coll </> #c10) 10 False
      respBody <- getRespBody requestMetric
      traceTest respBody "c7 inc --------------"
      and [ "c10{foo=\"10\",bar=\"False\"} 2.0" `BS.isInfixOf` respBody
          , "c11{foo=\"11\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

{-
# HELP c11 c11
# TYPE c11 counter
c11{foo="11",bar="False"} 1.0
# HELP c10 c10
# TYPE c10 counter
c10{foo="10",bar="False"} 1.0
-}




c1 = counter #c1
  .& lbl @"foo" @Int
  .& build

c2 = counter #c2
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c3 = counter #c3
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

g1 = gauge #g1
      .& build

c4 = counter #c4
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c5 = counter #c5
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c6 = counter #c6
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c7 = counter #c7
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c8 = counter #c8
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c9 = counter #c9
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c10 = counter #c10
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

c11 = counter #c11
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

-- collection of metrics, prevents from ambiguos metric names
collection =
     c1
  .> c2
  .> c3
  .> g1
  .> c4
  .> c5
  .> c6
  .> c7
  .> c8
  .> c9
  .> c10
  .> MNil

collection2 =
      c11
  .> MNil

-- collection3 = collection <> collection2
