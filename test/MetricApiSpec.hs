{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module MetricApiSpec where

import Common
import Euler.Events.MetricAPI


import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
-- import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, hspec, beforeAll)



spec :: Spec
spec = runIO $ bracket (async runMetricServer) cancel $ \_ -> hspec $
 beforeAll (register collection) $

  describe "Test a MetricApi." $ do
    -- it "Check metrics are empty" $ \_ -> do
    --   respBody <- getRespBody requestMetric
    --   traceTest respBody "isEmpty ---------"
    --   BS.null respBody `shouldBe` True

    it "Inc one vector metrics" $ \coll -> do
      inc (useMetric @C1 coll) 42
      respBody <- getRespBody requestMetric
      traceTest respBody "c1 --------------"
      "c1{foo=\"42\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Add two vector metrics" $ \coll -> do
      add (useMetric @C2 coll) 2 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c2 add --------------"
      "c2{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics" $ \coll -> do
      inc (useMetric @C3 coll) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c3 inc --------------"
      "c3{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc gauge metrics" $ \coll -> do
      incGauge (useMetric @G1 coll)
      respBody <- getRespBody requestMetric
      traceTest respBody "g1 --------------"
      "g1 1.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times" $ \coll -> do
      inc (useMetric @C4 coll) 3 True
      inc (useMetric @C4 coll) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c4 inc --------------"
      "c4{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "Inc two vector metrics 2 times with different values" $ \coll -> do
      inc (useMetric @C5 coll) 3 True
      inc (useMetric @C5 coll) 3 False
      respBody <- getRespBody requestMetric
      traceTest respBody "c5 inc --------------"
      and [ "c5{foo=\"3\",bar=\"True\"} 1.0" `BS.isInfixOf` respBody
          , "c5{foo=\"3\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True

    it "Reg two vector metrics 2 times" $ \coll -> do
      inc (useMetric @C6 coll) 3 True
      coll2 <- register collection
      inc (useMetric @C6 coll2) 3 True
      respBody <- getRespBody requestMetric
      traceTest respBody "c6 inc --------------"
      BS.putStrLn respBody
      "c6{foo=\"3\",bar=\"True\"} 2.0" `BS.isInfixOf` respBody `shouldBe` True

    it "increment many metrics" $ \coll -> do
      inc (useMetric @C7 coll) 7 False
      inc (useMetric @C8 coll) 8 False
      inc (useMetric @C9 coll) 9 False
      respBody <- getRespBody requestMetric
      traceTest respBody "c7 inc --------------"
      and [ "c7{foo=\"7\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c8{foo=\"8\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          , "c9{foo=\"9\",bar=\"False\"} 1.0" `BS.isInfixOf` respBody
          ] `shouldBe` True


type C1 = PromRep 'Counter "c1" '[ '("foo", Int)]
c1 :: C1
c1 = counter @"c1"
  .& lbl @"foo" @Int
  .& build


type C2 = PromRep 'Counter "c2" '[ '("foo", Int), '("bar", Bool)]
c2 :: C2
c2 = counter @"c2"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C3 = PromRep 'Counter "c3" '[ '("foo", Int), '("bar", Bool)]
c3 :: C3
c3 = counter @"c3"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type G1 = PromRep 'Gauge "g1" '[]
g1 :: G1
g1 = gauge @"g1"
      .& build

type C4 = PromRep 'Counter "c4" '[ '("foo", Int), '("bar", Bool)]
c4 :: C4
c4 = counter @"c4"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C5 = PromRep 'Counter "c5" '[ '("foo", Int), '("bar", Bool)]
c5 :: C5
c5 = counter @"c5"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C6 = PromRep 'Counter "c6" '[ '("foo", Int), '("bar", Bool)]
c6 :: C6
c6 = counter @"c6"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C7 = PromRep 'Counter "c7" '[ '("foo", Int), '("bar", Bool)]
c7 :: C7
c7 = counter @"c7"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C8 = PromRep 'Counter "c8" '[ '("foo", Int), '("bar", Bool)]
c8 :: C8
c8 = counter @"c8"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

type C9 = PromRep 'Counter "c9" '[ '("foo", Int), '("bar", Bool)]
c9 :: C9
c9 = counter @"c9"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& build

-- collection of metrics, prevents from ambiguos metric names
collection =
      c1
  </> c2
  </> c3
  </> g1
  </> c4
  </> c5
  </> c6
  </> c7
  </> c8
  </> c9
  </> MNil
