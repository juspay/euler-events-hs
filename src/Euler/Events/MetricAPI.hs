{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Euler.Events.MetricAPI where

import Data.Coerce (coerce)
import Data.Kind
import Data.Void
-- import Data.Proxy
import GHC.Exts (proxy#)
-- import GHC.Generics
-- import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import qualified Prometheus as P
import qualified Data.Text as T
import Data.Text (pack)

{-
API safety:
 - (?) you don't have to register a metric before use
 - (?) you cannot register metrics twice
 - you cannot perform operations on unregistered metrics

API features:
 - you don't have to maintain references locally (hhm, original lib uses the global state)
 - operations validity is guaranteed by types (no inc on gauge, no Eithers in return types)
 - typed label arguments

TODOs
 * untill the first operation a metric doesn't make it to the report

Both issues can be illustrated by the following code:

λ> myVector1 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> myVector2 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> withLabel myVector1 ("GET", "200") incCounter
λ> withLabel myVector2 ("GET", "200") incCounter
λ> myVector3 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests_" "")
λ> exportMetricsAsText >>= Data.ByteString.Lazy.putStr
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0

-}

data MetricSort = Counter | Gauge
  deriving stock (Show, Eq)

-- | Synonym for a list of associated pair
type Labels = [(Symbol, Type)]

type CanAddLabel :: Labels -> Constraint
type family CanAddLabel types where
  CanAddLabel types = If ( Length types <=? 8) () (TypeError ('Text "You cannot use more than 9 labels."))

type CheckLabelUniqueness :: Symbol -> Labels -> Constraint
type family CheckLabelUniqueness name labels where
  CheckLabelUniqueness name    ( '(name, _typ) ': tail) = TypeError ('Text "Label names must be unique across a metric.")
  CheckLabelUniqueness another ( '(name, _typ) ': tail) = CheckLabelUniqueness another tail
  CheckLabelUniqueness another '[] = ()

type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond the els where
  If 'True  the els = the
  If 'False the els = els

type family Length (ls :: [k]) :: Nat where
  Length '[] = 0
  Length (l:ls) = 1 + Length ls

type MetricDef :: MetricSort -> Labels -> Type
data MetricDef sort labels = MetricDef T.Text

-- | An empty 'Metric'
counter :: T.Text -> MetricDef 'Counter '[]
counter = MetricDef

gauge :: T.Text -> MetricDef 'Gauge '[]
gauge = MetricDef

lbl
  :: forall
      (label :: Symbol)
      (typ :: Type)
      (types :: Labels)
      (sort :: MetricSort)
  .  KnownSymbol label
  => CanAddLabel types
  => CheckLabelUniqueness label types
  => MetricDef sort types -> MetricDef sort ( '(label, typ) ': types)
lbl = coerce

infixl 3 .&
-- | 'Data.Function.&' with higher precedence.
(.&) :: a -> (a -> b) -> b
a .& f = f a
{-# INLINE (.&) #-}

newtype Metric sort labels = Metric (PromRep sort labels)
-- data Metric sort labels = Metric

type PrometheusThing :: MetricSort -> Labels -> Constraint
class PrometheusThing sort labels where
  data PromRep sort labels :: Type
  register :: MetricDef sort labels -> IO (PromRep sort labels)
  type PromAction labels :: Type
  runOperation :: PromRep sort labels -> PromAction labels

instance PrometheusThing Counter '[] where
  data instance PromRep Counter '[] = PromRepCounter P.Counter
  register (MetricDef name) =
    (P.register $ P.counter $ P.Info name name)
    >>= pure . PromRepCounter
  type PromAction '[] = IO ()
  runOperation (PromRepCounter ref) = P.incCounter ref

instance (KnownSymbol l1, Show t1) => PrometheusThing Counter '[ '(l1, t1)] where
  data instance PromRep Counter '[ '(l1, t1)] = PromRepVec1Counter (P.Vector (T.Text) P.Counter)
  register (MetricDef name) =
    (P.register
      $ P.vector (pack $ symbolVal' @l1 proxy#)
      $ P.counter $ P.Info name name)
    >>= pure . PromRepVec1Counter
  type PromAction '[ '(l1, t1)] = t1 -> IO ()
  runOperation (PromRepVec1Counter ref) v1 = P.withLabel ref (pack $ show v1) P.incCounter

reg :: forall sort labels. PrometheusThing sort labels => MetricDef sort labels -> IO (PromRep sort labels)
reg = register

inc :: forall labels. PrometheusThing Counter labels => PromRep 'Counter labels -> PromAction labels
inc = runOperation @Counter @labels

c0 = counter (pack "noLabels")

c1 = counter (pack "c1")
      .& lbl @"foo" @Int

-- type Metric :: MetricSort -> Labels -> Type
-- data Metric sort labels = Metric

-- inc :: forall types. PrometheusThing 'Counter types => Metric 'Counter types -> PromAction 'Counter types
-- inc = execute @('Counter) @types

-- type PrometheusThing :: MetricSort -> Labels -> Constraint
-- class PrometheusThing sort ls where
--   type PromAction sort ls :: Type
--   execute :: Metric sr ls -> PromAction sort ls

-- instance PrometheusThing 'Counter '[] where
--   type PromAction 'Counter '[] = IO ()
--   execute m = pure ()





-- instance PrometheusThing '[] where
--   type PromRep '[] = IO ()
--   type VecLabels '[] = Void
--   execute m = do
--     case m of
--       MkCounter _ _ (Just (MetricCounterImpl ref)) -> P.incCounter ref
--       _ -> pure ()
--   labels = undefined

-- instance (KnownSymbol l1, Show t1) => PrometheusThing '[ '(l1,t1)] where
--   type PromRep '[ '(l1,t1)] = t1 -> IO String
--   type VecLabels '[ '(l1,t1)] = T.Text
--   execute _ v1 = pure $ show v1
--   labels = pack $ symbolVal' @l1 proxy#

-- instance (KnownSymbol l1, KnownSymbol l2, Show t1, Show t2)
--   => PrometheusThing ['(l1,t1), '(l2,t2)] where
--   type PromRep ['(l1,t1), '(l2,t2)] = t1 -> t2 -> IO (String, String)
--   type VecLabels ['(l1,t1), '(l2,t2)] = (T.Text, T.Text)
--   execute _ v1 v2 = pure (show v1, show v2)
--   labels =  (pack $ symbolVal' @l1 proxy#, pack $ symbolVal' @l2 proxy#)

-- instance (Show t1, Show t2, Show t3) => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3)] = t1 -> t2 -> t3 -> IO (String, String, String)
--   execute _ v1 v2 v3 = pure (show v1, show v2, show v3)

-- instance (Show t1, Show t2, Show t3, Show t4)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] =
--     t1 -> t2 -> t3 -> t4 -> IO (String, String, String, String)
--   execute _ v1 v2 v3 v4 = pure (show v1, show v2, show v3, show v4)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> IO (String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 = pure (show v1, show v2, show v3, show v4, show v5)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IO (String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 = pure (show v1, show v2, show v3, show v4, show v5, show v6)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> IO (String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> IO (String, String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 v8 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> IO (String, String, String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 v8 v9 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8, show v9)



-- type MetricImpl :: MetricSort -> Type
-- data family MetricImpl
-- newtype instance MetricImpl 'Counter = MetricCounterImpl P.Counter
-- newtype instance MetricImpl 'Gauge = MetricGaugeImpl P.Gauge
-- -- newtype instance MetricImpl 'Counter = MetricVector1CounterImpl (P.Vector T.Text P.Counter)
-- -- newtype instance MetricImpl 'Gauge = MetricVector1GaugeImpl (P.Vector T.Text P.Gauge)

-- -- | The definition of the metric
-- type Metric :: MetricState -> MetricSort -> [(Symbol, Type)] -> Type
-- data Metric state sort labels where
--   MkCounter :: String -> [String] -> Maybe (MetricImpl 'Counter) -> Metric state 'Counter labels
--   MkGauge :: String -> [String] -> Maybe (MetricImpl 'Gauge) -> Metric state 'Gauge labels

-- type Metric' :: MetricState -> MetricSort -> [(Symbol, Type)] -> Type
-- data Metric' state sort labels = Metric'
--   { ref :: VectorImpl labels (MetricImpl sort) }


-- attachRef :: MetricImpl sr -> Metric st sr ls -> Metric st sr ls
-- attachRef r m = case m of
--   MkCounter name labels _ -> MkCounter name labels (Just r)
--   MkGauge name labels _ -> MkGauge name labels (Just r)

--   -- { name :: String
--   -- , labels :: [String]
--   -- , ref :: Maybe (MetricImpl sort)
--   -- }
--   -- deriving stock (Show, Eq)




-- -- register:: Metric 'Created sort types -> IO (Metric 'Registered sort types)
-- -- register = \case
-- --   metric@(MkCounter name _ _) -> do
-- --     ref <- P.register $ P.counter (P.Info (pack name) (pack name))
-- --     pure $ (MetricCounterImpl ref)) metric
-- --   -- metric@(MkGauge name _ _) -> do
-- --   --   ref <- P.register $ P.gauge (P.Info (pack name) (pack name))
-- --   --   (pure . coerce. attachRef (MetricGaugeImpl ref)) metric



-- -- register':: Metric 'Created sort types -> IO (Metric' 'Registered sort types)
-- -- register' = \case
-- --   metric@(MkCounter name _ _) -> do
-- --     ref <- P.register $ P.counter (P.Info (pack name) (pack name))
-- --     (pure . coerce . attachRef (MetricCounterImpl ref)) metric
-- --   metric@(MkGauge name _ _) -> do
-- --     ref <- P.register $ P.gauge (P.Info (pack name) (pack name))
-- --     (pure . coerce. attachRef (MetricGaugeImpl ref)) metric

-- reg' :: forall sort types. PrometheusThing types => Metric 'Created sort types -> IO (Metric' 'Registered sort types)
-- reg' m = case m of
--   MkCounter _ _ _ -> do
--     let vLabels = labels @types
--     undefined


-- inc :: forall types. PrometheusThing types => Metric 'Registered 'Counter types -> PromRep types
-- inc m = execute @types @_ @('Counter) m

-- class PrometheusThing (ls :: [(Symbol, Type)]) where
--   type PromRep ls :: Type
--   type VecLabels ls :: Type
--   execute :: Metric st sr ls -> PromRep ls
--   labels :: VecLabels ls

-- instance PrometheusThing '[] where
--   type PromRep '[] = IO ()
--   type VecLabels '[] = Void
--   execute m = do
--     case m of
--       MkCounter _ _ (Just (MetricCounterImpl ref)) -> P.incCounter ref
--       _ -> pure ()
--   labels = undefined

-- instance (KnownSymbol l1, Show t1) => PrometheusThing '[ '(l1,t1)] where
--   type PromRep '[ '(l1,t1)] = t1 -> IO String
--   type VecLabels '[ '(l1,t1)] = T.Text
--   execute _ v1 = pure $ show v1
--   labels = pack $ symbolVal' @l1 proxy#

-- instance (KnownSymbol l1, KnownSymbol l2, Show t1, Show t2)
--   => PrometheusThing ['(l1,t1), '(l2,t2)] where
--   type PromRep ['(l1,t1), '(l2,t2)] = t1 -> t2 -> IO (String, String)
--   type VecLabels ['(l1,t1), '(l2,t2)] = (T.Text, T.Text)
--   execute _ v1 v2 = pure (show v1, show v2)
--   labels =  (pack $ symbolVal' @l1 proxy#, pack $ symbolVal' @l2 proxy#)

-- instance (Show t1, Show t2, Show t3) => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3)] = t1 -> t2 -> t3 -> IO (String, String, String)
--   execute _ v1 v2 v3 = pure (show v1, show v2, show v3)

-- instance (Show t1, Show t2, Show t3, Show t4)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] =
--     t1 -> t2 -> t3 -> t4 -> IO (String, String, String, String)
--   execute _ v1 v2 v3 v4 = pure (show v1, show v2, show v3, show v4)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> IO (String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 = pure (show v1, show v2, show v3, show v4, show v5)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IO (String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 = pure (show v1, show v2, show v3, show v4, show v5, show v6)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> IO (String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> IO (String, String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 v8 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8)

-- instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9)
--   => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] where
--   type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] =
--     t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> IO (String, String, String, String, String, String, String, String, String)
--   execute _ v1 v2 v3 v4 v5 v6 v7 v8 v9 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8, show v9)


-- ---

-- c0 = counter "noLabels"

-- c1 = counter "c1"
--       .& lbl @"foo" @Int

-- c2 = counter "c2"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool

-- c3 = counter "c3"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool

-- c4 = counter "c4"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool

-- c5 = counter "c5"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool

-- c6 = counter "c6"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool

-- c7 = counter "c7"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool

-- c8 = counter "c8"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool
--       .& lbl @"qux1" @Bool

-- c9 = counter "c9"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool
--       .& lbl @"qux1" @Bool
--       .& lbl @"foo2" @Bool

-- -- c10 = counter "c10"
-- --       .& lbl @"foo" @Int
-- --       .& lbl @"bar" @Bool
-- --       .& lbl @"baz" @Bool
-- --       .& lbl @"qux" @Bool
-- --       .& lbl @"foo1" @Bool
-- --       .& lbl @"bar1" @Bool
-- --       .& lbl @"baz1" @Bool
-- --       .& lbl @"qux1" @Bool
-- --       .& lbl @"foo2" @Bool
-- --       .& lbl @"bar2" @Bool

-- -- g1 = gauge "metricName" .& lbl @"foo" @Int



---
-- 2
---

-- data MetricSort = Counter | Gauge | Summary | Histogram

-- type Metric :: MetricSort -> Symbol -> [Symbol] -> Type
-- data Metric sort name labels where
--   MkCounter :: Metric 'Counter name '[]
--   MkCounterWithLabels :: Metric 'Counter name labels
--   -- +6 cons, not an issue indeed!

-- counter :: forall (name :: Symbol). Metric 'Counter name '[]
-- counter = MkCounter

-- counterWithLabels :: forall (name :: Symbol) (labels :: [Symbol]). Metric 'Counter name labels
-- counterWithLabels = MkCounterWithLabels

-- cnt = counter @"name"
-- cntWithLabels = counterWithLabels @"name" @["foo", "bar"]

-- -- one for both cons!
-- inc :: Metric 'Counter name labels -> IO ()
-- inc = undefined

-- -- one for both cons!
-- set :: Metric 'Gauge name labels -> IO ()
-- set = undefined


---
-- 1
---

-- data MetricKind = Counter | Gauge | Summary | Histogram

-- type Metric :: MetricKind -> Symbol -> Type
-- data Metric kind name where
--   MkCounter :: Metric 'Counter name
--   MkGauge   :: Metric 'Gauge name
--   -- ...

-- counter :: forall (name :: Symbol). Metric 'Counter name
-- counter = MkCounter

-- cnt1 = counter @"my_counter"
-- gauge1 = MkGauge @"my_gauge"

-- type CounterF :: forall (name :: Symbol). Metric 'Counter name
-- type family CounterF where
--   CounterF = 'MkCounter

-- type Vector :: forall (kind :: MetricKind) (name :: Symbol). Metric kind name -> [Symbol] -> Type
-- data Vector metric labels where
--   MkVector :: Metric _k _name -> Vector metric labels

-- -- vector :: Vector m labels
-- -- vector m = MkVector m

-- vec1 = MkVector @('MkCounter @"my_counter") @["foo", "bar"]
-- vec2 = MkVector @('MkGauge @"my_gauge") @["foo", "bar", "baz"]

-- inc :: Metric 'Counter name -> IO ()
-- inc = undefined

-- -- Nice!
-- foo1 = inc cnt1
-- -- foo2 = inc vec1

-- withinVector :: forall metric labels kind name. Vector metric labels -> (Metric kind name -> IO ()) -> IO ()
-- withinVector = undefined

-- foo3 = withinVector vec1 inc
-- foo4 = withinVector vec2 inc

-- -- label :: [Symbol] -> Metric -> Vector

-- -- register :: Metric kind ls
-- -- register = undefined

-- -- inc :: Metric 'Counter _ls -> IO ()

-- -- set :: Metric 'Gauge _ls -> ...
-- -- observe :: Metric Histogram ls ->




