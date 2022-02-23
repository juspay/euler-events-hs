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
{-# LANGUAGE OverloadedLabels #-}

module Euler.Events.MetricAPI where

import Data.Coerce (coerce)
import Data.Kind
-- import Data.Proxy
import GHC.Exts (proxy#)
-- import GHC.Generics
-- import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

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
 * unique label names?
-}

type family CanAddLabel (types ::[(Symbol,Type)]) :: Constraint where
  CanAddLabel types = If ( Length types <=? 8) () (TypeError ('Text "You cannot use more than 9 labels."))

type family AddLabel (types :: [(Symbol, Type)]) (label :: Symbol) (typ :: Type) :: [(Symbol, Type)] where
  AddLabel types label typ = '(label, typ) ': types

type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond the els where
  If 'True  the els = the
  If 'False the els = els

type family Length (ls :: [k]) :: Nat where
  Length '[] = 0
  Length (l:ls) = 1 + Length ls

data MetricSort = Counter | Gauge

data Metric (sort :: MetricSort) (labels :: [(Symbol, Type)]) = Metric
  { name :: String
  , labels :: [String]
  }
  deriving stock (Show, Eq)

-- | An empty 'Metric'
counter :: String -> Metric 'Counter '[]
counter name = Metric name []

gauge :: String -> Metric 'Gauge '[]
gauge name = Metric name []

lbl
  :: forall (label :: Symbol) (typ :: Type) (types :: [(Symbol, Type)]) sort
  .  KnownSymbol label
  -- => Length types <= 1
  => CanAddLabel types
  => Metric sort types -> Metric sort ( '(label, typ) ': types)
lbl metric = coerce $ metric {labels = (symbolVal' @label proxy#) : labels metric }


c1 = counter "metricName"
      .& lbl @"foo" @Int

c2 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

c3 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c4 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c5 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c6 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c7 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c8 = counter "metricName"
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool

c9 = counter "metricName"
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"bar" @Bool
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool

-- c10 = counter "metricName"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"bar" @Bool
--       .& lbl @"bar" @Bool
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"bar" @Bool
--       .& lbl @"bar" @Bool
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"bar" @Bool

-- g1 = gauge "metricName" .& lbl @"foo" @Int


infixl 3 .&
-- | 'Data.Function.&' with higher precedence.
(.&) :: a -> (a -> b) -> b
a .& f = f a
{-# INLINE (.&) #-}

inc :: forall ls. PrometheusThing ls => Metric 'Counter ls -> PromRep ls
inc _ = execute @ls

class PrometheusThing (ls :: [(Symbol, Type)]) where
  type PromRep ls :: Type
  execute :: PromRep ls

instance (Show t1) => PrometheusThing ( '(l1,t1) ': '[] ) where
  type PromRep ( '(l1,t1) ': '[] ) = t1 -> IO String
  execute v1 = pure $ show v1

instance (Show t1, Show t2) => PrometheusThing ['(_l1,t1), '(_l2,t2)] where
  type PromRep ['(_l1,t1), '(_l2,t2)] = t1 -> t2 -> IO (String, String)
  execute v1 v2 = pure (show v1, show v2)

instance (Show t1, Show t2, Show t3) => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3)] = t1 -> t2 -> t3 -> IO (String, String, String)
  execute v1 v2 v3 = pure (show v1, show v2, show v3)

instance (Show t1, Show t2, Show t3, Show t4)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4)] =
    t1 -> t2 -> t3 -> t4 -> IO (String, String, String, String)
  execute v1 v2 v3 v4 = pure (show v1, show v2, show v3, show v4)

instance (Show t1, Show t2, Show t3, Show t4, Show t5)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5)] =
    t1 -> t2 -> t3 -> t4 -> t5 -> IO (String, String, String, String, String)
  execute v1 v2 v3 v4 v5 = pure (show v1, show v2, show v3, show v4, show v5)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6)] =
    t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IO (String, String, String, String, String, String)
  execute v1 v2 v3 v4 v5 v6 = pure (show v1, show v2, show v3, show v4, show v5, show v6)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7)] =
    t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> IO (String, String, String, String, String, String, String)
  execute v1 v2 v3 v4 v5 v6 v7 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8)] =
    t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> IO (String, String, String, String, String, String, String, String)
  execute v1 v2 v3 v4 v5 v6 v7 v8 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8)

instance (Show t1, Show t2, Show t3, Show t4, Show t5, Show t6, Show t7, Show t8, Show t9)
  => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3), '(_l4,t4), '(_l5,t5), '(_l6,t6), '(_l7,t7), '(_l8,t8), '(_l9,t9)] =
    t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> IO (String, String, String, String, String, String, String, String, String)
  execute v1 v2 v3 v4 v5 v6 v7 v8 v9 = pure (show v1, show v2, show v3, show v4, show v5, show v6, show v7, show v8, show v9)


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




