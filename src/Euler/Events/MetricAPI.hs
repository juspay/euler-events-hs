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
import Data.Proxy
import GHC.Exts (proxy#)
-- import GHC.Generics
-- import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

-- (?) you don't have to register a metric before use
-- (?) you cannot register metrics twice
-- you cannot perform operations on unregistered metrics

-- API "features":
-- you don't have to maintain references locally
-- operations validity is guaranteed by types (no inc on gauge, no Eithers in return types)
-- typed label arguments


-- -- | This family extracts name of the type from Generic 'Rep'.
-- type family GetTypeName (a :: k -> Type) :: Symbol where
--   GetTypeName (D1 ('MetaData name _ _ _) _) = name

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

-- type MetricLike :: forall k. (k -> Type) -> Constraint
class MetricLike (a :: k -> Type) where
  type CanAddLabel (types :: k) :: Constraint
  type AddLabel (types :: k) (label :: Symbol) (typ :: Type) = (result :: k) | result -> types label typ
  withLabel
    :: KnownSymbol label
    => CanAddLabel types
    => a types
    -> a (AddLabel types label typ)

type If :: Bool -> Constraint -> Constraint -> Constraint
type family If cond the els where
  If True  the els = the
  If False the els = els

instance MetricLike (Metric sort) where
  type CanAddLabel types = If ( (Length types) <=? 0) () (TypeError (Text "NahT!!!"))
  type AddLabel (types :: [(Symbol, Type)]) (label :: Symbol) (typ :: Type) = '(label, typ) ': types
  withLabel
    :: forall (label :: Symbol) (typ :: Type) (types :: [(Symbol, Type)])
    .  KnownSymbol label
    => CanAddLabel types
    => Metric sort types -> Metric sort ( '(label, typ) ': types)
  withLabel metric = coerce $ metric {labels = (symbolVal' @label proxy#) : labels metric }

-- | Synonym for 'withLabel' with label type variable as first one, enabling @lbl \@Foo@ type
-- application syntax.
lbl
  :: forall (label :: Symbol) (typ :: Type) k (types :: k) (a :: k -> Type)
  .  MetricLike a
  => KnownSymbol label
  => CanAddLabel types
  => a types
  -> a (AddLabel types label typ)
lbl = withLabel

{-
-- instance (KnownSymbol x, types ~ '[]) => IsLabel x (Metric types) where
--   fromLabel = defM (symbolVal' @x proxy#)

-- m1 :: Metric '[]
-- m1 = #metric_name

-- m2 = m1 .& lbl @"foo" @Int .& lbl @"bar" @Bool
-}

c1 = counter "metricName" .& lbl @"foo" @Int .& lbl @"bar" @Bool

c2 = counter "metricName" .& lbl @"foo" @Int

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

instance (Show t1, Show t2, Show t3) => PrometheusThing ['(_l1,t1), '(_l2,t2), '(_l3,t3)] where
  type PromRep ['(_l1,t1), '(_l2,t2), '(_l3,t3)] = t1 -> t2 -> t3 -> IO (String, String, String)
  execute v1 v2 v3 = pure (show v1, show v2, show v3)

instance (Show t1, Show t2) => PrometheusThing ['(_l1,t1), '(_l2,t2)] where
  type PromRep ['(_l1,t1), '(_l2,t2)] = t1 -> t2 -> IO (String, String)
  execute v1 v2 = pure (show v1, show v2)

instance (Show t1) => PrometheusThing ( '(l1,t1) ': '[] ) where
  type PromRep ( '(l1,t1) ': '[] ) = t1 -> IO String
  execute v1 = pure $ show v1

-- instance (PrometheusThing ls all, Show typ) => PrometheusThing ( '(label, typ) ': ls) all where
--   type PromRep ( '(label, typ) ': ls) all = typ -> PromRep ls all
--   execute acc _ _ val = execute (acc <> [show val]) (Proxy @ls) (Proxy @all)

-- instance PrometheusThing '[] all where
--   type PromRep '[] all = IO (Tuple (Length all))
--   execute acc _ _ = do
--     -- pure $ list2tuple' @all acc
--     -- pure $ list2Tuple @(Tuple (Length all)) acc
--     -- TODO
--     undefined

type family Tuple (size :: Nat) where
  Tuple 1 = String
  Tuple 2 = (String, String)
  Tuple 3 = (String, String, String)
  Tuple any = ()
  -- ...


list2tuple' :: forall ts src. (Show src) => src -> Tuple (Length ts)
list2tuple' = undefined

-- | shit!
list2Tuple :: (Read a1, Show a2) => a2 -> a1
list2Tuple lst = read $ "(" ++ ( init . tail . show ) lst ++ ")"



type family Length (ls :: [k]) :: Nat where
  Length '[] = 0
  Length (l:ls) = 1 + Length ls


-- type Tupable :: [Type] -> Constraint
-- class Tupable ts where
--   type TupleF ts
--   tuplify :: []




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




