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

-- | A type-safe wrapper around "Prometheus" API from @prometheus-client@
-- package. Provides more exporessive types to rule out potential runtime
-- errors.
module Euler.Events.MetricAPI
  (
    -- * Defining and register metrics
    counter
  , gauge
  , (.&)
  , lbl
  , reg
    -- * Using metrics
    -- ** Counters
  , inc
  , add
    -- ** Gauges

    -- ** Histograms

  )
where

import Data.Coerce (coerce)
import Data.Kind
import Data.Text (pack)
import GHC.Exts (proxy#)
import GHC.TypeLits
import qualified Data.Text as T
import qualified Prometheus as P

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

Example of using well-typed API:

c1 = counter (pack "c1")
      .& lbl @"foo" @Int

c1' <- reg c1
inc c1' 42
add c1' 10 42

-}

-- TODO add histograms
data MetricSort = Counter | Gauge
  deriving stock (Show, Eq)

-- | Synonym for a type-level list of associated pairs
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

type PromPrim :: MetricSort -> Type
type family PromPrim s = r | r -> s where
  PromPrim 'Counter = P.Counter
  PromPrim 'Gauge = P.Gauge

type PrometheusThing :: MetricSort -> Labels -> Constraint
class PrometheusThing sort labels where
  data PromRep sort labels :: Type
  register :: (P.Info -> P.Metric (PromPrim sort)) -> MetricDef sort labels -> IO (PromRep sort labels)
  type PromAction labels :: Type
  runOperation :: (PromPrim sort -> IO ()) -> PromRep sort labels -> PromAction labels

-- | Bare metric, without any labels
instance PrometheusThing sort '[] where
  data instance PromRep sort '[] = PromRepBare (PromPrim sort)
  register con (MetricDef name) =
    (P.register $ con $ P.Info name name)
    >>= pure . PromRepBare
  type PromAction '[] = IO ()
  runOperation op (PromRepBare ref) = op ref

-- | 1-ary vector
instance (KnownSymbol l1, Show t1) => PrometheusThing sort '[ '(l1, t1)] where
  data instance PromRep sort '[ '(l1, t1)] = PromRepVec1 (P.Vector (T.Text) (PromPrim sort))
  register con (MetricDef name) =
    (P.register
      $ P.vector (pack $ symbolVal' @l1 proxy#)
      $ con $ P.Info name name)
    >>= pure . PromRepVec1
  type PromAction '[ '(l1, t1)] = t1 -> IO ()
  runOperation op (PromRepVec1 ref) v1 = P.withLabel ref (pack $ show v1) op

-- | 2-ary vector
instance (KnownSymbol l1, KnownSymbol l2, Show t1, Show t2)
  => PrometheusThing sort '[ '(l1, t1), '(l2, t2)] where
  data instance PromRep sort '[ '(l1, t1), '(l2, t2)] =
    PromRepVec2 (P.Vector (T.Text, T.Text) (PromPrim sort))
  register con (MetricDef name) =
      (P.register
        $ P.vector ls
        $ con $ P.Info name name)
      >>= pure . PromRepVec2
    where
      ls = ( pack $ symbolVal' @l1 proxy#
           , pack $ symbolVal' @l2 proxy#
           )
  type PromAction '[ '(l1, t1), '(l2, t2)] = t1 -> t2 -> IO ()
  runOperation op (PromRepVec2 ref) v1 v2 = P.withLabel ref ls op
    where
      ls = ( pack $ show v1
           , pack $ show v2
           )

-- TODO implement for 3..9-ary

-- | An aux typeclass to back one 'reg' for all sorts of metrics.
type Registrable :: MetricSort -> Constraint
class Registrable sort where
  cons :: P.Info -> P.Metric (PromPrim sort)

instance Registrable 'Counter where
  cons = P.counter

instance Registrable 'Gauge where
  cons = P.gauge

-- | Registers a metric by its definition, returning a wrapper around the primitive which
-- carries the sort and labels.
reg :: forall sort labels
    .  PrometheusThing sort labels
    => Registrable sort
    => MetricDef sort labels -> IO (PromRep sort labels)
reg = register cons

{-------------------------------------------------------------------------------
  Working with counters
-------------------------------------------------------------------------------}

inc :: forall labels. PrometheusThing 'Counter labels => PromRep 'Counter labels -> PromAction labels
inc = runOperation @'Counter @labels P.incCounter

add :: forall labels. PrometheusThing 'Counter labels => PromRep 'Counter labels -> Word -> PromAction labels
add rep value = runOperation @'Counter @labels
  (flip P.unsafeAddCounter $ fromIntegral value) rep






{-------------------------------------------------------------------------------
  Sample metrics (move to tests)
-------------------------------------------------------------------------------}

c0 = counter (pack "noLabels")

c1 = counter (pack "c1")
      .& lbl @"foo" @Int

c3 = counter (pack "c3")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool

c4 = counter (pack "c4")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool

c5 = counter (pack "c5")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool
      .& lbl @"foo1" @Bool

c6 = counter (pack "c6")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool
      .& lbl @"foo1" @Bool
      .& lbl @"bar1" @Bool

c7 = counter (pack "c7")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool
      .& lbl @"foo1" @Bool
      .& lbl @"bar1" @Bool
      .& lbl @"baz1" @Bool

c8 = counter (pack "c8")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool
      .& lbl @"foo1" @Bool
      .& lbl @"bar1" @Bool
      .& lbl @"baz1" @Bool
      .& lbl @"qux1" @Bool

c9 = counter (pack "c9")
      .& lbl @"foo" @Int
      .& lbl @"bar" @Bool
      .& lbl @"baz" @Bool
      .& lbl @"qux" @Bool
      .& lbl @"foo1" @Bool
      .& lbl @"bar1" @Bool
      .& lbl @"baz1" @Bool
      .& lbl @"qux1" @Bool
      .& lbl @"foo2" @Bool

-- c10 = counter (pack "c10")
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool
--       .& lbl @"qux1" @Bool
--       .& lbl @"foo2" @Bool
--       .& lbl @"bar2" @Bool

g1 = gauge (pack "metricName") .& lbl @"foo" @Int
