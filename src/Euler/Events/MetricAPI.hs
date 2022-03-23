{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A type-safe wrapper around "Prometheus" API from @prometheus-client@
-- package. Provides more expressive types to rule out some potential runtime
-- errors for cases when your metrics are known in advance.
module Euler.Events.MetricAPI
  (
    -- * Introduction
    -- $intro

    -- * Building metrics
    counter
  , gauge
  , (.&)
  , lbl
  , build
  -- , lbl
  -- , build
    -- * Using metrics
  , useMetric
    -- ** Counters
  , inc
  , add
    -- ** Gauges
  , incGauge
    -- ** Histograms

  )
where

{-
TODO:
 * add help to MetricDef (easy)
 * add histograms support (easy)
 * implement instances for 5..9-arity
-}

import Data.Coerce (coerce)
import Data.Kind
import Data.Proxy
import Data.Text (pack)
import Type.Reflection
import GHC.Exts (proxy#)
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified Data.Text as T
import qualified Prometheus as P
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

{- $intro

Let's consider a typical workflow based on bare "Prometheus" API:

@
λ> myVector1 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> myVector2 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> withLabel myVector1 ("GET", "200") incCounter
λ> withLabel myVector2 ("GET", "200") incCounter
λ> withLabel myVector2 ("200", "GET") incCounter
λ> myVector3 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests_" "")
λ> register $ gauge $ Info "" ""
λ> exportMetricsAsText >>= Data.ByteString.Lazy.putStr
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
http_requests{name="200",name="GET"} 1.0
# HELP
# TYPE  gauge
 0.0
@

This snippet demonstrates several flaws of the API

* one might register several metrics sharing the same name
* you might give the same names to labels in vectors, making them ambigous
* since all label values are just 'Text', one may accidenatlly mess up their order when using a metric
* empty names and labels are considered to be just fine

This module solves all issues mentioned.

Example of using well-typed API:

>>> c1def = counter @"c1" .& lbl @"foo" @Int .& build
>>> g1def = gauge @"g1" .& build
>>> coll = g1def </> c1def </> MNil
>>> coll' <- reg coll
>>> inc (useMetric @(PromRep Counter "c1" '[("foo", Int)])) 42

== On difference between labelled/bare metrics

One peculiarity of labelled metrics in comparison to their unlabelled counterparts is that
when being registered, labelled metrics won't appear in 'exportMetricsAsText` output.
Indeed, in order to make it to the export data such metrics need to get at least one set
of their label values, otherwise it won't make sense. Keep in mind this discrepancy in behaviour.

-}

{-
* add support for histograms
* use symbols instead of types (Kyrylo's map should work here well)
-}

-- TODO add histograms
data MetricSort = Counter | Gauge
  deriving stock (Show, Eq)

-- | Kind synonym for a type-level list of associated pairs from
-- 'Symbol' to 'Type'
type STAssoc = [(Symbol, Type)]

-- type CanAddLabel :: STAssoc -> Constraint
type family CanAddLabel (types :: STAssoc) :: Constraint where
  CanAddLabel types = If ( Length types <=? 8) () (TypeError ('Text "You cannot use more than 9 labels."))

-- type CheckLabelUniqueness :: Symbol -> STAssoc -> Constraint
type family CheckLabelUniqueness (name :: Symbol) (labels :: STAssoc) :: Constraint where
  CheckLabelUniqueness name    ( '(name, _typ) ': tail) = TypeError ('Text "Label names must be unique across a metric.")
  CheckLabelUniqueness another ( '(name, _typ) ': tail) = CheckLabelUniqueness another tail
  CheckLabelUniqueness another '[] = ()

-- type If :: Bool -> Constraint -> Constraint -> Constraint
type family If (cond :: Bool) (the :: Constraint) (els :: Constraint) :: Constraint where
  If 'True  the _ = the
  If 'False _ els = els

type family Length (ls :: [k]) :: Nat where
  Length '[] = 0
  Length (l:ls) = 1 + Length ls

-- type EqSymbol :: Symbol -> Symbol -> Bool
type family EqSymbol (s1 :: Symbol) (s2 :: Symbol) where
  EqSymbol s1 s2 = EqOrd (CmpSymbol s1 s2)

-- type EqOrd :: Ordering -> Bool
type family EqOrd (o :: Ordering) :: Bool where
  EqOrd 'EQ = 'True
  EqOrd _   = 'False

-- type UniqName :: Symbol -> [Symbol] -> Constraint
type family UniqName (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  UniqName n '[] = ()
  UniqName n (h ': tail) = If (EqSymbol n h)
                             (TypeError ('Text "Metrics name must be unique in a collection"))
                             (UniqName n tail)

-- type NotEmpty :: Symbol -> Constraint
type family NotEmpty (s :: Symbol) where
  NotEmpty s = If (EqSymbol "" s) (TypeError ('Text "Empty names/labels are prohibited")) ()

-- | A definition of a metric.
-- type MetricDef :: MetricSort -> Symbol -> STAssoc -> Type
data MetricDef (sort :: MetricSort) (name :: Symbol) (labels :: STAssoc) = MetricDef

-- type PromPrim :: MetricSort -> Type
type family PromPrim s = r | r -> s where
  PromPrim 'Counter = P.Counter
  PromPrim 'Gauge = P.Gauge

-- type PrometheusThing :: MetricSort -> Symbol -> STAssoc -> Constraint
class PrometheusThing sort (name :: Symbol) labels where
  data PromRep sort name labels :: Type
  registerMetric :: (P.Info -> P.Metric (PromPrim sort)) -> MetricDef sort name labels -> IO (PromRep sort name labels)
  type PromAction labels :: Type
  runOperation :: (PromPrim sort -> IO ()) -> PromRep sort name labels -> PromAction labels
  -- | A hatch to add some strictness. After the registration this
  -- operation force the evaluation of the metric. The default
  -- implementation does nothing, but 0-ary instance performs
  -- an empty action to do the job.
  dummyOp :: (PromPrim sort -> IO ()) -> PromRep sort name labels -> IO ()
  dummyOp _ _ = pure ()

-- | An aux typeclass to back one 'reg' for all sorts of metrics.
-- type Registrable :: MetricSort -> Constraint
class Registrable sort where
  cons :: P.Info -> P.Metric (PromPrim sort)

instance Registrable 'Counter where
  cons = P.counter

instance Registrable 'Gauge where
  cons = P.gauge

-- | Basic counter
counter :: forall name. NotEmpty name => MetricDef 'Counter name '[]
counter = MetricDef

-- | Basic gauge
gauge :: forall name. NotEmpty name => MetricDef 'Gauge name '[]
gauge = MetricDef

type family Snoc (list ::[k]) (elem :: k) :: [k] where
  Snoc '[] e = '[e]
  Snoc (e ': es) elem = e ': (Snoc es elem)

-- | Attaches a label to a metric of any sort
lbl
  :: forall
      (label :: Symbol)
      (typ :: Type)
      (types :: STAssoc)
      (sort :: MetricSort)
      (name :: Symbol)
  .  KnownSymbol label
  => CanAddLabel types
  => CheckLabelUniqueness label types
  => NotEmpty label
  => MetricDef sort name types -> MetricDef sort name (Snoc types '(label, typ))
lbl = coerce

infixl 3 .&
-- | 'Data.Function.&' with higher precedence.
(.&) :: a -> (a -> b) -> b
a .& f = f a
{-# INLINE (.&) #-}

-- | Constructs (lazily) a metric (calculated by 'PromRep' family)
-- by its definition.
build :: forall sort name labels
       . (PrometheusThing sort name labels
       , Registrable sort)
       => MetricDef sort name labels -> PromRep sort name labels
build = unsafePerformIO . (registerMetric cons)

{-------------------------------------------------------------------------------
  Metrics collection
-------------------------------------------------------------------------------}

-- | The state of the metric collection
data MetricsState = Built | Registered

-- | Metrics collection, which is effectively a resticted heterogeneous list.
-- It allows only 'PromRep sort name labels` types in it. All types must be
-- uniqie which is guaranteed by @UniqName@ constraint.
--
-- It's polymorphic in @state@, so we are not cons ':+:' constructor, wich is
-- not supposed to be used with 'Registered' collections. Instead this module
-- exports '.&' operator.
data Metrics (state :: MetricsState) (ts :: [Type]) (names :: [Symbol]) (map :: STAssoc) where
  MNil  :: Metrics state '[] '[] '[]
  (:+:) :: ( Typeable (PromRep sort name labels)
           , PrometheusThing sort name labels
           , UniqName name names
           , KnownSymbol  s
           )
        => (Proxy s, PromRep sort name labels)
        -> Metrics state ts names as
        -> Metrics state ((PromRep sort name labels) ': ts) ( name ': names) ( '(s, PromRep sort name labels) ': as)
infixr 4 :+:

type family Concat (l :: [k]) (r :: [k]) where
   Concat '[] a = a
   Concat (x ': xs) a = x ': Concat xs a

-- | A concat operator for metric collections.
type family ConcatT m1 m2 where
  ConcatT (Metrics state '[] '[]) (Metrics state ts2 names2) = Metrics state ts2 names2
  -- ConcatT (Metrics state ts1 names1) (Metrics state '[] '[]) = Metrics state ts1 names1
  ConcatT (Metrics state ts1 names1) (Metrics state ts2 names2) =
    Metrics state (Concat ts1 ts2) (Concat names1 names2)

concatT :: UniqNames names1 names2
  => Metrics 'Built ts1 names1
  -> Metrics 'Built ts2 names2
  -> ConcatT (Metrics 'Built ts1 names1) (Metrics 'Built ts2 names2)
-- concatT m1 MNil = m1
concatT MNil m2 = m2
concatT (x1 :+: xs1) m2 = concatT xs1 (x1 </> m2)

-- | A cons operator for metric collections.
infixr 4 .>
(.>) :: ( Typeable (PromRep sort name labels)
      , PrometheusThing sort name labels
      , UniqName name names
      , KnownSymbol  s
      )
      => (Proxy s, PromRep sort name labels)
      -> Metrics 'Built ts names as
      -> Metrics 'Built ((PromRep sort name labels) ': ts) ( name ': names) ( '(s, PromRep sort name labels) ': as)
(.>) = (:+:)

type family ElemT (s :: Type) (ss :: [Type]) :: Constraint where
  ElemT _ '[] = TypeError ('Text "Not an element!")
  ElemT s (s:t) = ()
  ElemT s (h:t) = ElemT s t

type family ElemST (s :: Symbol) (map :: STAssoc) :: Constraint  where
  ElemST _ '[] = TypeError ('Text "not an element!")
  ElemST s ( '(s, t) ': r) = ()
  ElemST s ( '(_, _) ': r) = ElemST s r

type family TypeBySym (s :: Symbol) (map :: STAssoc) = r where
  TypeBySym s ( '(s, t) ': r) = t
  TypeBySym s ( '(_, _) ': r) = TypeBySym s r

mMap :: forall state ts names as r
      . (   forall sort name labels
          . PrometheusThing sort name labels
         => SafetyBox (PromRep sort name labels) -> r)
     -> Metrics state ts names as -> [r]
mMap _ MNil = []
mMap f ((_,m) :+: bs) = f (SafetyBox m) : mMap f bs

runDummyOp :: forall (sort :: MetricSort) (name :: Symbol) (labels :: STAssoc)
                . PrometheusThing sort name labels
               => SafetyBox (PromRep sort name labels) -> IO ()
runDummyOp  (SafetyBox m) = dummyOp @sort @name @labels (\_ -> pure ()) m

-- | An action to register a metric collection.
register :: Metrics 'Built ts names as -> IO (Metrics 'Registered ts names as)
register metrics = do
  _ <- sequence (mMap runDummyOp metrics)
  pure $ coerce metrics

-- | An opaque wrapper for metrics, to protect them from being used directly.
newtype SafetyBox a = SafetyBox a

-- | An accessor to get a metric from a collection. Since all types in a collection
-- is unique we can implement this function as a total one.
useMetric :: forall t ts names as. (Typeable t, ElemT t ts) => Metrics 'Registered ts names as -> SafetyBox t
useMetric = go
  where
    -- Inner helper goes without recurring ElemT constraint
    go :: forall t ts names as. (Typeable t) => Metrics 'Registered ts names as -> SafetyBox t
    -- This won't happen, guaranteed by top-level ElemT constraint
    go MNil = undefined
    go (h :+: rest) = case eqTypeRep (typeRep @t) (typeOf h) of
      Just HRefl -> SafetyBox h
      _ -> go rest

-- | Extract a metric by its name:
--
-- >>> collectin </> #metric_name
--
(</>) :: forall (s :: Symbol) ts names (as :: STAssoc)
       . (ElemST s as, KnownSymbol s)
      => Metrics 'Registered ts names as -> Proxy s -> SafetyBox (TypeBySym s as)
(</>) m _ = go m
  where
    go :: forall (s' :: Symbol) ts names (as' :: STAssoc). Metrics 'Registered ts names as' -> SafetyBox (TypeBySym s as)
    go MNil = undefined
    go ((proxy, value) :+: rest) = case eqTypeRep (typeOf (Proxy @s)) (typeOf proxy) of
      Just HRefl -> SafetyBox $ unsafeCoerce value
      Nothing  -> go rest

-- | Overloaded labels instance for more convenience
instance (KnownSymbol s, l ~ s) => IsLabel s (Proxy l) where
  fromLabel = Proxy @l

{-------------------------------------------------------------------------------
  Working with counters
-------------------------------------------------------------------------------}

inc :: forall name labels
     . PrometheusThing 'Counter name labels
    => SafetyBox (PromRep 'Counter name labels) -> PromAction labels
inc (SafetyBox m) = runOperation @'Counter @name @labels P.incCounter m

add :: forall name labels
     . PrometheusThing 'Counter name labels
    => SafetyBox (PromRep 'Counter name labels) -> Word -> PromAction labels
add (SafetyBox rep) value = runOperation @'Counter @name @labels
  (flip P.unsafeAddCounter $ fromIntegral value) rep

{-------------------------------------------------------------------------------
  Working with gauge
-------------------------------------------------------------------------------}

incGauge :: forall name labels
          . PrometheusThing 'Gauge name labels
         => SafetyBox (PromRep 'Gauge name labels) -> PromAction labels
incGauge (SafetyBox m)= runOperation @'Gauge @name @labels P.incGauge m




{-------------------------------------------------------------------------------
  PrometheusThing instances, just repetative boilerplate
-------------------------------------------------------------------------------}

-- | Bare metric, without any labels
instance (KnownSymbol name) => PrometheusThing sort name '[] where
  data instance PromRep sort name '[] = PromRepBare (PromPrim sort)
  registerMetric con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register $ con $ P.Info name help)
        >>= pure . PromRepBare
  type PromAction '[] = IO ()
  runOperation op (PromRepBare ref) = op ref
  dummyOp op (PromRepBare ref) = op ref

-- | 1-ary vector
instance (KnownSymbol name, KnownSymbol l1, Show t1)
  => PrometheusThing sort name '[ '(l1, t1)] where
  data instance PromRep sort name '[ '(l1, t1)] =
    PromRepVec1 (P.Vector (T.Text) (PromPrim sort))
  registerMetric con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register
        $ P.vector (pack $ symbolVal' @l1 proxy#)
        $ con $ P.Info name help)
        >>= pure . PromRepVec1
  type PromAction '[ '(l1, t1)] = t1 -> IO ()
  runOperation op (PromRepVec1 ref) v1 = P.withLabel ref (pack $ show v1) op

-- | 2-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1
         , KnownSymbol l2, Show t2
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2)] =
    PromRepVec2 (P.Vector (T.Text, T.Text) (PromPrim sort))
  registerMetric con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec2
  type PromAction '[ '(l1, t1), '(l2, t2)] = t1 -> t2 -> IO ()
  runOperation op (PromRepVec2 ref) v1 v2 =
    P.withLabel ref
      ( pack $ show v1
      , pack $ show v2
      )
      op

-- | 3-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1
         , KnownSymbol l2, Show t2
         , KnownSymbol l3, Show t3
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2), '(l3, t3)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2), '(l3, t3)] =
    PromRepVec3 (P.Vector (T.Text, T.Text, T.Text) (PromPrim sort))
  registerMetric con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec3
  type PromAction '[ '(l1, t1), '(l2, t2), '(l3, t3)] = t1 -> t2 -> t3 -> IO ()
  runOperation op (PromRepVec3 ref) v1 v2 v3 =
    P.withLabel ref
      ( pack $ show v1
      , pack $ show v2
      , pack $ show v3
      )
      op

-- | 4-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1
         , KnownSymbol l2, Show t2
         , KnownSymbol l3, Show t3
         , KnownSymbol l4, Show t4
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)] =
    PromRepVec4 (P.Vector (T.Text, T.Text, T.Text, T.Text) (PromPrim sort))
  registerMetric con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec4
  type PromAction '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)]
    = t1 -> t2 -> t3 -> t4 -> IO ()
  runOperation op (PromRepVec4 ref) v1 v2 v3 v4 =
    P.withLabel ref
      ( pack $ show v1
      , pack $ show v2
      , pack $ show v3
      , pack $ show v4
      )
      op








{-------------------------------------------------------------------------------
  Sample metrics (move to tests)
  See test/MetricApiSpec.hs
-------------------------------------------------------------------------------}

coll :: _
coll = (#cool_name, c2)
    .> (#specified, g1)
    .> (#be_means_of, c0)
    .> (#overloaded_labels, c1)
    .> MNil
  where
    c0 = counter @"c0" .& build

    c1 =  counter @"c1"
          .& lbl @"foo" @Int
          .& build

    c2 =  counter @"c2"
          .& lbl @"foo" @Int
          .& lbl @"bar" @Bool
          .& build

    g1 = gauge @"g1"
          .& lbl @"foo" @Int
          .& build



-- c2 = counter @"c2"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool

-- c3 = counter @"c3"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool

-- c4 = counter @"c4"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool

-- c5 = counter @"c5"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool

-- c6 = counter @"c6"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool

-- c7 = counter @"c7"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool

-- c8 = counter @"c8"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool
--       .& lbl @"qux1" @Bool

-- c9 = counter @"c9"
--       .& lbl @"foo" @Int
--       .& lbl @"bar" @Bool
--       .& lbl @"baz" @Bool
--       .& lbl @"qux" @Bool
--       .& lbl @"foo1" @Bool
--       .& lbl @"bar1" @Bool
--       .& lbl @"baz1" @Bool
--       .& lbl @"qux1" @Bool
--       .& lbl @"foo2" @Bool

-- -- c10 = counter @"c10"
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
