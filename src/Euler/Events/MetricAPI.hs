{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A type-safe wrapper around "Prometheus" API from @prometheus-client@
-- package. Provides more exporessive types to rule out some potential runtime
-- errors.
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
    -- * Using metrics
  , useMetric
    -- ** Counters
  , inc
  , add
    -- ** Gauges
  , incG
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
import Data.Text (pack)
import Type.Reflection
import GHC.Exts (proxy#)
import GHC.TypeLits
import qualified Data.Text as T
import qualified Prometheus as P
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
* empty names and labels are fine

This module solves all issues mentioned.

Example of using well-typed API:

-- TODO update and use inline
>>> -- creates a counter @c1@ with one label @foo@ of type 'Int'
>>> c1def = counter @"c1" .& lbl @"foo" @Int
-- another metric
>>> g1def = gauge @"g1"
-- collection of metrics, prevents from ambiguos metric names
>>> coll = g1def :+: c1def :+: MNil
>>> -- registers a metric
>>> c1 <- reg coll c1def
>>> -- use a counter
>>> inc c1 42
>>> -- adding up a value to a counter
>>> add c1 10 42

-}

-- TODO add histograms
data MetricSort = Counter | Gauge
  deriving stock (Show, Eq)

-- | Synonym for a type-level list of associated pairs
type Labels = [(Symbol, Type)]

-- type CanAddLabel :: Labels -> Constraint
type family CanAddLabel (types :: Labels) :: Constraint where
  CanAddLabel types = If ( Length types <=? 8) () (TypeError ('Text "You cannot use more than 9 labels."))

-- type CheckLabelUniqueness :: Symbol -> Labels -> Constraint
type family CheckLabelUniqueness (name :: Symbol) (labels :: Labels) :: Constraint where
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

-- type MetricDef :: MetricSort -> Symbol -> Labels -> Type
data MetricDef (sort :: MetricSort) (name :: Symbol) (labels :: Labels) = MetricDef

-- | Basic counter
counter :: forall name. NotEmpty name => MetricDef 'Counter name '[]
counter = MetricDef

-- Basic gauge
gauge :: forall name. NotEmpty name => MetricDef 'Gauge name '[]
gauge = MetricDef

-- | Attaches a label
lbl
  :: forall
      (label :: Symbol)
      (typ :: Type)
      (types :: Labels)
      (sort :: MetricSort)
      (name :: Symbol)
  .  KnownSymbol label
  => CanAddLabel types
  => CheckLabelUniqueness label types
  => NotEmpty label
  => MetricDef sort name types -> MetricDef sort name ( '(label, typ) ': types)
lbl = coerce

infixl 3 .&
-- | 'Data.Function.&' with higher precedence.
(.&) :: a -> (a -> b) -> b
a .& f = f a
{-# INLINE (.&) #-}


-- type PromPrim :: MetricSort -> Type
type family PromPrim s = r | r -> s where
  PromPrim 'Counter = P.Counter
  PromPrim 'Gauge = P.Gauge

-- type PrometheusThing :: MetricSort -> Symbol -> Labels -> Constraint
class PrometheusThing sort (name :: Symbol) labels where
  data PromRep sort name labels :: Type
  register :: (P.Info -> P.Metric (PromPrim sort)) -> MetricDef sort name labels -> IO (PromRep sort name labels)
  type PromAction labels :: Type
  runOperation :: (PromPrim sort -> IO ()) -> PromRep sort name labels -> PromAction labels

-- | An aux typeclass to back one 'reg' for all sorts of metrics.
-- type Registrable :: MetricSort -> Constraint
class Registrable sort where
  cons :: P.Info -> P.Metric (PromPrim sort)

instance Registrable 'Counter where
  cons = P.counter

instance Registrable 'Gauge where
  cons = P.gauge

build :: forall sort name labels
       . PrometheusThing sort name labels
      => Registrable sort
      => MetricDef sort name labels -> PromRep sort name labels
build def = unsafePerformIO $ register cons def

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
incG (SafetyBox m)= runOperation @'Gauge @name @labels P.incGauge m

{-------------------------------------------------------------------------------
  PrometheusThing instances, just repetative boilerplate
-------------------------------------------------------------------------------}

-- | Bare metric, without any labels
instance (KnownSymbol name) => PrometheusThing sort name '[] where
  data instance PromRep sort name '[] = PromRepBare (PromPrim sort)
  register con _ =
    let
      name = pack $ symbolVal' @name proxy#
      help = name
    in
      (P.register $ con $ P.Info name help)
        >>= pure . PromRepBare
  type PromAction '[] = IO ()
  runOperation op (PromRepBare ref) = op ref

-- | 1-ary vector
instance (KnownSymbol name, KnownSymbol l1, Show t1)
  => PrometheusThing sort name '[ '(l1, t1)] where
  data instance PromRep sort name '[ '(l1, t1)] =
    PromRepVec1 (P.Vector (T.Text) (PromPrim sort))
  register con _ =
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
  register con _ =
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
  register con _ =
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
  register con _ =
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


data Metrics (ts :: [Type]) (names :: [Symbol]) where
  MNil  :: Metrics '[] '[]
  (:+:) :: ( Typeable (PromRep sort name labels)
           , UniqName name names
           )
        => (PromRep sort name labels)
        -> Metrics ts names
        -> Metrics ((PromRep sort name labels) ': ts) ( name ': names)
infixr 4 :+:

type family ElemT (s :: Type) (ss :: [Type]) :: Constraint where
  ElemT _ '[] = TypeError ('Text "Not an element!")
  ElemT s (s:t) = ()
  ElemT s (h:t) = ElemT s t

-- | An opaque wrapper for metrics
newtype SafetyBox a = SafetyBox a

useMetric :: forall t ts names. (Typeable t, ElemT t ts) => Metrics ts names -> SafetyBox t
useMetric = go
  where
    -- Inner helper goes without recurring ElemT constraint
    go :: forall t ts names. (Typeable t) => Metrics ts names -> SafetyBox t
    -- This won't happen, guaranteed by top-level ElemT constraint
    go MNil = undefined
    go (h :+: rest) = case eqTypeRep (typeRep @t) (typeOf h) of
      Just HRefl -> SafetyBox h
      _ -> go rest

{-------------------------------------------------------------------------------
  Sample metrics (move to tests)
-------------------------------------------------------------------------------}

type C0 = PromRep 'Counter "c0" '[]
c0 :: C0
c0 = counter @"c0" .& build

type C1 = PromRep 'Counter "c1" '[ '("foo", Int)]
c1 :: C1
c1 =  counter @"c1"
      .& lbl @"foo" @Int
      .& build

coll = c0 :+: c1 :+: MNil

data MyRuntime ts names = MyRuntime
  { foo     :: Int
  , metrics :: Metrics ts names
  }

myRuntime = MyRuntime 42 (coll)

c1w = useMetric @C1 (metrics myRuntime)
-- won't compile
-- int11 = useMetric @Int (metrics myRuntime)

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

-- g1 = gauge @"g1" .& lbl @"foo" @Int
