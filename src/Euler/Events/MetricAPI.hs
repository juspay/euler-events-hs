-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoStarIsType #-}
-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
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

    -- * Defining and registering metrics
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

{-
TODO:
 * add help to MetricDef
-}

import Data.Coerce (coerce)
import Data.Kind
import Data.Text (pack)
import GHC.Exts (proxy#)
import GHC.TypeLits
import qualified Data.Text as T
import qualified Prometheus as P
import Unsafe.Coerce (unsafeCoerce)

{- $intro

Let's consider a typical workflow based on bare "Prometheus" API:

@
λ> myVector1 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> myVector2 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> withLabel myVector1 ("GET", "200") incCounter
λ> withLabel myVector2 ("GET", "200") incCounter
λ> withLabel myVector2 ("200", "GET") incCounter
λ> myVector3 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests_" "")
λ> exportMetricsAsText >>= Data.ByteString.Lazy.putStr
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
http_requests{name="200",name="GET"} 1.0
@

This snippet demonstrates several flaws of the API (plus denotes "fixed by this API" status)

* one might register several metrics sharing the same ambigous name
* you might give the same names to labels in vectors, making them ambigous (+)
* since all label values are just 'Text', one may accidenatlly mess up their order when using a metric (+)
* if there is no operations on a metric it won't make it to the exported data

This module solves __some__ of the issues mentioned, namely:

* compile-time checks that all labels have unique names
* lables are well-typed, allowing using typed values when working over them

Example of using well-typed API:

-- TODO update and use inline 
>>> -- creates a counter with one label
>>> c1 = counter (pack "c1") .& lbl @"foo" @Int
>>> -- registers a counter
>>> c1' <- reg c1
>>> -- incrementing a counter
>>> inc c1' 42
>>> -- adding up a value to a counter
>>> add c1' 10 42

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

-- type MetricDef :: MetricSort -> Symbol -> Labels -> Type
data MetricDef (sort :: MetricSort) (name :: Symbol) (labels :: Labels) = MetricDef

-- type MetricCollection :: [Symbol] -> Type
data MetricCollection (ns :: [Symbol]) where
  MNil  :: MetricCollection '[]
  (:+:) :: forall sort name labels ns
        .  ( UniqName name ns
           , PrometheusThing sort name labels
           , Registrable sort
           )
        => MetricDef sort name labels -> MetricCollection ns -> MetricCollection (name ': ns)
infixr 4 :+:

-- | An empty 'Metric'
counter :: forall name. MetricDef 'Counter name '[]
counter = MetricDef

gauge :: forall name. MetricDef 'Gauge name '[]
gauge = MetricDef

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

-- -- | 1-ary vector
-- instance (KnownSymbol l1, Show t1) => PrometheusThing sort '[ '(l1, t1)] where
--   data instance PromRep sort '[ '(l1, t1)] = PromRepVec1 (P.Vector (T.Text) (PromPrim sort))
--   register con (MetricDef name) =
--     (P.register
--       $ P.vector (pack $ symbolVal' @l1 proxy#)
--       $ con $ P.Info name name)
--     >>= pure . PromRepVec1
--   type PromAction '[ '(l1, t1)] = t1 -> IO ()
--   runOperation op (PromRepVec1 ref) v1 = P.withLabel ref (pack $ show v1) op

-- -- | 2-ary vector
-- instance (KnownSymbol l1, KnownSymbol l2, Show t1, Show t2)
--   => PrometheusThing sort '[ '(l1, t1), '(l2, t2)] where
--   data instance PromRep sort '[ '(l1, t1), '(l2, t2)] =
--     PromRepVec2 (P.Vector (T.Text, T.Text) (PromPrim sort))
--   register con (MetricDef name) =
--       (P.register
--         $ P.vector ls
--         $ con $ P.Info name name)
--       >>= pure . PromRepVec2
--     where
--       ls = ( pack $ symbolVal' @l1 proxy#
--            , pack $ symbolVal' @l2 proxy#
--            )
--   type PromAction '[ '(l1, t1), '(l2, t2)] = t1 -> t2 -> IO ()
--   runOperation op (PromRepVec2 ref) v1 v2 = P.withLabel ref ls op
--     where
--       ls = ( pack $ show v1
--            , pack $ show v2
--            )

-- TODO implement for 3..9-ary

-- | An aux typeclass to back one 'reg' for all sorts of metrics.
-- type Registrable :: MetricSort -> Constraint
class Registrable sort where
  cons :: P.Info -> P.Metric (PromPrim sort)

instance Registrable 'Counter where
  cons = P.counter

instance Registrable 'Gauge where
  cons = P.gauge

-- regCollection ::MetricCollection names -> IO ()
-- regCollection MNil = pure ()
-- regCollection (metric :+: tail) = do
--   reg metric
--   regCollection tail

-- type Elem :: Symbol -> [Symbol] -> Constraint
type family Elem (s :: Symbol) (ss :: [Symbol]) :: Constraint where
  Elem _ '[] = TypeError ('Text "Not an element!")
  Elem s (s:t) = ()
  Elem s (h:t) = Elem s t

-- | The same as reg, but requires a collection, for solely purpose of
-- checking metric name uniqueness
reg' :: forall sort name labels names
    .  PrometheusThing sort name labels
    => Registrable sort
    => Elem name names
    => MetricCollection names -> MetricDef sort name labels -> IO (PromRep sort name labels)
reg' _ def = register cons def

-- | Registers a metric by its definition, returning a wrapper around the primitive which
-- carries the sort and labels.
reg :: forall sort name labels
    .  PrometheusThing sort name labels
    => Registrable sort
    => MetricDef sort name labels -> IO (PromRep sort name labels)
reg = register cons

{-------------------------------------------------------------------------------
  Working with counters
-------------------------------------------------------------------------------}

inc :: forall name labels. PrometheusThing 'Counter name labels => PromRep 'Counter name labels -> PromAction labels
inc = runOperation @'Counter @name @labels P.incCounter

add :: forall name labels. PrometheusThing 'Counter name labels => PromRep 'Counter name labels -> Word -> PromAction labels
add rep value = runOperation @'Counter @name @labels
  (flip P.unsafeAddCounter $ fromIntegral value) rep






{-------------------------------------------------------------------------------
  Sample metrics (move to tests)
-------------------------------------------------------------------------------}

c0 = counter @"c0"

cfoo = counter @"foo"

-- c1 = counter @"c1"
--       .& lbl @"foo" @Int

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

g1 = gauge @"g1" .& lbl @"foo" @Int

collection1 = c0 :+: MNil
-- collection1 = c1 :+: c2 :+: MNil

c0' = reg' collection1 c0
cfoo' = reg' collection1 cfoo