module Euler.Types.Event where

import           Euler.Types.Event.Order (Order)

data Event =
  EventOrder Order
  deriving (Show)
