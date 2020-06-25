module Euler.Class where

import qualified Euler.Proto.Event as Proto

-- import           Proto3.Suite      (Message)
class Event a
  -- TODO (if possible): toProtoMsg :: Message b => a -> b
  where
  toProtoEvent :: a -> Proto.Event
