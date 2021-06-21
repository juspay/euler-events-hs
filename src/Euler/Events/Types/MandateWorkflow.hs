module Euler.Events.Types.MandateWorkflow where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (WorkflowEvent))
import GHC.Generics (Generic)

data TrackingData = TrackingData 
    { model     :: Model
    , logType   :: Action
    , stage     :: Text
    , contents  :: Value
    , flowType  :: Flow
    }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Model = MANDATE_WORK_FLOW
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Action = APP_EVENT
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Flow = NOTIFICATION
  | NOTIFICATION_SYNC
  | MANDATE_EXECUTE
  | MANDATE_EXECUTE_SYNC

  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload TrackingData where
  toEvent :: EventMetadata -> TrackingData -> Event TrackingData
  toEvent = toEvent' WorkflowEvent
