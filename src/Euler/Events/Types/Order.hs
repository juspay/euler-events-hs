{-
 Copyright 2023-24, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU Affero General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your option)
 any later version. This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Affero General Public License for more details. You should have
 received a copy of the GNU Affero General Public License along with this
 program. If not, see <https://www.gnu.org/licenses/>.
-}

module Euler.Events.Types.Order where

{-
This module depricated.
Please, use euler-types and make custom type in your api.
See src/Euler/Common/ExtraLog.hs from order-api for example.
This module left untouched for tests.
-}


import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (OrderEvent))
import GHC.Generics (Generic)

data Order = Order
  { orderId :: Text,
    version :: Int,
    amount :: Double,
    status :: OrderStatus,
    merchantId :: Text,
    dateCreated :: UTCTime,
    lastModified :: UTCTime,
    -- extra info
    eventType :: OrderEventType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderStatus
  = NEW
  | SUCCESS
  | NOT_FOUND
  | ERROR
  | JUSPAY_DECLINED
  | PENDING_AUTHENTICATION
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | AUTHORIZING
  | AUTHORIZED
  | CREATED
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  | CAPTURE_INITIATED
  | CAPTURE_FAILED
  | VOID_FAILED
  | AUTO_REFUNDED
  | PARTIAL_CHARGED
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OrderEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload Order where
  toEvent :: EventMetadata -> Order -> Event Order
  toEvent = toEvent' OrderEvent
