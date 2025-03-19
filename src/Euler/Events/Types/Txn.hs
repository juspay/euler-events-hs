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

module Euler.Events.Types.Txn where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (TxnEvent))
import GHC.Generics (Generic)

data Txn = Txn
  { version :: Int,
    orderId :: Text,
    txnUuid :: Maybe Text,
    txnId :: Text,
    txnAmount :: Maybe Double,
    status :: TxnStatus,
    dateCreated :: Maybe UTCTime,
    lastModified :: Maybe UTCTime,
    merchantId :: Maybe Text,
    gateway :: Maybe Text,
    -- extra info
    eventType :: TxnEventType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnStatus
  = STARTED
  | AUTHENTICATION_FAILED
  | JUSPAY_DECLINED
  | PENDING_VBV
  | VBV_SUCCESSFUL
  | AUTHORIZED
  | AUTHORIZATION_FAILED
  | CHARGED
  | AUTHORIZING
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  | NOP
  | CAPTURE_INITIATED
  | CAPTURE_FAILED
  | VOID_FAILED
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload Txn where
  toEvent :: EventMetadata -> Txn -> Event Txn
  toEvent = toEvent' TxnEvent
