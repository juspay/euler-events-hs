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

module Euler.Events.Types.TxnCardInfo where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Euler.Events.Class (EventPayload (toEvent, toEvent'))
import Euler.Events.Types.Event (Event, EventMetadata, EventType (TxnCardInfoEvent))
import GHC.Generics (Generic)

data TxnCardInfo = TxnCardInfo
  { orderId :: Text,
    txnUuid :: Text,
    dateCreated :: Maybe UTCTime,
    cardType :: Maybe Text,
    cardIssuerBankName :: Maybe Text,
    paymentMethodType :: Maybe PaymentMethodType,
    paymentMethod :: Maybe Text,
    -- extra info
    eventType :: TxnCardInfoEventType
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PaymentMethodType
  = WALLET
  | UPI
  | NB
  | CARD
  | PAYLATER
  | CONSUMER_FINANCE
  | REWARD
  | CASH
  | UNKNOWN
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnCardInfoEventType
  = CREATE
  | UPDATE
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance EventPayload TxnCardInfo where
  toEvent :: EventMetadata -> TxnCardInfo -> Event TxnCardInfo
  toEvent = toEvent' TxnCardInfoEvent
