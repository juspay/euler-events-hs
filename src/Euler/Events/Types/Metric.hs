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


module Euler.Events.Types.Metric where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data MetricOperation a
  = ReadyUp
  | ReadyDown (MetricResult a)
  | Increment Text -- Increment <counterName>
  | Set Text Double -- Set <gaugeName> <value>
  | RegisterVector1Counter Text Text -- RegisterVector1Counter <vectorName> <labelName>
  | IncrementVector1Counter Text Text -- IncrementVector1Counter <vectorName> <labelValue>

data MetricResult a
  = ReadyUpResult a
  | ReadyDownResult
  | Incremented
  | Setted
  | RegisteredVector1Counter
  | IncrementedVector1Counter
  | MetricResultError

data MetricKey
  = API_KEY_BASED_AUTHENTICATION_SUCCESS
  | AUTO_REFUNDED_TRANSACTIONS
  | CACHE_ADD
  | CACHE_DELETE
  | CACHE_HIT
  | CACHE_MISS
  | CLIENT_AUTHTOKEN_BASED_AUTHENTICATION_SUCCESS
  | CLIENT_AUTH_TOKEN_GENERATED
  | DB_FETCH_FAILURE
  | DB_SYNC_BATCH_SIZE
  | DB_SYNC_COMMAND_COUNT
  | DB_SYNC_DELAY
  | DB_SYNC_DIRTY_CMD
  | DB_SYNC_ERROR
  | DB_SYNC_PROCESS_TIME
  | DB_SYNC_STREAM_LENGTH
  | DB_SYNC_UNIQUE_CONSTRAINT_ERROR
  | EC_TIMEOUT_COUNT
  | EULER_HS_ORDER_STATUS_COUNT
  | EULER_HS_PAYMENT_STATUS_COUNT
  | EULER_ORDER_STATUS_COUNT
  | EULER_ORDER_STATUS_MISMATCH_COUNT
  | EULER_PAYMENT_STATUS_COUNT
  | EULER_PAYMENT_STATUS_MISMATCH_COUNT
  | EXTERNAL_API_CALL
  | EXTERNAL_API_CALL_FIRST_BYTE
  | EXTERNAL_API_CALL_LOOKUP
  | EXTERNAL_API_CALL_REQUEST
  | EXTERNAL_API_CALL_RESPONSE
  | EXTERNAL_API_CALL_SOCKET
  | EXTERNAL_API_CALL_TCP
  | EXTERNAL_API_CALL_TLS
  | EXTERNAL_API_CALL_TOTAL
  | FORCE_SYNC_FAIL
  | GATEWAY_SCORE_1D
  | GATEWAY_SCORE_2D
  | GATEWAY_SCORE_3D
  | GATEWAY_SCORE_4D
  | INCOMING_REFUND_WEBHOOK
  | INCOMING_REFUND_WEBHOOK_REFUND_SYNC
  | INVALID_REQUEST
  | LOG_CLASSIFICATION_COUNT
  | MEM_CACHE_HIT
  | MEM_CACHE_MISS
  | MEM_CACHE_RELOAD
  | MERCHANT_KEY_LOOKUP_API_KEY
  | MERCHANT_KEY_LOOKUP_HASH
  | ORDER_STATUS_CACHE_ADD
  | ORDER_STATUS_CACHE_HIT
  | ORDER_STATUS_CACHE_MISS
  | OUTGOING_WEBHOOK_REJECTED
  | REFUND_DECODE_ERROR
  | REFUND_FAILURE
  | REFUND_FORWARDED_EC
  | REFUND_INITIATED
  | REFUND_INVALID_REQUEST
  | REFUND_MOVED_TO_MANUAL_REVIEW_BY_CRON
  | REFUND_RECEIVED_EULER
  | REFUND_SUCCESS
  | REFUND_TRIGGERED
  | REFUND_WEBHOOK_FAILURE
  | REFUND_WEBHOOK_INITIATED
  | REFUND_WEBHOOK_SUCCESS
  | REQUEUED_REFUNDS_COUNT
  | SIGNATURE_AUTHENTICATION_INVALID_TIMESTAMP
  | SIGNATURE_AUTHENTICATION_TIMESTAMP_EXCEED_THRESHOLD
  | SIGNATURE_BASED_AUTHENTICATION_SUCCESS
  | STATUS_FORCE_SYNC
  | TEMP_CARD_EXPIRED_TOKEN
  | TEMP_CARD_INVALID_TOKEN
  | TEMP_CARD_VALID_TOKEN
  | TOTAL_LOCKS_ACQUIRED
  | TOTAL_REQUESTS_REJECTED
  | TOTAL_WEBHOOK_TRIGGERED
  | TXNS_CARD_STORED
  | TXNS_PROXIED_TO_EC
  | TXNS_START
  | TXNS_STATUS
  | UPI_WEBHOOK_FORWARDED_COUNT
  | UPI_WEBHOOK_RECEIVED_COUNT
  | UPI_WEBHOOK_SUCCESS_COUNT
  | VIES_CYBS_PA_ENROLL_FAILURE
  | VIES_CYBS_PA_ENROLL_SUCCESS
  | VIES_CYBS_PA_VALIDATE_FAILED
  | VIES_CYBS_PA_VALIDATE_SUCCESS
  | WALLET_FORWARDED_EC
  deriving (Show, Read, Eq, Ord)

data CachingMetrics = CachingMetrics
  { key        :: Text
  , redis_name :: Text
  }
  deriving (Show, Generic)

data LocksAcquiredLabel = LocksAcquiredLabel
  { base_key    :: Text
  , merchant_id :: Text
  }
  deriving (Show, Generic)

data TotalRequestsRejected = TotalRequestsRejected
  { base_key    :: Text
  , merchant_id :: Text
  }
  deriving (Show, Generic)
