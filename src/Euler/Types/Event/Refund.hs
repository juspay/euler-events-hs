{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Refund where

import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Euler.Class              (Event (toProtoEvent))
import           Prelude                  hiding (id)

import qualified Euler.Proto.Event        as Proto
import qualified Euler.Proto.Event.Refund as Proto
import           Euler.Util               (fromInt, fromMaybeBool, fromMaybeDouble, fromMaybeInt, fromMaybeText,
                                           fromMaybeUTCTime, fromSumType, fromText, fromUTCTime)

data Refund =
  Refund
    { id                  :: Int
    , amount              :: Double
    , authorizationId     :: Maybe Text
    , dateCreated         :: UTCTime
    , epgTxnId            :: Maybe Text
    , gateway             :: Text
    , processed           :: Bool
    , rootReferenceNumber :: Maybe Text
    , txnDetailId         :: Maybe Int
    , referenceId         :: Maybe Text
    , status              :: RefundStatus
    , uniqueRequestId     :: Maybe Text
    , errorMessage        :: Maybe Text
    , sentToGateway       :: Maybe Bool
    , responseCode        :: Maybe Text
    , internalReferenceId :: Maybe Text
    , refundArn           :: Maybe Text
    , initiatedBy         :: Maybe Text
    , refundType          :: Maybe Text
    , refundSource        :: Maybe Text
    , lastModified        :: Maybe UTCTime
    -- extra info
    , eventType           :: RefundEventType
    }

data RefundStatus
  = Failure
  | ManualReview
  | Pending
  | Success
  | TransactionFailure

data RefundEventType
  = RefundCreate
  | RefundUpdate

instance Event Refund where
  toProtoEvent Refund {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventRefund $
          Proto.Refund
            { Proto.refundId = fromInt id
            , Proto.refundAmount = amount
            , Proto.refundAuthorizationId = fromMaybeText authorizationId
            , Proto.refundDateCreated = fromUTCTime dateCreated
            , Proto.refundEpgTxnId = fromMaybeText epgTxnId
            , Proto.refundGateway = fromText gateway
            , Proto.refundProcessed = processed
            , Proto.refundRootReferenceNumber =
                fromMaybeText rootReferenceNumber
            , Proto.refundTxnDetailId = fromMaybeInt txnDetailId
            , Proto.refundReferenceId = fromMaybeText referenceId
            , Proto.refundStatus = fromSumType . fromStatus $ status
            , Proto.refundUniqueRequestId = fromMaybeText uniqueRequestId
            , Proto.refundErrorMessage = fromMaybeText errorMessage
            , Proto.refundSentToGateway = fromMaybeBool sentToGateway
            , Proto.refundResponseCode = fromMaybeText responseCode
            , Proto.refundInternalReferenceId =
                fromMaybeText internalReferenceId
            , Proto.refundRefundArn = fromMaybeText refundArn
            , Proto.refundInitiatedBy = fromMaybeText initiatedBy
            , Proto.refundRefundType = fromMaybeText refundType
            , Proto.refundRefundSource = fromMaybeText refundSource
            , Proto.refundLastModified = fromMaybeUTCTime lastModified
            , Proto.refundRefundEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromStatus :: RefundStatus -> Proto.RefundStatus
fromStatus refundStatus =
  case refundStatus of
    Failure            -> Proto.RefundStatusFAILURE
    ManualReview       -> Proto.RefundStatusMANUAL_REVIEW
    Pending            -> Proto.RefundStatusPENDING
    Success            -> Proto.RefundStatusSUCCESS
    TransactionFailure -> Proto.RefundStatusTRANSACTION_FAILURE

fromEventType :: RefundEventType -> Proto.RefundEventType
fromEventType eventType =
  case eventType of
    RefundCreate -> Proto.RefundEventTypeREFUND_EVENT_CREATE
    RefundUpdate -> Proto.RefundEventTypeREFUND_EVENT_UPDATE
