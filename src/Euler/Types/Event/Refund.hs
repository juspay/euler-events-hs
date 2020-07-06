{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Refund where

import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Euler.Class              (Event (toProtoEvent))
import qualified Euler.Proto.Event        as Proto
import qualified Euler.Proto.Event.Refund as Proto
import           Euler.Util               (fromInt, fromSumType, fromText, fromUTCTime)
import           Prelude                  hiding (id)

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
            , Proto.refundMaybeAuthorizationId =
                Proto.RefundMaybeAuthorizationIdAuthorizationId . fromText <$>
                authorizationId
            , Proto.refundDateCreated = fromUTCTime dateCreated
            , Proto.refundMaybeEpgTxnId =
                Proto.RefundMaybeEpgTxnIdEpgTxnId . fromText <$> epgTxnId
            , Proto.refundGateway = fromText gateway
            , Proto.refundProcessed = processed
            , Proto.refundMaybeRootReferenceNumber =
                Proto.RefundMaybeRootReferenceNumberRootReferenceNumber .
                fromText <$>
                rootReferenceNumber
            , Proto.refundMaybeTxnDetailId =
                Proto.RefundMaybeTxnDetailIdTxnDetailId . fromInt <$>
                txnDetailId
            , Proto.refundMaybeReferenceId =
                Proto.RefundMaybeReferenceIdReferenceId . fromText <$>
                referenceId
            , Proto.refundStatus = fromSumType . fromStatus $ status
            , Proto.refundMaybeUniqueRequestId =
                Proto.RefundMaybeUniqueRequestIdUniqueRequestId . fromText <$>
                uniqueRequestId
            , Proto.refundMaybeErrorMessage =
                Proto.RefundMaybeErrorMessageErrorMessage . fromText <$>
                errorMessage
            , Proto.refundMaybeSentToGateway =
                Proto.RefundMaybeSentToGatewaySentToGateway <$> sentToGateway
            , Proto.refundMaybeResponseCode =
                Proto.RefundMaybeResponseCodeResponseCode . fromText <$>
                responseCode
            , Proto.refundMaybeInternalReferenceId =
                Proto.RefundMaybeInternalReferenceIdInternalReferenceId .
                fromText <$>
                internalReferenceId
            , Proto.refundMaybeRefundArn =
                Proto.RefundMaybeRefundArnRefundArn . fromText <$> refundArn
            , Proto.refundMaybeInitiatedBy =
                Proto.RefundMaybeInitiatedByInitiatedBy . fromText <$>
                initiatedBy
            , Proto.refundMaybeRefundType =
                Proto.RefundMaybeRefundTypeRefundType . fromText <$> refundType
            , Proto.refundMaybeRefundSource =
                Proto.RefundMaybeRefundSourceRefundSource . fromText <$>
                refundSource
            , Proto.refundMaybeLastModified =
                Proto.RefundMaybeLastModifiedLastModified . fromUTCTime <$>
                lastModified
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
