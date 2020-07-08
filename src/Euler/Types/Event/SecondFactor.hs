{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.SecondFactor where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)
import           Euler.Class                    (Event (toProtoEvent))
import           Prelude                        hiding (id)

import qualified Euler.Proto.Event              as Proto
import qualified Euler.Proto.Event.SecondFactor as Proto
import           Euler.Util                     (fromInt, fromSumType, fromText, fromUTCTime)

data SecondFactor =
  SecondFactor
    { id                      :: Int
    , version                 :: Int
    , otp                     :: Maybe Text
    , status                  :: Text
    , txnId                   :: Text
    , sfType                  :: Text
    , url                     :: Maybe Text
    , secondFactorResponse    :: Maybe Text
    , dateCreated             :: UTCTime
    , epgTxnId                :: Maybe Text
    , lastUpdated             :: UTCTime
    , txnDetailId             :: Maybe Int
    , gatewayAuthReqParams    :: Maybe Text
    , authenticationAccountId :: Maybe Text
    , canAcceptResponse       :: Maybe Bool
    , challengesAttempted     :: Maybe Int
    , responseAttempted       :: Maybe Int
  -- extra info
    , eventType               :: SecondFactorEventType
    }

data SecondFactorEventType
  = SecondFactorCreate
  | SecondFactorUpdate

instance Event SecondFactor where
  toProtoEvent SecondFactor {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventSecondFactor $
          Proto.SecondFactor
            { Proto.secondFactorId = fromInt id
            , Proto.secondFactorVersion = fromInt version
            , Proto.secondFactorStatus = fromText status
            , Proto.secondFactorTxnId = fromText txnId
            , Proto.secondFactorSfType = fromText sfType
            , Proto.secondFactorDateCreated = fromUTCTime dateCreated
            , Proto.secondFactorLastUpdated = fromUTCTime lastUpdated
            , Proto.secondFactorMaybeUrl =
                Proto.SecondFactorMaybeUrlUrl . fromText <$> url
            , Proto.secondFactorMaybeOtp =
                Proto.SecondFactorMaybeOtpOtp . fromText <$> otp
            , Proto.secondFactorMaybeSecondFactorResponse =
                Proto.SecondFactorMaybeSecondFactorResponseSecondFactorResponse .
                fromText <$>
                secondFactorResponse
            , Proto.secondFactorMaybeEpgTxnId =
                Proto.SecondFactorMaybeEpgTxnIdEpgTxnId . fromText <$> epgTxnId
            , Proto.secondFactorMaybeTxnDetailId =
                Proto.SecondFactorMaybeTxnDetailIdTxnDetailId . fromInt <$>
                txnDetailId
            , Proto.secondFactorMaybeGatewayAuthReqParams =
                Proto.SecondFactorMaybeGatewayAuthReqParamsGatewayAuthReqParams .
                fromText <$>
                gatewayAuthReqParams
            , Proto.secondFactorMaybeAuthenticationAccountId =
                Proto.SecondFactorMaybeAuthenticationAccountIdAuthenticationAccountId .
                fromText <$>
                authenticationAccountId
            , Proto.secondFactorMaybeCanAcceptResponse =
                Proto.SecondFactorMaybeCanAcceptResponseCanAcceptResponse <$>
                canAcceptResponse
            , Proto.secondFactorMaybeChallengesAttempted =
                Proto.SecondFactorMaybeChallengesAttemptedChallengesAttempted .
                fromInt <$>
                challengesAttempted
            , Proto.secondFactorMaybeResponseAttempted =
                Proto.SecondFactorMaybeResponseAttemptedResponseAttempted .
                fromInt <$>
                responseAttempted
            , Proto.secondFactorSecondFactorEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromEventType :: SecondFactorEventType -> Proto.SecondFactorEventType
fromEventType eventType =
  case eventType of
    SecondFactorCreate -> Proto.SecondFactorEventTypeSECOND_FACTOR_EVENT_CREATE
    SecondFactorUpdate -> Proto.SecondFactorEventTypeSECOND_FACTOR_EVENT_UPDATE
