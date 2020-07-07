{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Feature where

import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)
import           Euler.Class               (Event (toProtoEvent))
import           Prelude                   hiding (id)

import qualified Euler.Proto.Event         as Proto
import qualified Euler.Proto.Event.Feature as Proto
import           Euler.Util                (fromInt, fromSumType, fromText, fromUTCTime)

data Feature =
  Feature
    { id            :: Int
    , version       :: Int
    , enabled       :: Bool
    , name          :: Text
    , merchantId    :: Maybe Text
    , disabledUntil :: Maybe UTCTime
    -- extra info
    , eventType     :: FeatureEventType
    }

data FeatureEventType
  = FeatureCreate
  | FeatureUpdate

instance Event Feature where
  toProtoEvent Feature {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventFeature $
          Proto.Feature
            { Proto.featureId = fromInt id
            , Proto.featureVersion = fromInt version
            , Proto.featureEnabled = enabled
            , Proto.featureName = fromText name
            , Proto.featureMaybeMerchantId =
                Proto.FeatureMaybeMerchantIdMerchantId . fromText <$> merchantId
            , Proto.featureMaybeDisabledUntil =
                Proto.FeatureMaybeDisabledUntilDisabledUntil . fromUTCTime <$>
                disabledUntil
            , Proto.featureFeatureEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromEventType :: FeatureEventType -> Proto.FeatureEventType
fromEventType eventType =
  case eventType of
    FeatureCreate -> Proto.FeatureEventTypeFEATURE_EVENT_CREATE
    FeatureUpdate -> Proto.FeatureEventTypeFEATURE_EVENT_UPDATE
