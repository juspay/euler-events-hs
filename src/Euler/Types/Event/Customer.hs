{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Customer where

import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Euler.Class                (Event (toProtoEvent))
import           Prelude                    hiding (id)

import qualified Euler.Proto.Event          as Proto
import qualified Euler.Proto.Event.Customer as Proto
import           Euler.Util                 (fromInt, fromSumType, fromText, fromUTCTime)

data Customer =
  Customer
    { id                :: Text
    , version           :: Int
    , dateCreated       :: UTCTime
    , emailAddress      :: Text
    , firstName         :: Text
    , lastName          :: Text
    , lastUpdated       :: UTCTime
    , merchantAccountId :: Int
    , mobileCountryCode :: Text
    , mobileNumber      :: Text
    , objectReferenceId :: Text
    -- extra info
    , eventType         :: CustomerEventType
    }

data CustomerEventType
  = CustomerCreate
  | CustomerUpdate

instance Event Customer where
  toProtoEvent Customer {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventCustomer $
          Proto.Customer
            { Proto.customerId = fromText id
            , Proto.customerVersion = fromInt version
            , Proto.customerDateCreated = fromUTCTime dateCreated
            , Proto.customerEmailAddress = fromText emailAddress
            , Proto.customerFirstName = fromText firstName
            , Proto.customerLastName = fromText lastName
            , Proto.customerLastUpdated = fromUTCTime lastUpdated
            , Proto.customerMerchantAccountId = fromInt merchantAccountId
            , Proto.customerMobileCountryCode = fromText mobileCountryCode
            , Proto.customerMobileNumber = fromText mobileNumber
            , Proto.customerObjectReferenceId = fromText objectReferenceId
            , Proto.customerCustomerEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromEventType :: CustomerEventType -> Proto.CustomerEventType
fromEventType eventType =
  case eventType of
    CustomerCreate -> Proto.CustomerEventTypeCUSTOMER_EVENT_CREATE
    CustomerUpdate -> Proto.CustomerEventTypeCUSTOMER_EVENT_UPDATE
