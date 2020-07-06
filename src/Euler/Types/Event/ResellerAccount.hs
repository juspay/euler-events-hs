{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.ResellerAccount where

import           Data.Text                         (Text)
import           Euler.Class                       (Event (toProtoEvent))
import           Prelude                           hiding (id)

import qualified Euler.Proto.Event                 as Proto
import qualified Euler.Proto.Event.ResellerAccount as Proto
import           Euler.Util                        (fromInt, fromSumType, fromText)

data ResellerAccount =
  ResellerAccount
    { resellerId          :: Int
    , userId              :: Int
    , resellerName        :: Text
    , resellerApiEndpoint :: Maybe Text
    -- extra info
    , eventType           :: ResellerAccountEventType
    }

data ResellerAccountEventType
  = ResellerAccountCreate
  | ResellerAccountUpdate

instance Event ResellerAccount where
  toProtoEvent ResellerAccount {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventResellerAccount $
          Proto.ResellerAccount
            { Proto.resellerAccountResellerId = fromInt resellerId
            , Proto.resellerAccountUserId = fromInt userId
            , Proto.resellerAccountResellerName = fromText resellerName
            , Proto.resellerAccountMaybeResellerApiEndpoint =
                Proto.ResellerAccountMaybeResellerApiEndpointResellerApiEndpoint .
                fromText <$>
                resellerApiEndpoint
            , Proto.resellerAccountResellerAccountEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromEventType :: ResellerAccountEventType -> Proto.ResellerAccountEventType
fromEventType eventType =
  case eventType of
    ResellerAccountCreate ->
      Proto.ResellerAccountEventTypeRESELLER_ACCOUNT_EVENT_CREATE
    ResellerAccountUpdate ->
      Proto.ResellerAccountEventTypeRESELLER_ACCOUNT_EVENT_UPDATE
