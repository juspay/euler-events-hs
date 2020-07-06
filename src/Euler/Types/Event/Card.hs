{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.Card where

import           Data.Text              (Text)
import           Euler.Class            (Event (toProtoEvent))
import           Prelude                hiding (id)

import qualified Euler.Proto.Event      as Proto
import qualified Euler.Proto.Event.Card as Proto
import           Euler.Util             (fromInt, fromSumType, fromText)

data Card =
  Card
    { merchantAccountId     :: Int
    , customerId            :: Text
    , maskedCardNumber      :: Text
    , nameOnCard            :: Maybe Text
    , expMonth              :: Int
    , expYear               :: Int
    , cardIsin              :: Text
    , cardLastFourDigits    :: Text
    , cardReference         :: Text
    , cardFingerprint       :: Text
    , cardGlobalFingerprint :: Maybe Text
    , vaultProvider         :: VaultProvider
    , nickname              :: Maybe Text
    , cardType              :: Maybe CardType
    , cardIssuer            :: Maybe Text
    , cardBrand             :: Maybe Text
    , cardToken             :: Maybe Text
    -- extra info
    , eventType             :: CardEventType
    }

data CardType
  = Credit
  | Debit
  | Prepaid
  | NB
  | Wallet
  | PayLater
  | UPI
  | ATMCard
  | Reward

data VaultProvider
  = Juspay
  | PayU
  | Sodexo

data CardEventType
  = CardCreate
  | CardUpdate

instance Event Card where
  toProtoEvent Card {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventCard $
          Proto.Card
            { Proto.cardMerchantAccountId = fromInt merchantAccountId
            , Proto.cardCustomerId = fromText customerId
            , Proto.cardMaskedCardNumber = fromText maskedCardNumber
            , Proto.cardExpMonth = fromInt expMonth
            , Proto.cardExpYear = fromInt expYear
            , Proto.cardCardIsin = fromText cardIsin
            , Proto.cardCardLastFourDigits = fromText cardLastFourDigits
            , Proto.cardCardReference = fromText cardReference
            , Proto.cardCardFingerprint = fromText cardFingerprint
            , Proto.cardVaultProvider =
                fromSumType . fromVaultProvider $ vaultProvider
            , Proto.cardMaybeNameOnCard =
                Proto.CardMaybeNameOnCardNameOnCard . fromText <$> nameOnCard
            , Proto.cardMaybeCardGlobalFingerprint =
                Proto.CardMaybeCardGlobalFingerprintCardGlobalFingerprint .
                fromText <$>
                cardGlobalFingerprint
            , Proto.cardMaybeNickname =
                Proto.CardMaybeNicknameNickname . fromText <$> nickname
            , Proto.cardMaybeCardType =
                Proto.CardMaybeCardTypeCardType . fromSumType . fromCardType <$>
                cardType
            , Proto.cardMaybeCardIssuer =
                Proto.CardMaybeCardIssuerCardIssuer . fromText <$> cardIssuer
            , Proto.cardMaybeCardBrand =
                Proto.CardMaybeCardBrandCardBrand . fromText <$> cardBrand
            , Proto.cardMaybeCardToken =
                Proto.CardMaybeCardTokenCardToken . fromText <$> cardToken
            , Proto.cardCardEventType = fromSumType . fromEventType $ eventType
            }
      }

fromVaultProvider :: VaultProvider -> Proto.VaultProvider
fromVaultProvider vaultProvider =
  case vaultProvider of
    Juspay -> Proto.VaultProviderJUSPAY
    PayU   -> Proto.VaultProviderPAYU
    Sodexo -> Proto.VaultProviderSODEXO

fromCardType :: CardType -> Proto.CardType
fromCardType cardType =
  case cardType of
    Credit   -> Proto.CardTypeCREDIT
    Debit    -> Proto.CardTypeDEBIT
    Prepaid  -> Proto.CardTypePREPAID
    NB       -> Proto.CardTypeNB
    Wallet   -> Proto.CardTypeWALLET
    PayLater -> Proto.CardTypePAY_LATER
    UPI      -> Proto.CardTypeUPI
    ATMCard  -> Proto.CardTypeATM_CARD
    Reward   -> Proto.CardTypeREWARD

fromEventType :: CardEventType -> Proto.CardEventType
fromEventType eventType =
  case eventType of
    CardCreate -> Proto.CardEventTypeCARD_EVENT_CREATE
    CardUpdate -> Proto.CardEventTypeCARD_EVENT_UPDATE
