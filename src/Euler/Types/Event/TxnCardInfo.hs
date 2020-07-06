{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.TxnCardInfo where

import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           Euler.Class                   (Event (toProtoEvent))
import           Prelude                       hiding (id)

import qualified Euler.Proto.Event             as Proto
import qualified Euler.Proto.Event.TxnCardInfo as Proto
import           Euler.Util                    (fromInt, fromSumType, fromText, fromUTCTime)

data TxnCardInfo =
  TxnCardInfo
    { id                    :: Int
    , txnId                 :: Text
    , cardIsin              :: Maybe Text
    , cardIssuerBankName    :: Maybe Text
    , cardExpYear           :: Maybe Text
    , cardExpMonth          :: Maybe Text
    , cardSwitchProvider    :: Maybe Text
    , cardType              :: Maybe Text
    , cardLastFourDigits    :: Maybe Text
    , nameOnCard            :: Maybe Text
    , cardFingerprint       :: Maybe Text
    , cardReferenceId       :: Maybe Text
    , txnDetailId           :: Maybe Int
    , dateCreated           :: Maybe UTCTime
    , paymentMethodType     :: Maybe PaymentMethodType
    , paymentMethod         :: Maybe Text
    , cardGlobalFingerprint :: Maybe Text
    , paymentSource         :: Maybe Text
    , authType              :: Maybe Text
    -- extra info
    , eventType             :: TxnCardInfoEventType
    }

data PaymentMethodType
  = Wallet
  | UPI
  | NB
  | Card
  | Paylater
  | ConsumerFinance
  | Reward
  | Cash
  | Unknown

data TxnCardInfoEventType
  = TxnCardInfoCreate
  | TxnCardInfoUpdate

instance Event TxnCardInfo where
  toProtoEvent TxnCardInfo {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventTxnCardInfo $
          Proto.TxnCardInfo
            { Proto.txnCardInfoId = fromInt id
            , Proto.txnCardInfoTxnId = fromText txnId
            , Proto.txnCardInfoMaybeCardIsin =
                Proto.TxnCardInfoMaybeCardIsinCardIsin . fromText <$> cardIsin
            , Proto.txnCardInfoMaybeCardIssuerBankName =
                Proto.TxnCardInfoMaybeCardIssuerBankNameCardIssuerBankName .
                fromText <$>
                cardIssuerBankName
            , Proto.txnCardInfoMaybeCardExpYear =
                Proto.TxnCardInfoMaybeCardExpYearCardExpYear . fromText <$>
                cardExpYear
            , Proto.txnCardInfoMaybeCardExpMonth =
                Proto.TxnCardInfoMaybeCardExpMonthCardExpMonth . fromText <$>
                cardExpMonth
            , Proto.txnCardInfoMaybeCardSwitchProvider =
                Proto.TxnCardInfoMaybeCardSwitchProviderCardSwitchProvider .
                fromText <$>
                cardSwitchProvider
            , Proto.txnCardInfoMaybeCardType =
                Proto.TxnCardInfoMaybeCardTypeCardType . fromText <$> cardType
            , Proto.txnCardInfoMaybeCardLastFourDigits =
                Proto.TxnCardInfoMaybeCardLastFourDigitsCardLastFourDigits .
                fromText <$>
                cardLastFourDigits
            , Proto.txnCardInfoMaybeNameOnCard =
                Proto.TxnCardInfoMaybeNameOnCardNameOnCard . fromText <$>
                nameOnCard
            , Proto.txnCardInfoMaybeCardFingerprint =
                Proto.TxnCardInfoMaybeCardFingerprintCardFingerprint . fromText <$>
                cardFingerprint
            , Proto.txnCardInfoMaybeCardReferenceId =
                Proto.TxnCardInfoMaybeCardReferenceIdCardReferenceId . fromText <$>
                cardReferenceId
            , Proto.txnCardInfoMaybeTxnDetailId =
                Proto.TxnCardInfoMaybeTxnDetailIdTxnDetailId . fromInt <$>
                txnDetailId
            , Proto.txnCardInfoMaybeDateCreated =
                Proto.TxnCardInfoMaybeDateCreatedDateCreated . fromUTCTime <$>
                dateCreated
            , Proto.txnCardInfoMaybePaymentMethodType =
                Proto.TxnCardInfoMaybePaymentMethodTypePaymentMethodType .
                fromSumType . fromPaymentMethodType <$>
                paymentMethodType
            , Proto.txnCardInfoMaybePaymentMethod =
                Proto.TxnCardInfoMaybePaymentMethodPaymentMethod . fromText <$>
                paymentMethod
            , Proto.txnCardInfoMaybeCardGlobalFingerprint =
                Proto.TxnCardInfoMaybeCardGlobalFingerprintCardGlobalFingerprint .
                fromText <$>
                cardGlobalFingerprint
            , Proto.txnCardInfoMaybePaymentSource =
                Proto.TxnCardInfoMaybePaymentSourcePaymentSource . fromText <$>
                paymentSource
            , Proto.txnCardInfoMaybeAuthType =
                Proto.TxnCardInfoMaybeAuthTypeAuthType . fromText <$> authType
            , Proto.txnCardInfoTxnCardInfoEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromPaymentMethodType :: PaymentMethodType -> Proto.PaymentMethodType
fromPaymentMethodType paymentMethodType =
  case paymentMethodType of
    Wallet          -> Proto.PaymentMethodTypeWALLET
    UPI             -> Proto.PaymentMethodTypeUPI
    NB              -> Proto.PaymentMethodTypeNB
    Card            -> Proto.PaymentMethodTypeCARD
    Paylater        -> Proto.PaymentMethodTypePAYLATER
    ConsumerFinance -> Proto.PaymentMethodTypeCONSUMER_FINANCE
    Reward          -> Proto.PaymentMethodTypeREWARD
    Cash            -> Proto.PaymentMethodTypeCASH
    Unknown         -> Proto.PaymentMethodTypeUNKNOWN

fromEventType :: TxnCardInfoEventType -> Proto.TxnCardInfoEventType
fromEventType eventType =
  case eventType of
    TxnCardInfoCreate -> Proto.TxnCardInfoEventTypeTXN_CARD_INFO_EVENT_CREATE
    TxnCardInfoUpdate -> Proto.TxnCardInfoEventTypeTXN_CARD_INFO_EVENT_UPDATE
