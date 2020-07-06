{-# LANGUAGE RecordWildCards #-}

module Euler.Types.Event.TxnDetail where

import           Data.Text                   (Text)
import           Data.Time                   (UTCTime)
import           Euler.Class                 (Event (toProtoEvent))
import qualified Euler.Proto.Event           as Proto
import qualified Euler.Proto.Event.TxnDetail as Proto
import           Euler.Util                  (fromInt, fromSumType, fromText, fromUTCTime)
import           Prelude                     hiding (id)

data Txn =
  Txn
    { id                       :: Int
    , version                  :: Int
    , errorMessage             :: Maybe Text
    , orderId                  :: Text
    , status                   :: TxnStatus
    , txnId                    :: Text
    , txnType                  :: Text
    , dateCreated              :: Maybe UTCTime
    , lastModified             :: Maybe UTCTime
    , successResponseId        :: Maybe Int
    , txnMode                  :: Maybe Text
    , addToLocker              :: Maybe Bool
    , merchantId               :: Maybe Text
    , bankErrorCode            :: Maybe Text
    , bankErrorMessage         :: Maybe Text
    , gateway                  :: Maybe Gateway
    , expressCheckout          :: Maybe Bool
    , redirect                 :: Maybe Bool
    , gatewayPayload           :: Maybe Text
    , isEmi                    :: Maybe Bool
    , emiBank                  :: Maybe Text
    , emiTenure                :: Maybe Int
    , username                 :: Maybe Text
    , txnUuid                  :: Maybe Text
    , merchantGatewayAccountId :: Maybe Int
    , txnAmount                :: Maybe Double
    , txnObjectType            :: Maybe Text
    , sourceObject             :: Maybe Text
    , sourceObjectId           :: Maybe Text
    , currency                 :: Maybe Text
    , netAmount                :: Maybe Double
    , surchargeAmount          :: Maybe Double
    , taxAmount                :: Maybe Double
    -- extra info
    , eventType                :: TxnEventType
    }

data TxnStatus
  = STARTED
  | AUTHENTICATION_FAILED
  | JUSPAY_DECLINED
  | PENDING_VBV
  | VBV_SUCCESSFUL
  | AUTHORIZED
  | AUTHORIZATION_FAILED
  | CHARGED
  | AUTHORIZING
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  | NOP
  | CAPTURE_INITIATED
  | CAPTURE_FAILED
  | VOID_FAILED

data Gateway
  = AXIS
  | HDFC
  | ICICI
  | CITI
  | AMEX
  | CYBERSOURCE
  | IPG
  | MIGS
  | KOTAK
  | EBS
  | PAYU
  | CCAVENUE
  | CITRUS
  | ATOM
  | CCAVENUE_V2
  | TPSL
  | PAYTM
  | PAYTM_V2
  | PAYPAL
  | HDFC_EBS_VAS
  | PAYLATER
  | RAZORPAY
  | FSS_ATM_PIN
  | EBS_V3
  | ZAAKPAY
  | BILLDESK
  | SODEXO
  | BLAZEPAY
  | FSS_ATM_PIN_V2
  | MOBIKWIK
  | OLAMONEY
  | FREECHARGE
  | MPESA
  | SBIBUDDY
  | JIOMONEY
  | AIRTELMONEY
  | AMAZONPAY
  | PHONEPE
  | STRIPE
  | DUMMY
  | HDFC_IVR
  | ZESTMONEY
  | EPAYLATER
  | AXISNB
  | ICICINB
  | TPSL_SI
  | AXIS_UPI
  | HDFC_UPI
  | INDUS_UPI
  | KOTAK_UPI
  | SBI_UPI
  | ICICI_UPI
  | VIJAYA_UPI
  | HSBC_UPI
  | YESBANK_UPI
  | PAYTM_UPI
  | LINEPAY
  | OLAPOSTPAID
  | SIMPL
  | GOOGLEPAY
  | GOCASHFREE
  | PAYFORT
  | FSSPAY
  | CASH
  | MORPHEUS
  | FREECHARGE_V2
  | LAZYPAY
  | ITZCASH
  | AXIS_BIZ
  | LOANTAP
  | DEFAULT

data TxnEventType
  = TxnCreate
  | TxnUpdate

instance Event Txn where
  toProtoEvent Txn {..} =
    Proto.Event
      { Proto.eventEvent =
          Just $
          Proto.EventEventTxnDetail $
          Proto.TxnDetail
            { Proto.txnDetailId = fromInt id
            , Proto.txnDetailVersion = fromInt version
            , Proto.txnDetailMaybeErrorMessage =
                Proto.TxnDetailMaybeErrorMessageErrorMessage . fromText <$>
                errorMessage
            , Proto.txnDetailOrderId = fromText orderId
            , Proto.txnDetailTxnStatus = fromSumType . fromStatus $ status
            , Proto.txnDetailTxnId = fromText txnId
            , Proto.txnDetailTxnType = fromText txnType
            , Proto.txnDetailMaybeDateCreated =
                Proto.TxnDetailMaybeDateCreatedDateCreated . fromUTCTime <$>
                dateCreated
            , Proto.txnDetailMaybeLastModified =
                Proto.TxnDetailMaybeLastModifiedLastModified . fromUTCTime <$>
                lastModified
            , Proto.txnDetailMaybeSuccessResponseId =
                Proto.TxnDetailMaybeSuccessResponseIdSuccessResponseId . fromInt <$>
                successResponseId
            , Proto.txnDetailMaybeTxnMode =
                Proto.TxnDetailMaybeTxnModeTxnMode . fromText <$> txnMode
            , Proto.txnDetailMaybeAddToLocker =
                Proto.TxnDetailMaybeAddToLockerAddToLocker <$> addToLocker
            , Proto.txnDetailMaybeMerchantId =
                Proto.TxnDetailMaybeMerchantIdMerchantId . fromText <$>
                merchantId
            , Proto.txnDetailMaybeBankErrorCode =
                Proto.TxnDetailMaybeBankErrorCodeBankErrorCode . fromText <$>
                bankErrorCode
            , Proto.txnDetailMaybeBankErrorMessage =
                Proto.TxnDetailMaybeBankErrorMessageBankErrorMessage . fromText <$>
                bankErrorMessage
            , Proto.txnDetailMaybeGateway =
                Proto.TxnDetailMaybeGatewayGateway . fromSumType . fromGateway <$>
                gateway
            , Proto.txnDetailMaybeExpressCheckout =
                Proto.TxnDetailMaybeExpressCheckoutExpressCheckout <$>
                expressCheckout
            , Proto.txnDetailMaybeRedirect =
                Proto.TxnDetailMaybeRedirectRedirect <$> redirect
            , Proto.txnDetailMaybeGatewayPayload =
                Proto.TxnDetailMaybeGatewayPayloadGatewayPayload . fromText <$>
                gatewayPayload
            , Proto.txnDetailMaybeIsEmi =
                Proto.TxnDetailMaybeIsEmiIsEmi <$> isEmi
            , Proto.txnDetailMaybeEmiBank =
                Proto.TxnDetailMaybeEmiBankEmiBank . fromText <$> emiBank
            , Proto.txnDetailMaybeEmiTenure =
                Proto.TxnDetailMaybeEmiTenureEmiTenure . fromInt <$> emiTenure
            , Proto.txnDetailMaybeUsername =
                Proto.TxnDetailMaybeUsernameUsername . fromText <$> username
            , Proto.txnDetailMaybeTxnUuid =
                Proto.TxnDetailMaybeTxnUuidTxnUuid . fromText <$> txnUuid
            , Proto.txnDetailMaybeMerchantGatewayAccountId =
                Proto.TxnDetailMaybeMerchantGatewayAccountIdMerchantGatewayAccountId .
                fromInt <$>
                merchantGatewayAccountId
            , Proto.txnDetailMaybeTxnAmount =
                Proto.TxnDetailMaybeTxnAmountTxnAmount <$> txnAmount
            , Proto.txnDetailMaybeTxnObjectType =
                Proto.TxnDetailMaybeTxnObjectTypeTxnObjectType . fromText <$>
                txnObjectType
            , Proto.txnDetailMaybeSourceObject =
                Proto.TxnDetailMaybeSourceObjectSourceObject . fromText <$>
                sourceObject
            , Proto.txnDetailMaybeSourceObjectId =
                Proto.TxnDetailMaybeSourceObjectIdSourceObjectId . fromText <$>
                sourceObjectId
            , Proto.txnDetailMaybeCurrency =
                Proto.TxnDetailMaybeCurrencyCurrency . fromText <$> currency
            , Proto.txnDetailMaybeNetAmount =
                Proto.TxnDetailMaybeNetAmountNetAmount <$> netAmount
            , Proto.txnDetailMaybeSurchargeAmount =
                Proto.TxnDetailMaybeSurchargeAmountSurchargeAmount <$>
                surchargeAmount
            , Proto.txnDetailMaybeTaxAmount =
                Proto.TxnDetailMaybeTaxAmountTaxAmount <$> taxAmount
            , Proto.txnDetailTxnEventType =
                fromSumType . fromEventType $ eventType
            }
      }

fromStatus :: TxnStatus -> Proto.TxnStatus
fromStatus txnStatus =
  case txnStatus of
    STARTED               -> Proto.TxnStatusSTARTED
    AUTHENTICATION_FAILED -> Proto.TxnStatusAUTHENTICATION_FAILED
    JUSPAY_DECLINED       -> Proto.TxnStatusJUSPAY_DECLINED
    PENDING_VBV           -> Proto.TxnStatusPENDING_VBV
    VBV_SUCCESSFUL        -> Proto.TxnStatusVBV_SUCCESSFUL
    AUTHORIZED            -> Proto.TxnStatusAUTHORIZED
    AUTHORIZATION_FAILED  -> Proto.TxnStatusAUTHORIZATION_FAILED
    CHARGED               -> Proto.TxnStatusCHARGED
    AUTHORIZING           -> Proto.TxnStatusAUTHORIZING
    COD_INITIATED         -> Proto.TxnStatusCOD_INITIATED
    VOIDED                -> Proto.TxnStatusVOIDED
    VOID_INITIATED        -> Proto.TxnStatusVOID_INITIATED
    NOP                   -> Proto.TxnStatusNOP
    CAPTURE_INITIATED     -> Proto.TxnStatusCAPTURE_INITIATED
    CAPTURE_FAILED        -> Proto.TxnStatusCAPTURE_FAILED
    VOID_FAILED           -> Proto.TxnStatusVOID_FAILED

fromGateway :: Gateway -> Proto.Gateway
fromGateway gateway =
  case gateway of
    AXIS           -> Proto.GatewayAXIS
    HDFC           -> Proto.GatewayHDFC
    ICICI          -> Proto.GatewayICICI
    CITI           -> Proto.GatewayCITI
    AMEX           -> Proto.GatewayAMEX
    CYBERSOURCE    -> Proto.GatewayCYBERSOURCE
    IPG            -> Proto.GatewayIPG
    MIGS           -> Proto.GatewayMIGS
    KOTAK          -> Proto.GatewayKOTAK
    EBS            -> Proto.GatewayEBS
    PAYU           -> Proto.GatewayPAYU
    CCAVENUE       -> Proto.GatewayCCAVENUE
    CITRUS         -> Proto.GatewayCITRUS
    ATOM           -> Proto.GatewayATOM
    CCAVENUE_V2    -> Proto.GatewayCCAVENUE_V2
    TPSL           -> Proto.GatewayTPSL
    PAYTM          -> Proto.GatewayPAYTM
    PAYTM_V2       -> Proto.GatewayPAYTM_V2
    PAYPAL         -> Proto.GatewayPAYPAL
    HDFC_EBS_VAS   -> Proto.GatewayHDFC_EBS_VAS
    PAYLATER       -> Proto.GatewayPAYLATER
    RAZORPAY       -> Proto.GatewayRAZORPAY
    FSS_ATM_PIN    -> Proto.GatewayFSS_ATM_PIN
    EBS_V3         -> Proto.GatewayEBS_V3
    ZAAKPAY        -> Proto.GatewayZAAKPAY
    BILLDESK       -> Proto.GatewayBILLDESK
    SODEXO         -> Proto.GatewaySODEXO
    BLAZEPAY       -> Proto.GatewayBLAZEPAY
    FSS_ATM_PIN_V2 -> Proto.GatewayFSS_ATM_PIN_V2
    MOBIKWIK       -> Proto.GatewayMOBIKWIK
    OLAMONEY       -> Proto.GatewayOLAMONEY
    FREECHARGE     -> Proto.GatewayFREECHARGE
    MPESA          -> Proto.GatewayMPESA
    SBIBUDDY       -> Proto.GatewaySBIBUDDY
    JIOMONEY       -> Proto.GatewayJIOMONEY
    AIRTELMONEY    -> Proto.GatewayAIRTELMONEY
    AMAZONPAY      -> Proto.GatewayAMAZONPAY
    PHONEPE        -> Proto.GatewayPHONEPE
    STRIPE         -> Proto.GatewaySTRIPE
    DUMMY          -> Proto.GatewayDUMMY
    HDFC_IVR       -> Proto.GatewayHDFC_IVR
    ZESTMONEY      -> Proto.GatewayZESTMONEY
    EPAYLATER      -> Proto.GatewayEPAYLATER
    AXISNB         -> Proto.GatewayAXISNB
    ICICINB        -> Proto.GatewayICICINB
    TPSL_SI        -> Proto.GatewayTPSL_SI
    AXIS_UPI       -> Proto.GatewayAXIS_UPI
    HDFC_UPI       -> Proto.GatewayHDFC_UPI
    INDUS_UPI      -> Proto.GatewayINDUS_UPI
    KOTAK_UPI      -> Proto.GatewayKOTAK_UPI
    SBI_UPI        -> Proto.GatewaySBI_UPI
    ICICI_UPI      -> Proto.GatewayICICI_UPI
    VIJAYA_UPI     -> Proto.GatewayVIJAYA_UPI
    HSBC_UPI       -> Proto.GatewayHSBC_UPI
    YESBANK_UPI    -> Proto.GatewayYESBANK_UPI
    PAYTM_UPI      -> Proto.GatewayPAYTM_UPI
    LINEPAY        -> Proto.GatewayLINEPAY
    OLAPOSTPAID    -> Proto.GatewayOLAPOSTPAID
    SIMPL          -> Proto.GatewaySIMPL
    GOOGLEPAY      -> Proto.GatewayGOOGLEPAY
    GOCASHFREE     -> Proto.GatewayGOCASHFREE
    PAYFORT        -> Proto.GatewayPAYFORT
    FSSPAY         -> Proto.GatewayFSSPAY
    CASH           -> Proto.GatewayCASH
    MORPHEUS       -> Proto.GatewayMORPHEUS
    FREECHARGE_V2  -> Proto.GatewayFREECHARGE_V2
    LAZYPAY        -> Proto.GatewayLAZYPAY
    ITZCASH        -> Proto.GatewayITZCASH
    AXIS_BIZ       -> Proto.GatewayAXIS_BIZ
    LOANTAP        -> Proto.GatewayLOANTAP
    DEFAULT        -> Proto.GatewayDEFAULT

fromEventType :: TxnEventType -> Proto.TxnEventType
fromEventType txnEventType =
  case txnEventType of
    TxnCreate -> Proto.TxnEventTypeTXN_EVENT_CREATE
    TxnUpdate -> Proto.TxnEventTypeTXN_EVENT_UPDATE
