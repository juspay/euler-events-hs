module Euler.Events.Types.TxnDetail where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Euler.Events.Class (Event)
import           GHC.Generics       (Generic)

data Txn =
  Txn
    { version      :: Int
    , orderId      :: Text
    , txnUuid      :: Maybe Text
    , txnId        :: Text
    , txnAmount    :: Maybe Double
    , currency     :: Maybe Text
    , status       :: TxnStatus
    , hostname     :: Text
    , errorMessage :: Maybe Text
    , dateCreated  :: Maybe UTCTime
    , lastModified :: Maybe UTCTime
    , merchantId   :: Maybe Text
    , gateway      :: Maybe Gateway
    -- extra info
    , eventType    :: TxnEventType
    , timestamp    :: UTCTime -- can we use lastModified for this
    }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data TxnEventType
  = TxnCreate
  | TxnUpdate
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Event Txn
