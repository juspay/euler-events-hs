module EventSpec where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock.POSIX (posixDayLength, posixSecondsToUTCTime)
import Euler.Events.Class (logEvent, toLazyByteString)
import qualified Euler.Events.Constants as Constants
import Euler.Events.Logger.Stdout (StdoutConfig (StdoutConfig))
import Euler.Events.Types.Event (Event (Event), EventMetadata (EventMetadata))
import qualified Euler.Events.Types.Event as Event
import Euler.Events.Types.Order (Order (Order))
import qualified Euler.Events.Types.Order as Order
import Euler.Events.Types.Txn (Txn (Txn))
import qualified Euler.Events.Types.Txn as Txn
import Euler.Events.Types.TxnCardInfo (TxnCardInfo (TxnCardInfo))
import qualified Euler.Events.Types.TxnCardInfo as TxnCardInfo
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
  let time = posixSecondsToUTCTime posixDayLength
  let metadata =
        EventMetadata
          { Event.timestamp = time,
            Event.hostname = "euler-order",
            Event.xRequestId = "dummyXrequestId",
            Event.xGlobalRequestId = "dummyXGlobalRequestId",
            Event.txnUuid = Just "txn123",
            Event.orderId = Just "order123",
            Event.merchantId = Nothing
          }
  let orderPayload =
        Order
          { Order.orderId = "order123",
            Order.version = 0,
            Order.amount = 1.2,
            Order.status = Order.SUCCESS,
            Order.merchantId = "merchant123",
            Order.dateCreated = time,
            Order.lastModified = time,
            Order.eventType = Order.CREATE
          }
  let orderEvent =
        Event
          { Event.metadata = metadata,
            Event.eventLibraryVersion = Constants.eventLibraryVersion,
            Event.event = Event.OrderEvent,
            Event.message = orderPayload
          }
  describe "OrderRoundTrip" $ do
    it "encodes and decodes an Order Event" $
      ( (decode :: ByteString -> Maybe (Event Order))
          . toLazyByteString StdoutConfig () metadata
          $ orderPayload
      )
        `shouldBe` Just orderEvent
    it "prints an Order Event on stdout" $
      logEvent StdoutConfig () metadata orderPayload `shouldReturn` Nothing
  let txnPayload =
        Txn
          { Txn.version = 1,
            Txn.orderId = "order356",
            Txn.txnUuid = Just "txn123",
            Txn.txnId = "txnid25",
            Txn.txnAmount = Just 5.60,
            Txn.status = Txn.AUTHORIZATION_FAILED,
            Txn.dateCreated = Just time,
            Txn.lastModified = Nothing,
            Txn.merchantId = Just "merchant89",
            Txn.gateway = Just "Paytm",
            Txn.eventType = Txn.UPDATE
          }
  let txnEvent =
        Event
          { Event.metadata = metadata,
            Event.eventLibraryVersion = Constants.eventLibraryVersion,
            Event.event = Event.TxnEvent,
            Event.message = txnPayload
          }
  describe "TxnRoundTrip" $ do
    it "encodes and decodes a Txn Event" $
      ( (decode :: ByteString -> Maybe (Event Txn))
          . toLazyByteString StdoutConfig () metadata
          $ txnPayload
      )
        `shouldBe` Just txnEvent
    it "prints a Txn Event on stdout" $
      logEvent StdoutConfig () metadata txnPayload `shouldReturn` Nothing
  let txnCardInfoPayload =
        TxnCardInfo
          { TxnCardInfo.orderId = "order55",
            TxnCardInfo.txnUuid = "txn66",
            TxnCardInfo.dateCreated = Just time,
            TxnCardInfo.cardType = Nothing,
            TxnCardInfo.cardIssuerBankName = Just "ICICI",
            TxnCardInfo.paymentMethodType = Just TxnCardInfo.CARD,
            TxnCardInfo.paymentMethod = Just "VISA",
            TxnCardInfo.eventType = TxnCardInfo.UPDATE
          }
  let txnCardInfoEvent =
        Event
          { Event.metadata = metadata,
            Event.eventLibraryVersion = Constants.eventLibraryVersion,
            Event.event = Event.TxnCardInfoEvent,
            Event.message = txnCardInfoPayload
          }
  describe "TxnCardInfoRoundTrip" $ do
    it "encodes and decodes a TxnCardInfo Event" $
      ( (decode :: ByteString -> Maybe (Event TxnCardInfo))
          . toLazyByteString StdoutConfig () metadata
          $ txnCardInfoPayload
      )
        `shouldBe` Just txnCardInfoEvent
    it "prints a TxnCardInfo Event on stdout" $
      logEvent StdoutConfig () metadata txnCardInfoPayload
        `shouldReturn` Nothing
