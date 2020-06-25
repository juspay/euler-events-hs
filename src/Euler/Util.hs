module Euler.Util where

import           Data.Int              (Int64)
import           Data.Text             (Text)
import           Data.Text.Lazy        (fromStrict)
import qualified Data.Text.Lazy        as Text.Lazy
import           Data.Time.Clock       (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Euler.Constants       (defaultInt64, defaultLazyText)
import           Proto3.Suite          (Enumerated (Enumerated))

fromInt :: Integral a => a -> Int64
fromInt = fromIntegral

fromMaybeInt :: Integral a => Maybe a -> Int64
fromMaybeInt = maybe defaultInt64 fromInt

fromSumType :: a -> Enumerated a
fromSumType = Enumerated . Right

fromText :: Text -> Text.Lazy.Text
fromText = fromStrict

fromMaybeText :: Maybe Text -> Text.Lazy.Text
fromMaybeText = maybe defaultLazyText fromText

fromUTCTime :: UTCTime -> Int64
fromUTCTime = fromInt . utcTimeToNanos

fromMaybeUTCTime :: Maybe UTCTime -> Int64
fromMaybeUTCTime = maybe 0 fromUTCTime

utcTimeToNanos :: UTCTime -> Integer
utcTimeToNanos t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t (posixSecondsToUTCTime 0))) `div`
  1000
