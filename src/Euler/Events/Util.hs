{-
 Copyright 2023-24, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU Affero General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your option)
 any later version. This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Affero General Public License for more details. You should have
 received a copy of the GNU Affero General Public License along with this
 program. If not, see <https://www.gnu.org/licenses/>.
-}

module Euler.Events.Util where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Time.Clock (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Euler.Events.Constants (compressionLevel, compressionType, compressionEnable)
import Network.Wai.Middleware.Prometheus (Compression(..))
import System.Environment as SE
import Text.Read (readMaybe)

-- import           Proto3.Suite          (Enumerated (Enumerated))

fromInt :: Integral a => a -> Int64
fromInt = fromIntegral

-- fromSumType :: a -> Enumerated a
-- fromSumType = Enumerated . Right

fromText :: Text -> Text.Lazy.Text
fromText = fromStrict

fromUTCTime :: UTCTime -> Int64
fromUTCTime = fromInt . utcTimeToNanos

utcTimeToNanos :: UTCTime -> Integer
utcTimeToNanos t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t (posixSecondsToUTCTime 0)))
    `div` 1000

tshow :: Show a => a -> Text
tshow = pack . show

mbCompressionLevel :: IO (Maybe Int)
mbCompressionLevel = (>>= readMaybe) <$> SE.lookupEnv compressionLevel

mbCompressionEnable :: IO (Maybe Bool)
mbCompressionEnable = (>>= readMaybe) <$> SE.lookupEnv compressionEnable

mbCompressionType :: IO (Maybe Compression)
mbCompressionType = do
  v <- SE.lookupEnv compressionType
  mbCmpLevelValue <- mbCompressionLevel
  pure $ case fmap toLower <$> v of
    Just "zstd" ->  Just $ Zstd $ fromMaybe 1 mbCmpLevelValue
    Just "gzip" -> Just $ GZip $ fromMaybe 6 mbCmpLevelValue
    _ -> Nothing
