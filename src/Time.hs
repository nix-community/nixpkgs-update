module Time where

import OurPrelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

oneHourAgo :: MonadIO m => m UTCTime
oneHourAgo = liftIO $ addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime

twoHoursAgo :: MonadIO m => m UTCTime
twoHoursAgo =
  liftIO $ addUTCTime (fromInteger $ -60 * 60 * 2) <$> getCurrentTime

-- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
runDate :: MonadIO m => m Text
runDate =
  T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$>
  liftIO getCurrentTime
