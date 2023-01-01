{-# LANGUAGE TemplateHaskell #-}

module Time where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import OurPrelude

data Time m a where
  Now :: Time m UTCTime

makeSem ''Time

runIO :: Member (Embed IO) r => Sem (Time ': r) a -> Sem r a
runIO =
  interpret $ \case
    Now -> embed getCurrentTime

runPure :: UTCTime -> Sem (Time ': r) a -> Sem r a
runPure t =
  interpret $ \case
    Now -> pure t

-- | Return the UTC time 1 hour ago

-- $setup
-- >>> import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
-- >>> let exampleCurrentTime = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d" "2019-06-06" :: UTCTime
--
-- Examples:
--
-- >>> run $ runPure exampleCurrentTime oneHourAgo
-- 2019-06-05 23:00:00 UTC

oneHourAgo :: Member Time r => Sem r UTCTime
oneHourAgo = now <&> addUTCTime (fromInteger $ -60 * 60)

-- | Return the UTC time 2 hours ago
--
-- Examples:
--
-- >>> run $ runPure exampleCurrentTime twoHoursAgo
-- 2019-06-05 22:00:00 UTC
twoHoursAgo :: Member Time r => Sem r UTCTime
twoHoursAgo = now <&> addUTCTime (fromInteger $ -60 * 60 * 2)

-- | Return the current ISO8601 date and time without timezone
--
-- Examples:
--
-- >>> run $ runPure exampleCurrentTime runDate
-- "2019-06-06T00:00:00Z"
runDate :: Member Time r => Sem r Text
runDate = now <&> iso8601Show <&> T.pack
