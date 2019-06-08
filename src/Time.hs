{-# LANGUAGE TemplateHaskell #-}

module Time where

import OurPrelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

data Time m a where
  Now :: Time m UTCTime

makeSem ''Time

runIO :: Member (Lift IO) r => Sem (Time ': r) a -> Sem r a
runIO =
  interpret $ \case
    Now -> sendM getCurrentTime

runPure :: UTCTime -> Sem (Time ': r) a -> Sem r a
runPure t =
  interpret $ \case
    Now -> pure t

oneHourAgo :: Member Time r => Sem r UTCTime
oneHourAgo = now <&> addUTCTime (fromInteger $ -60 * 60)

twoHoursAgo :: Member Time r => Sem r UTCTime
twoHoursAgo = now <&> addUTCTime (fromInteger $ -60 * 60 * 2)

-- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
-- unix depends on an earlier version currently https://github.com/haskell/unix/issues/131
runDate :: Member Time r => Sem r Text
runDate =
  now <&> formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <&>
  T.pack
