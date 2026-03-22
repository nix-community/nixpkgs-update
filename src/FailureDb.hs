{-# LANGUAGE OverloadedStrings #-}

-- | Optional SQLite bridge: when @DATABASE_URL@ is set to a path (same DB as the
-- Rust tracker), record classified update failures on @packages.attr_path@.
-- Updates @last_update_attempt@ on each write. If no row matches @attr_path@,
-- inserts a minimal row (@id@ and @attr_path@ both set to the attr path).
--
-- When @NIXPKGS_UPDATE_FAILURE_LOG@ is set to a file path, each failure also
-- appends one JSON object (NDJSON) for log pipelines independent of SQLite.
--
-- @consecutive_failures@ increments when the stored failure kind matches the
-- new one, else resets to @1@. Success clears failure columns and sets
-- @last_success_at@.
module FailureDb
  ( recordPackageUpdateFailure,
    clearPackageUpdateFailure,
    readConsecutiveFailures,
    recordFailedWipPr,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple (Connection, Only (..), close, execute, open, query, query_)
import FailureKind (classifyFailureMessage, failureKindCode)
import System.Environment (lookupEnv)

-- | Bound stored failure text (full logs can be huge).
maxFailureMessageChars :: Int
maxFailureMessageChars = 12000

lastStatementChanges :: Connection -> IO Int
lastStatementChanges conn = do
  rows <- query_ conn "SELECT changes()" :: IO [Only Int]
  case rows of
    Only n : _ -> pure n
    _ -> pure 0

appendNdjsonFailureLog ::
  Text ->
  Text ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  IO ()
appendNdjsonFailureLog attrPath kind msg maintainerCount rebuildPaths = do
  mbPath <- lookupEnv "NIXPKGS_UPDATE_FAILURE_LOG"
  case mbPath of
    Nothing -> pure ()
    Just path -> do
      t <- getCurrentTime
      let ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
          row =
            object
              [ "attr_path" .= attrPath,
                "failure_kind" .= kind,
                "failure_message" .= msg,
                "maintainer_count" .= maintainerCount,
                "rebuild_path_count" .= rebuildPaths,
                "recorded_at_utc" .= ts
              ]
      BSL.appendFile path (encode row <> "\n")

-- | @DATABASE_URL@ is read once per call; failures to open or update are ignored
-- so the batch updater never aborts because of telemetry.
--
-- Optional @declared_maintainer_count@ and @last_rebuild_path_count@ are updated
-- only when the corresponding argument is 'Just' (SQL @COALESCE@ keeps the
-- previous row value when the argument is NULL).
recordPackageUpdateFailure ::
  Text ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  IO ()
recordPackageUpdateFailure attrPath rawMsg maintainerCount rebuildPaths = do
  let kind = failureKindCode (classifyFailureMessage rawMsg)
      msg = T.take maxFailureMessageChars rawMsg
      mc = fromIntegral <$> maintainerCount :: Maybe Int64
      mr = fromIntegral <$> rebuildPaths :: Maybe Int64
      run dbPath = do
        conn <- open dbPath
        execute
          conn
          "UPDATE packages SET last_update_failure_kind = ?, last_update_failure_message = ?, last_update_failure_at = datetime('now'), last_update_attempt = datetime('now'), declared_maintainer_count = COALESCE(?, declared_maintainer_count), last_rebuild_path_count = COALESCE(?, last_rebuild_path_count), consecutive_failures = CASE WHEN last_update_failure_kind IS NOT NULL AND last_update_failure_kind = ? THEN COALESCE(consecutive_failures, 0) + 1 ELSE 1 END WHERE attr_path = ?"
          (kind, msg, mc, mr, kind, attrPath)
        touched <- lastStatementChanges conn
        when (touched == 0) $
          void $
            try @SomeException $
              execute
                conn
                "INSERT INTO packages (id, attr_path, last_update_attempt, last_update_failure_kind, last_update_failure_message, last_update_failure_at, declared_maintainer_count, last_rebuild_path_count, consecutive_failures) VALUES (?, ?, datetime('now'), ?, ?, datetime('now'), ?, ?, 1)"
                (attrPath, attrPath, kind, msg, mc, mr)
        close conn
  void $
    try @SomeException $
      appendNdjsonFailureLog attrPath kind msg maintainerCount rebuildPaths
  void $
    try @SomeException $
      maybe (pure ()) run =<< lookupEnv "DATABASE_URL"

-- | Clears failure columns; sets @last_success_at@ and @consecutive_failures = 0@.
clearPackageUpdateFailure ::
  Text ->
  Maybe Int ->
  Maybe Int ->
  IO ()
clearPackageUpdateFailure attrPath maintainerCount rebuildPaths =
  void $
    try @SomeException $
      maybe (pure ()) run =<< lookupEnv "DATABASE_URL"
  where
    run dbPath = do
      conn <- open dbPath
      let mc = fromIntegral <$> maintainerCount :: Maybe Int64
          mr = fromIntegral <$> rebuildPaths :: Maybe Int64
      execute
        conn
        "UPDATE packages SET last_update_failure_kind = NULL, last_update_failure_message = NULL, last_update_failure_at = NULL, last_update_attempt = datetime('now'), last_success_at = datetime('now'), consecutive_failures = 0, declared_maintainer_count = COALESCE(?, declared_maintainer_count), last_rebuild_path_count = COALESCE(?, last_rebuild_path_count) WHERE attr_path = ?"
        (mc, mr, attrPath)
      close conn

-- | After a failure write, read the streak counter (for WIP PR body throttling).
readConsecutiveFailures :: Text -> IO (Maybe Int)
readConsecutiveFailures attrPath =
  try @SomeException go >>= \case
    Left _ -> pure Nothing
    Right x -> pure x
  where
    go = do
      mb <- lookupEnv "DATABASE_URL"
      case mb of
        Nothing -> pure Nothing
        Just dbPath -> do
          conn <- open dbPath
          rows <- query conn "SELECT consecutive_failures FROM packages WHERE attr_path = ? LIMIT 1" (Only attrPath) :: IO [Only Int]
          close conn
          pure $ case rows of
            Only n : _ -> Just n
            _ -> Nothing

-- | Record the nixpkgs PR number opened for a Tier-A failed update.
recordFailedWipPr :: Text -> Int -> IO ()
recordFailedWipPr attrPath prNum =
  void $
    try @SomeException $
      maybe (pure ()) run =<< lookupEnv "DATABASE_URL"
  where
    run dbPath = do
      conn <- open dbPath
      execute
        conn
        "UPDATE packages SET failed_update_pr = ?, failed_update_pr_opened_at = datetime('now') WHERE attr_path = ?"
        (prNum, attrPath)
      close conn
