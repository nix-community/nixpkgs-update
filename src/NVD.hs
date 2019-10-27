{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module NVD
  ( withVulnDB
  , getCVEs
  , Connection
  , ProductID
  , Version
  , CVE
  , CVEID
  , UTCTime
  ) where

import OurPrelude

import CVE (CVE(..), CVEID, cveMatcherList, parseFeed)
import Codec.Compression.GZip (decompress)
import Control.Exception (SomeException, ioError, try)
import Crypto.Hash.SHA256 (hashlazy)
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Hex (hex, unhex)
import Data.List (group)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock
  ( UTCTime
  , diffUTCTime
  , getCurrentTime
  , nominalDay
  , utctDay
  )
import Data.Time.ISO8601 (parseISO8601)
import Database.SQLite.Simple
  ( Connection
  , Only(..)
  , Query(..)
  , execute
  , executeMany
  , execute_
  , query
  , withConnection
  , withTransaction
  )
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
  ( XdgDirectory(..)
  , createDirectoryIfMissing
  , getXdgDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.IO.Error (userError)
import Utils (ProductID, Version)
import Version (matchVersion)

-- | Either @recent@, @modified@, or any year since @2002@.
type FeedID = String

type Extension = String

type Timestamp = UTCTime

type Checksum = BSL.ByteString

type DBVersion = Int

data Meta =
  Meta Timestamp Checksum

-- | Database version the software expects. If the software version is
-- higher than the database version or the database has not been updated in more
-- than 7.5 days, the database will be deleted and rebuilt from scratch. Bump
-- this when the database layout changes or the build-time data filtering
-- changes.
softwareVersion :: DBVersion
softwareVersion = 1

getDBPath :: IO FilePath
getDBPath = do
  cacheDir <- getXdgDirectory XdgCache "nixpkgs-update"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir </> "nvd.sqlite3"

withDB :: (Connection -> IO a) -> IO a
withDB action = do
  dbPath <- getDBPath
  withConnection dbPath action

markUpdated :: Connection -> IO ()
markUpdated conn = do
  now <- getCurrentTime
  execute conn "UPDATE meta SET last_update = ?" [now]

-- | Rebuild the entire database, redownloading all data.
rebuildDB :: IO ()
rebuildDB = do
  dbPath <- getDBPath
  removeFile dbPath
  withConnection dbPath $ \conn -> do
    execute_ conn "CREATE TABLE meta (db_version int, last_update text)"
    execute
      conn
      "INSERT INTO meta VALUES (?, ?)"
      (softwareVersion, "1970-01-01 00:00:00" :: Text)
    execute_ conn $
      Query $
      T.unlines
        [ "CREATE TABLE cves ("
        , "  cve_id text PRIMARY KEY,"
        , "  description text,"
        , "  published text,"
        , "  modified text)"
        ]
    execute_ conn $
      Query $
      T.unlines
        [ "CREATE TABLE matchers ("
        , "  cve_id text REFERENCES cve,"
        , "  product_id text,"
        , "  matcher text,"
        , "  UNIQUE(cve_id, product_id, matcher))"
        ]
    execute_ conn "CREATE INDEX matchers_by_product_id ON matchers(product_id)"
    years <- allYears
    forM_ years $ updateFeed conn
    markUpdated conn

feedURL :: FeedID -> Extension -> String
feedURL feed ext =
  "https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-" <> feed <> ext

throwString :: String -> IO a
throwString = ioError . userError

throwText :: Text -> IO a
throwText = throwString . T.unpack

allYears :: IO [FeedID]
allYears = do
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  return $ map show [2002 .. year]

parseMeta :: BSL.ByteString -> Either T.Text Meta
parseMeta raw = do
  let splitLine = second BSL.tail . BSL.break (== ':') . BSL.takeWhile (/= '\r')
  let fields = map splitLine $ BSL.lines raw
  lastModifiedDate <-
    note "no lastModifiedDate in meta" $ lookup "lastModifiedDate" fields
  sha256 <- note "no sha256 in meta" $ lookup "sha256" fields
  timestamp <-
    note "invalid lastModifiedDate in meta" $
    parseISO8601 $ BSL.unpack lastModifiedDate
  checksum <- note "invalid sha256 in meta" $ unhex sha256
  return $ Meta timestamp checksum

getMeta :: FeedID -> IO Meta
getMeta feed = do
  raw <- simpleHttp $ feedURL feed ".meta"
  either throwText pure $ parseMeta raw

getCVE :: Connection -> CVEID -> IO CVE
getCVE conn cveID_ = do
  cves <-
    query
      conn
      (Query $
       T.unlines
         [ "SELECT cve_id, description, published, modified"
         , "FROM cves"
         , "WHERE cve_id = ?"
         ])
      (Only cveID_)
  case cves of
    [cve] -> pure cve
    [] -> fail $ "no cve with id " <> (T.unpack cveID_)
    _ -> fail $ "multiple cves with id " <> (T.unpack cveID_)

getCVEs :: Connection -> ProductID -> Version -> IO [CVE]
getCVEs conn productID version = do
  rows <-
    query
      conn
      (Query $
       T.unlines
         [ "SELECT cve_id, matcher"
         , "FROM matchers"
         , "WHERE product_id = ?"
         , "ORDER BY cve_id"
         ])
      (Only productID)
  let cveIDs =
        map head $
        group $
        flip mapMaybe rows $ \(cveID_, matcher) ->
          if matchVersion matcher version
            then Just cveID_
            else Nothing
  forM cveIDs $ getCVE conn

putCVEs :: Connection -> [CVE] -> IO ()
putCVEs conn cves =
  withTransaction conn $ do
    executeMany
      conn
      (Query $
       T.unlines
         [ "REPLACE INTO cves(cve_id, description, published, modified)"
         , "VALUES (?, ?, ?, ?)"
         ])
      cves
    executeMany
      conn
      "DELETE FROM matchers WHERE cve_id = ?"
      (map (Only . cveID) cves)
    executeMany
      conn
      (Query $
       T.unlines
         [ "REPLACE INTO matchers(cve_id, product_id, matcher)"
         , "VALUES (?, ?, ?)"
         ])
      (concatMap cveMatcherList cves)

getDBMeta :: Connection -> IO (DBVersion, UTCTime)
getDBMeta conn = do
  rows <- query conn "SELECT db_version, last_update FROM meta" ()
  case rows of
    [meta] -> pure meta
    _ -> fail "failed to get meta information"

needsRebuild :: IO Bool
needsRebuild = do
  dbMeta <- try $ withDB getDBMeta
  currentTime <- getCurrentTime
  case dbMeta of
    Left (e :: SomeException) -> do
      putStrLn $ "rebuilding database because " <> show e
      pure True
    Right (dbVersion, t) ->
      pure $
      diffUTCTime currentTime t > (7.5 * nominalDay) ||
      dbVersion /= softwareVersion

-- | Download a feed and store it in the database.
updateFeed :: Connection -> FeedID -> IO ()
updateFeed conn feedID = do
  json <- downloadFeed feedID
  parsed <- either throwText pure $ parseFeed json
  putCVEs conn parsed

-- | Update the vulnerability database and run an action with a connection to
-- it.
withVulnDB :: (Connection -> IO a) -> IO a
withVulnDB action = do
  rebuild <- needsRebuild
  when rebuild rebuildDB
  withDB $ \conn -> do
    (_, lastUpdate) <- withDB getDBMeta
    currentTime <- getCurrentTime
    when (diffUTCTime currentTime lastUpdate > (0.25 * nominalDay)) $ do
      updateFeed conn "modified"
      markUpdated conn
    action conn

-- | Update a feed if it's older than a maximum age and return the contents as
-- ByteString.
downloadFeed :: FeedID -> IO BSL.ByteString
downloadFeed feed = do
  putStrLn $ "Updating National Vulnerability Database feed (" <> feed <> ")"
  Meta _ expectedChecksum <- getMeta feed
  compressed <- simpleHttp $ feedURL feed ".json.gz"
  let raw = decompress compressed
  let actualChecksum = BSL.fromStrict $ hashlazy raw
  when (actualChecksum /= expectedChecksum) $
    throwString $
    "wrong hash, expected: " <>
    BSL.unpack (hex expectedChecksum) <>
    " got: " <> BSL.unpack (hex actualChecksum)
  return raw
