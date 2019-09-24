{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module NVD where

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
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime
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
  , getModificationTime
  , getXdgDirectory
  , removeFile
  )
import System.FilePath ((<.>), (</>))
import System.IO.Error (userError)
import Utils (ProductID, Version)
import Version (matchVersion)

-- | Either @recent@, @modified@, or any year since @2002@.
type FeedID = String

type Extension = String

type Timestamp = UTCTime

type Checksum = BSL.ByteString

type CompressedFeed = BSL.ByteString

type MaxAge = NominalDiffTime

data Meta =
  Meta Timestamp Checksum

getDBPath :: IO FilePath
getDBPath = do
  cacheDir <- getXdgDirectory XdgCache "nixpkgs-update"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir </> "nvd.sqlite3"

withDB :: (Connection -> IO a) -> IO a
withDB action = do
  dbPath <- getDBPath
  withConnection dbPath action

-- | Rebuild the entire database, redownloading all data.
rebuildDB :: IO ()
rebuildDB = do
  dbPath <- getDBPath
  removeFile dbPath
  withConnection dbPath $ \conn -> do
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
    execute_ conn $
      Query $
      T.unlines
        ["CREATE INDEX matchers_by_product_id", "ON matchers(product_id)"]
    years <- allYears
    forM_ years $ downloadFeed conn (7.5 * nominalDay)

feedURL :: FeedID -> Extension -> String
feedURL feed ext =
  "https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-" <> feed <> ext

throwString :: String -> IO a
throwString = ioError . userError

throwText :: Text -> IO a
throwText = throwString . T.unpack

allYears :: IO [FeedID]
allYears = do
  now <- liftIO getCurrentTime
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
      (Query $ T.unlines ["DELETE FROM matchers", "WHERE cve_id = ?"])
      (map (Only . cveID) cves)
    executeMany
      conn
      (Query $
       T.unlines
         [ "REPLACE INTO matchers(cve_id, product_id, matcher)"
         , "VALUES (?, ?, ?)"
         ])
      (concatMap cveMatcherList cves)

lastModification :: Connection -> IO UTCTime
lastModification conn = do
  rows <- query conn "SELECT max(modified) FROM cves" ()
  case rows of
    [[timestamp]] -> pure timestamp
    _ -> fail "failed to get last modification time"

-- | Download a feed and store it in the database.
downloadFeed :: Connection -> MaxAge -> FeedID -> IO ()
downloadFeed conn maxAge feedID
  -- TODO: Because the database may need to be rebuilt frequently during
  -- development, we cache the json in files to avoid redownloading. After
  -- development is done, it can be downloaded directly without caching.
 = do
  json <- cacheFeedInFile maxAge feedID
  parsed <- either throwText pure $ parseFeed json
  putCVEs conn parsed

updateVulnDB :: IO ()
updateVulnDB = do
  cacheTime <- try $ withDB lastModification
  currentTime <- getCurrentTime
  let needsRebuild =
        case cacheTime of
          Left (_ :: SomeException) -> True
          Right t -> diffUTCTime currentTime t > (7.5 * nominalDay)
  when needsRebuild rebuildDB
  withDB $ \conn -> do
    downloadFeed conn (0.5 * nominalDay) "modified"
    cves <- getCVEs conn "chrome" "74.0.3729.108"
    forM_ cves $ \CVE {cveID, cveDescription} -> do
      TIO.putStrLn $ cveID <> " " <> cveDescription

-- | Update a feed if it's older than a maximum age and return the contents as
-- ByteString.
cacheFeedInFile :: MaxAge -> FeedID -> IO BSL.ByteString
cacheFeedInFile maxAge feed = do
  cacheDir <- getXdgDirectory XdgCache "nixpkgs-update/nvd"
  createDirectoryIfMissing True cacheDir
  let cacheFile = cacheDir </> feed <.> "json"
  cacheTime <- try $ getModificationTime cacheFile
  currentTime <- getCurrentTime
  let needsUpdate =
        case cacheTime of
          Left (_ :: IOError) -> True
          Right t -> diffUTCTime currentTime t > maxAge
  if needsUpdate
    then do
      putStrLn $ "updating feed " <> feed
      Meta _ expectedChecksum <- getMeta feed
      compressed <- simpleHttp $ feedURL feed ".json.gz"
      let raw = decompress compressed
      let actualChecksum = BSL.fromStrict $ hashlazy raw
      when (actualChecksum /= expectedChecksum) $
        throwString $
        "wrong hash, expected: " <> BSL.unpack (hex expectedChecksum) <>
        " got: " <>
        BSL.unpack (hex actualChecksum)
      BSL.writeFile cacheFile raw
      return raw
    else do
      BSL.readFile cacheFile
