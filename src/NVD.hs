{-# LANGUAGE OverloadedStrings #-}

module NVD where

import OurPrelude

import Codec.Compression.GZip (decompress)
import Control.Exception (ioError, try)
import Crypto.Hash.SHA256 (hashlazy)
import CVE (CVE, parseFeed)
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Hex (hex, unhex)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock
  (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDay, utctDay)
import Data.Time.ISO8601 (parseISO8601)
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
  (XdgDirectory(..), createDirectoryIfMissing, getModificationTime,
  getXdgDirectory)
import System.FilePath ((<.>), (</>))
import System.IO.Error (userError)
import Utils (ProductID, Version)

-- | Either @recent@, @modified@, or any year since @2002@.
type FeedID = String
type Extension = String
type Timestamp = UTCTime
type Checksum = BSL.ByteString
type CompressedFeed = BSL.ByteString
type MaxAge = NominalDiffTime

data Meta = Meta Timestamp Checksum

feedURL :: FeedID -> Extension -> String
feedURL feed ext = "https://nvd.nist.gov/feeds/json/cve/1.0/nvdcve-1.0-" <> feed <> ext

throwString :: MonadIO m => String -> m a
throwString = liftIO . ioError . userError

throwText :: MonadIO m => Text -> m a
throwText = throwString . T.unpack

allYears :: IO [FeedID]
allYears = do
  now <- liftIO getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  return $ map show [2002..year]

parseMeta :: BSL.ByteString -> Either T.Text Meta
parseMeta raw = do
  let splitLine = second BSL.tail . BSL.break (==':') . BSL.takeWhile (/='\r')
  let fields = map splitLine $ BSL.lines raw
  lastModifiedDate <- note "no lastModifiedDate in meta" $ lookup "lastModifiedDate" fields
  sha256 <- note "no sha256 in meta" $ lookup "sha256" fields
  timestamp <- note "invalid lastModifiedDate in meta" $ parseISO8601 $ BSL.unpack lastModifiedDate
  checksum <- note "invalid sha256 in meta" $ unhex sha256
  return $ Meta timestamp checksum

getMeta :: MonadIO m => FeedID -> m Meta
getMeta feed = do
  raw <- simpleHttp $ feedURL feed ".meta"
  either throwText pure $ parseMeta raw

getCVEs :: (ProductID, Version) -> IO [CVE]
getCVEs (product, version) = do
  years <- allYears
  feeds <- sequence $ map (cacheFeed (7 * nominalDay)) years
  return []

updateVulnDB :: IO ()
updateVulnDB = do
  -- This will be enough to develop with.
  feed <- cacheFeed (99 * nominalDay) "2019"
  putStrLn $ "loading data"
  parsed <- either throwText pure $ parseFeed feed
  print $ head parsed
  -- putStrLn $ "checking feed cache"
  -- years <- allYears
  -- feeds <- sequence $ map (cacheFeed (7 * nominalDay)) years
  return ()

getCacheFile ::  MonadIO m => FeedID -> m FilePath
getCacheFile feed = do
  cacheDir <- liftIO $ getXdgDirectory XdgCache "nixpkgs-update/nvd"
  liftIO $ createDirectoryIfMissing True cacheDir
  return $ cacheDir </> feed <.> "json"

cacheFeed :: MonadIO m => MaxAge -> FeedID -> m BSL.ByteString
cacheFeed maxAge feed = do
  cacheFile <- getCacheFile feed
  cacheTime <- liftIO $ try $ getModificationTime cacheFile
  Meta newestTime expectedChecksum <- getMeta feed
  let needsUpdate = case cacheTime of
        Left (_ :: IOError) -> True
        Right t -> diffUTCTime newestTime t > maxAge

  if needsUpdate
    then do
      liftIO $ putStrLn $ "updating feed: " <> feed
      compressed <- simpleHttp $ feedURL feed ".json.gz"
      let raw = decompress compressed
      let actualChecksum = BSL.fromStrict $ hashlazy raw
      when (actualChecksum /= expectedChecksum) $ throwString $
        "wrong hash, expected: " <> BSL.unpack (hex expectedChecksum)
        <> " got: " <> BSL.unpack (hex actualChecksum)
      liftIO $ BSL.writeFile cacheFile raw
      return raw
    else do
      liftIO $ BSL.readFile cacheFile
