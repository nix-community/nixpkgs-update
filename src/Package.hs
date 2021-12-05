{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Package where

import OurPrelude

import qualified Database.SQLite.Simple as SQL
import qualified Utils

data Package = Package
  { attrPath :: Text
  , targetVersion :: Utils.Version
  , tried :: Bool
  }
  deriving (Show, Eq, Ord)

instance SQL.FromRow Package where
  fromRow = do
    attrPath <- SQL.field
    targetVersion <- SQL.field
    tried <- SQL.field
    pure Package {..}

getDBPath :: IO FilePath
getDBPath = do
  cacheDir <- Utils.cacheDir
  pure $ cacheDir </> "package.sqlite3"

withDB :: (SQL.Connection -> IO a) -> IO a
withDB action = do
  dbPath <- getDBPath
  SQL.withConnection dbPath action
