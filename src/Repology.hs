{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Repology where

import Data.Aeson
import Data.HashMap.Strict
import Data.List
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Client.TLS (newTlsManager)
import OurPrelude
import Servant.API
import Servant.Client (BaseUrl (..), ClientEnv (ClientEnv), ClientM, Scheme (..), client, runClientM)
import System.IO

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "repology.org" 443 "/api/v1"

type Metapackage = Vector Package

compareMetapackage :: Metapackage -> Metapackage -> Ordering
compareMetapackage ps1 ps2 = compareMetapackage' (ps1 V.!? 0) (ps2 V.!? 0)
  where
    compareMetapackage' (Just p1) (Just p2) = compare (name p1) (name p2)
    compareMetapackage' Nothing (Just _) = LT
    compareMetapackage' (Just _) Nothing = GT
    compareMetapackage' _ _ = EQ

type Metapackages = HashMap Text Metapackage

type API =
  "metapackage" :> Capture "metapackage_name" Text :> Get '[JSON] Metapackage :<|> "metapackages" :> QueryParam "search" Text :> QueryParam "maintainers" Text :> QueryParam "category" Text :> QueryParam "inrepo" Text :> QueryParam "outdated" Bool :> QueryParam "notinrepo" Text :> QueryParam "minspread" Integer :> QueryParam "maxspread" Integer :> Get '[JSON] Metapackages :<|> "metapackages" :> Capture "name" Text :> QueryParam "search" Text :> QueryParam "maintainers" Text :> QueryParam "category" Text :> QueryParam "inrepo" Text :> QueryParam "outdated" Bool :> QueryParam "notinrepo" Text :> QueryParam "minspread" Integer :> QueryParam "maxspread" Integer :> Get '[JSON] Metapackages

data Package = Package
  { repo :: Text,
    name :: Maybe Text,
    version :: Text,
    origversion :: Maybe Text,
    status :: Maybe Text,
    summary :: Maybe Text,
    categories :: Maybe (Vector Text),
    licenses :: Maybe (Vector Text),
    www :: Maybe (Vector Text),
    downloads :: Maybe (Vector Text)
  }
  deriving (Eq, Show, Generic, FromJSON)

api :: Proxy API
api = Proxy

metapackage :: Text -> ClientM (Vector Package)

metapackages ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  ClientM Metapackages

metapackages' ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  ClientM Metapackages
metapackage :<|> metapackages :<|> metapackages' = client api

-- type PagingResult = PagingResult (Vector Metapackage, ClientM PagingResult)
-- metapackages :: Text -> ClientM PagingResult
-- metapackages n = do
--   m <- ms n
--   return (lastMetapackageName m, sortedMetapackages m)
lastMetapackageName :: Metapackages -> Maybe Text
lastMetapackageName = keys >>> sort >>> Prelude.reverse >>> headMay

sortedMetapackages :: Metapackages -> Vector Metapackage
sortedMetapackages = elems >>> sortBy compareMetapackage >>> V.fromList

nixRepo :: Text
nixRepo = "nix_unstable"

nixOutdated :: ClientM Metapackages
nixOutdated =
  metapackages
    Nothing
    Nothing
    Nothing
    (Just nixRepo)
    (Just True)
    Nothing
    Nothing
    Nothing

nextNixOutdated :: Text -> ClientM Metapackages
nextNixOutdated n =
  metapackages'
    n
    Nothing
    Nothing
    Nothing
    (Just nixRepo)
    (Just True)
    Nothing
    Nothing
    Nothing

outdatedForRepo :: Text -> Vector Package -> Maybe Package
outdatedForRepo r =
  V.find (\p -> (status p) == Just "outdated" && (repo p) == r)

newest :: Vector Package -> Maybe Package
newest = V.find (\p -> (status p) == Just "newest")

dropMaybes :: [(Maybe Package, Maybe Package)] -> [(Package, Package)]
dropMaybes = Data.List.foldl' twoJusts []
  where
    twoJusts a (Just o, Just n) = (o, n) : a
    twoJusts a _ = a

getUpdateInfo :: ClientM (Maybe Text, Bool, Vector (Package, Package))
getUpdateInfo = do
  outdated <- nixOutdated
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
  let mLastName = lastMetapackageName outdated
  liftIO $ hPutStrLn stderr $ show mLastName
  liftIO $ hPutStrLn stderr $ show (length ms)
  return (mLastName, length ms /= 1, V.fromList nixNew)

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
getNextUpdateInfo ::
  Text -> ClientM (Maybe Text, Bool, Vector (Package, Package))
getNextUpdateInfo n = do
  outdated <- nextNixOutdated n
  let ms = elems outdated
  let nixPackages = fmap (outdatedForRepo nixRepo) ms
  let newestPackages = fmap newest ms
  let nixNew = dropMaybes (zip nixPackages newestPackages)
  let mLastName = lastMetapackageName outdated
  liftIO $ hPutStrLn stderr $ show mLastName
  liftIO $ hPutStrLn stderr $ show (length ms)
  return (mLastName, length ms /= 1, V.fromList nixNew)

repologyUrl :: Text -> Text
repologyUrl pname = "https://repology.org/metapackage/" <> pname <> "/versions"

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
updateInfo :: (Package, Package) -> Maybe Text
updateInfo (outdated, newestP) = do
  pname <- name outdated
  pure $ T.unwords [pname, version outdated, version newestP, repologyUrl pname]

justs :: Vector (Maybe a) -> Vector a
justs = V.concatMap (maybeToList >>> V.fromList)

moreNixUpdateInfo ::
  (Maybe Text, Vector (Package, Package)) ->
  ClientM (Vector (Package, Package))
moreNixUpdateInfo (Nothing, acc) = do
  (mLastName, moreWork, newNix) <- getUpdateInfo
  liftIO $
    V.sequence_ $
      fmap Data.Text.IO.putStrLn $
        justs $
          fmap updateInfo newNix
  if moreWork
    then moreNixUpdateInfo (mLastName, newNix V.++ acc)
    else return acc
moreNixUpdateInfo (Just pname, acc) = do
  (mLastName, moreWork, newNix) <- getNextUpdateInfo pname
  liftIO $
    V.sequence_ $
      fmap Data.Text.IO.putStrLn $
        justs $
          fmap updateInfo newNix
  if moreWork
    then moreNixUpdateInfo (mLastName, newNix V.++ acc)
    else return acc

allNixUpdateInfo :: ClientM (Vector (Package, Package))
allNixUpdateInfo = moreNixUpdateInfo (Nothing, V.empty)

fetch :: IO ()
fetch = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  liftIO $ hPutStrLn stderr "starting"
  manager' <- newTlsManager
  e <- runClientM allNixUpdateInfo (ClientEnv manager' baseUrl Nothing)
  case e of
    Left ce -> liftIO $ hPutStrLn stderr $ show ce
    Right _ -> liftIO $ hPutStrLn stderr $ "done"
  return ()
