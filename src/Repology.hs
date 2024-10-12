{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Repology where

import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay)
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
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import System.IO

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "repology.org" 443 "/api/v1"

rateLimit :: IO ()
rateLimit = threadDelay 2000000

type Project = Vector Package

-- compareProject :: Project -> Project -> Ordering
-- compareProject ps1 ps2 = compareProject' (ps1 V.!? 0) (ps2 V.!? 0)
--   where
--     compareProject' (Just p1) (Just p2) = compare (name p1) (name p2)
--     compareProject' Nothing (Just _) = LT
--     compareProject' (Just _) Nothing = GT
--     compareProject' _ _ = EQ

type Projects = HashMap Text Project

type API =
  "project" :> Capture "project_name" Text :> Get '[JSON] Project
    :<|> "projects" :> QueryParam "inrepo" Text :> QueryParam "outdated" Bool :> Get '[JSON] Projects
    :<|> "projects" :> Capture "name" Text :> QueryParam "inrepo" Text :> QueryParam "outdated" Bool :> Get '[JSON] Projects

data Package = Package
  { repo :: Text,
    srcname :: Maybe Text, -- corresponds to attribute path
    visiblename :: Text, -- corresponds to pname
    version :: Text,
    origversion :: Maybe Text,
    status :: Maybe Text,
    summary :: Maybe Text,
    categories :: Maybe (Vector Text),
    licenses :: Maybe (Vector Text)
  }
  deriving (Eq, Show, Generic, FromJSON)

api :: Proxy API
api = Proxy

project :: Text -> ClientM (Vector Package)
projects ::
  Maybe Text ->
  Maybe Bool ->
  ClientM Projects
projects' ::
  Text ->
  Maybe Text ->
  Maybe Bool ->
  ClientM Projects
project :<|> projects :<|> projects' = client api

-- type PagingResult = PagingResult (Vector Project, ClientM PagingResult)
-- projects :: Text -> ClientM PagingResult
-- projects n = do
--   m <- ms n
--   return (lastProjectName m, sortedProjects m)
lastProjectName :: Projects -> Maybe Text
lastProjectName = keys >>> sort >>> Prelude.reverse >>> headMay

-- sortedProjects :: Projects -> Vector Project
-- sortedProjects = elems >>> sortBy compareProject >>> V.fromList

nixRepo :: Text
nixRepo = "nix_unstable"

nixOutdated :: ClientM Projects
nixOutdated =
  projects
    (Just nixRepo)
    (Just True)

nextNixOutdated :: Text -> ClientM Projects
nextNixOutdated n =
  projects'
    n
    (Just nixRepo)
    (Just True)

outdatedForRepo :: Text -> Vector Package -> Maybe Package
outdatedForRepo r =
  V.find (\p -> (status p) == Just "outdated" && (repo p) == r)

newest :: Vector Package -> Maybe Package
newest = V.find (\p -> (status p) == Just "newest")

getUpdateInfo :: ClientM (Maybe Text, Bool, Vector (Text, (Package, Package)))
getUpdateInfo = do
  liftIO rateLimit
  outdated <- nixOutdated
  let nixNew = toList $ Data.HashMap.Strict.mapMaybe (liftA2 (liftA2 (,)) (outdatedForRepo nixRepo) newest) outdated
  let mLastName = lastProjectName outdated
  liftIO $ hPutStrLn stderr $ show mLastName
  liftIO $ hPutStrLn stderr $ show (size outdated)
  return (mLastName, size outdated /= 1, V.fromList nixNew)

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
getNextUpdateInfo ::
  Text -> ClientM (Maybe Text, Bool, Vector (Text, (Package, Package)))
getNextUpdateInfo n = do
  liftIO rateLimit
  outdated <- nextNixOutdated n
  let nixNew = toList $ Data.HashMap.Strict.mapMaybe (liftA2 (liftA2 (,)) (outdatedForRepo nixRepo) newest) outdated
  let mLastName = lastProjectName outdated
  liftIO $ hPutStrLn stderr $ show mLastName
  liftIO $ hPutStrLn stderr $ show (size outdated)
  return (mLastName, size outdated /= 1, V.fromList nixNew)

-- Argument should be the Repology identifier of the project, not srcname/attrPath or visiblename/pname.
repologyUrl :: Text -> Text
repologyUrl projectName = "https://repology.org/project/" <> projectName <> "/versions"

--  let sorted = sortBy (\(p1,_) (p2,_) -> compare (name p1) (name p2)) nixNew
updateInfo :: (Text, (Package, Package)) -> Maybe Text
updateInfo (projectName, (outdated, newestP)) = do
  attrPath <- srcname outdated
  pure $ T.unwords [attrPath, version outdated, version newestP, repologyUrl projectName]

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
    then moreNixUpdateInfo (mLastName, fmap snd newNix V.++ acc)
    else return acc
moreNixUpdateInfo (Just n, acc) = do
  (mLastName, moreWork, newNix) <- getNextUpdateInfo n
  liftIO $
    V.sequence_ $
      fmap Data.Text.IO.putStrLn $
        justs $
          fmap updateInfo newNix
  if moreWork
    then moreNixUpdateInfo (mLastName, fmap snd newNix V.++ acc)
    else return acc

allNixUpdateInfo :: ClientM (Vector (Package, Package))
allNixUpdateInfo = moreNixUpdateInfo (Nothing, V.empty)

fetch :: IO ()
fetch = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  liftIO $ hPutStrLn stderr "starting"
  manager' <- newTlsManager
  e <- runClientM allNixUpdateInfo (mkClientEnv manager' baseUrl)
  case e of
    Left ce -> liftIO $ hPutStrLn stderr $ show ce
    Right _ -> liftIO $ hPutStrLn stderr $ "done"
  return ()
