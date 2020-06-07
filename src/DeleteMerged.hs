{-# LANGUAGE OverloadedStrings #-}

module DeleteMerged
  ( deleteDone
  ) where

import OurPrelude

import qualified Data.Text.IO as T
import qualified GH
import GitHub.Data (Name, Owner)
import qualified Git

deleteDone :: Text -> Name Owner -> IO ()
deleteDone githubToken ghUser = do
  result <-
    runExceptT $ do
      Git.fetch
      Git.cleanAndResetTo "master"
      refs <- ExceptT $ GH.closedAutoUpdateRefs (GH.authFromToken githubToken) ghUser
      let branches = fmap (\r -> ("auto-update/" <> r)) refs
      liftIO $ Git.deleteBranchesEverywhere branches
  case result of
    Left e -> T.putStrLn e
    _ -> return ()
