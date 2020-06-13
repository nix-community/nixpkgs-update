{-# LANGUAGE OverloadedStrings #-}

module DeleteMerged
  ( deleteDone
  ) where

import OurPrelude

import qualified Data.Text.IO as T
import qualified GH
import GitHub.Data (Name, Owner)
import qualified Git

deleteDone :: Bool -> Text -> Name Owner -> IO ()
deleteDone delete githubToken ghUser = do
  result <-
    runExceptT $ do
      Git.fetch
      Git.cleanAndResetTo "master"
      refs <- ExceptT $ GH.closedAutoUpdateRefs (GH.authFromToken githubToken) ghUser
      let branches = fmap (\r -> ("auto-update/" <> r)) refs
      if delete
      then liftIO $ Git.deleteBranchesEverywhere branches
      else liftIO $ T.putStrLn $ tshow branches
  case result of
    Left e -> T.putStrLn e
    _ -> return ()
