{-# LANGUAGE OverloadedStrings #-}

module DeleteMerged
  ( deleteDone
  ) where

import OurPrelude

import qualified Data.Text.IO as T
import qualified GH
import qualified Git

deleteDone :: Text -> IO ()
deleteDone githubToken = do
  result <-
    runExceptT $ do
      Git.fetch
      Git.cleanAndResetTo "master"
      refs <- ExceptT $ GH.closedAutoUpdateRefs githubToken
      let branches = fmap (\r -> ("auto-update/" <> r)) refs
      liftIO $ Git.deleteBranchesEverywhere branches
  case result of
    Left e -> T.putStrLn e
    _ -> return ()
