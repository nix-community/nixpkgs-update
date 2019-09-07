{-# LANGUAGE OverloadedStrings #-}

module DeleteMerged
  ( deleteDone
  ) where

import OurPrelude

import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified GH
import qualified Git

deleteDone :: Text -> IO ()
deleteDone githubToken = do
  result <-
    runExceptT $ do
      Git.fetch
      Git.cleanAndResetTo "master"
      refs <- ExceptT $ GH.closedAutoUpdateRefs githubToken
      V.sequence_
        (fmap (\r -> Git.deleteBranchEverywhere ("auto-update/" <> r)) refs)
  case result of
    Left e -> T.putStrLn e
    _ -> return ()
