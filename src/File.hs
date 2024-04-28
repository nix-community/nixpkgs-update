{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module File where

import qualified Data.Text as T
import Data.Text.IO as T
import OurPrelude
import Polysemy.Input
import Polysemy.Output

data File m a where
  Read :: FilePath -> File m Text
  Write :: FilePath -> Text -> File m ()

makeSem ''File

runIO ::
  (Member (Embed IO) r) =>
  Sem (File ': r) a ->
  Sem r a
runIO =
  interpret $ \case
    Read file -> embed $ T.readFile file
    Write file contents -> embed $ T.writeFile file contents

runPure ::
  [Text] ->
  Sem (File ': r) a ->
  Sem r ([Text], a)
runPure contentList =
  runOutputMonoid pure
    . runInputList contentList
    . reinterpret2 \case
      Read _file -> maybe "" id <$> input
      Write _file contents -> output contents

replace ::
  (Member File r) =>
  Text ->
  Text ->
  FilePath ->
  Sem r Bool
replace find replacement file = do
  contents <- File.read file
  let newContents = T.replace find replacement contents
  when (contents /= newContents) $ do
    File.write file newContents
  return $ contents /= newContents

replaceIO :: (MonadIO m) => Text -> Text -> FilePath -> m Bool
replaceIO find replacement file =
  liftIO $
    runFinal $
      embedToFinal $
        runIO $
          (replace find replacement file)
