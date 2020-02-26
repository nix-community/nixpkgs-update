module File
  ( replace,
  )
where

import qualified Data.Text as T
import Data.Text.IO as T
import OurPrelude
import Control.Monad (when)

replace :: MonadIO m => Text -> Text -> FilePath -> m Bool
replace find r file =
  liftIO $ do
    contents <- T.readFile file
    let newContents = T.replace find r contents
    when (contents /= newContents) $ do
      T.writeFile file  newContents
    return $ contents /= newContents
