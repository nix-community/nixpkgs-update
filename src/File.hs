module File
  ( replace,
  )
where

import qualified Data.Text as T
import Data.Text.IO as T
import OurPrelude

replace :: MonadIO m => Text -> Text -> FilePath -> m ()
replace find r file =
  liftIO $ do
    contents <- T.readFile file
    T.writeFile file (T.replace find r contents)
