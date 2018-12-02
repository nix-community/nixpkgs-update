module File
  ( replace
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Shelly
import Shelly (Sh, liftIO, toTextIgnore)

replaceIO :: Text -> Text -> FilePath -> IO ()
replaceIO find r file = do
  contents <- T.readFile file
  T.writeFile file (T.replace find r contents)

replace :: Text -> Text -> Shelly.FilePath -> Sh ()
replace find r file = liftIO $ replaceIO find r f
  where
    f = (T.unpack . toTextIgnore) file
