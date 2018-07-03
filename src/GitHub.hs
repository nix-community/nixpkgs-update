module GitHub where

import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Text (Text)
import GitHub.Auth (Auth(..))
import GitHub.Data.Definitions (Owner)
import GitHub.Data.Repos (Repo)
import GitHub.Data.Name (Name)
import GitHub.Data.Releases (releaseBody)
import GitHub.Endpoints.Repos.Releases (releaseByTagName)

changelog :: Name Owner -> Name Repo -> Text -> IO (Either Text Text)
changelog owner repo tag = bimap (T.pack . show) releaseBody <$> releaseByTagName owner repo tag
