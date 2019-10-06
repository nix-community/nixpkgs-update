{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CVE
  ( parseFeed
  , CVE(..)
  , CVEID
  , cveMatcherList
  , cveLI
  ) where

import OurPrelude

import Data.Aeson
  ( FromJSON
  , Object
  , (.!=)
  , (.:)
  , (.:!)
  , eitherDecode
  , parseJSON
  , withObject
  )
import Data.Aeson.Types (Parser, prependFailure)
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow)

import Utils (Boundary(..), ProductID, VersionMatcher(..))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type CVEID = Text

data CVE =
  CVE
    { cveID :: CVEID
    , cveMatchers :: Map ProductID (Set VersionMatcher)
    , cveDescription :: Text
    , cvePublished :: UTCTime
    , cveLastModified :: UTCTime
    }
  deriving (Show, Eq, Ord)

-- | cve list item
cveLI :: CVE -> Text
cveLI c =
  "- [" <> cveID c <> "](https://nvd.nist.gov/vuln/detail/" <> cveID c <> ")"

-- This decodes an entire CPE string and related attributes, but we only use
-- cpeVulnerable, cpeProduct, cpeVersion and cpeMatcher.
data CPE =
  CPE
    { cpeVulnerable :: Bool
    , cpePart :: Maybe Text
    , cpeVendor :: Maybe Text
    , cpeProduct :: Maybe Text
    , cpeVersion :: Maybe Text
    , cpeUpdate :: Maybe Text
    , cpeEdition :: Maybe Text
    , cpeLanguage :: Maybe Text
    , cpeSoftwareEdition :: Maybe Text
    , cpeTargetSoftware :: Maybe Text
    , cpeTargetHardware :: Maybe Text
    , cpeOther :: Maybe Text
    , cpeMatcher :: Maybe VersionMatcher
    }

instance Show CPE where
  show CPE { cpePart
           , cpeVendor
           , cpeProduct
           , cpeVersion
           , cpeUpdate
           , cpeEdition
           , cpeLanguage
           , cpeSoftwareEdition
           , cpeTargetSoftware
           , cpeTargetHardware
           , cpeOther
           , cpeMatcher
           } =
    "CPE {" <>
    (intercalate ", " . concat)
      [ cpeField "part" cpePart
      , cpeField "vendor" cpeVendor
      , cpeField "product" cpeProduct
      , cpeField "version" cpeVersion
      , cpeField "update" cpeUpdate
      , cpeField "edition" cpeEdition
      , cpeField "language" cpeLanguage
      , cpeField "softwareEdition" cpeSoftwareEdition
      , cpeField "targetSoftware" cpeTargetSoftware
      , cpeField "targetHardware" cpeTargetHardware
      , cpeField "other" cpeOther
      , cpeField "matcher" cpeMatcher
      ] <>
    "}"
    where
      cpeField :: Show a => String -> Maybe a -> [String]
      cpeField _ Nothing = []
      cpeField name (Just value) = [name <> " = " <> show value]

-- | Parse a @description_data@ subtree and return the concatenation of the
-- english descriptions.
parseDescription :: Object -> Parser Text
parseDescription o = do
  dData <- o .: "description_data"
  descriptions <-
    fmap concat $
    sequence $
    flip map dData $ \dDatum -> do
      value <- dDatum .: "value"
      lang :: Text <- dDatum .: "lang"
      pure $
        case lang of
          "en" -> [value]
          _ -> []
  pure $ T.intercalate "\n\n" descriptions

cpeToMatcher :: CPE -> Map ProductID (Set VersionMatcher)
cpeToMatcher CPE {cpeProduct = Just p, cpeVersion = Just v, cpeMatcher = Just m} =
  M.singleton p $ S.fromList [SingleMatcher v, m]
cpeToMatcher CPE {cpeProduct = Just p, cpeVersion = Just v} =
  M.singleton p $ S.fromList [SingleMatcher v]
cpeToMatcher CPE {cpeProduct = Just p, cpeMatcher = Just m} =
  M.singleton p $ S.fromList [m]
cpeToMatcher _ = M.empty

cpeMatchers :: [CPE] -> Map ProductID (Set VersionMatcher)
cpeMatchers = M.unionsWith S.union . map cpeToMatcher

cveMatcherList :: CVE -> [(CVEID, ProductID, VersionMatcher)]
cveMatcherList CVE {cveID, cveMatchers} = do
  (productID, matchers) <- M.assocs cveMatchers
  matcher <- S.elems matchers
  return (cveID, productID, matcher)

instance FromJSON CVE where
  parseJSON =
    withObject "CVE" $ \o -> do
      cve <- o .: "cve"
      meta <- cve .: "CVE_data_meta"
      cveID <- meta .: "ID"
      prependFailure (T.unpack cveID <> ": ") $ do
        cfgs <- o .: "configurations"
        cveCPEs <- parseConfigurations cfgs
        cvePublished <- o .: "publishedDate"
        cveLastModified <- o .: "lastModifiedDate"
        description <- cve .: "description"
        cveDescription <- parseDescription description
        let cveMatchers = cpeMatchers cveCPEs
        pure CVE {..}

instance ToRow CVE where
  toRow CVE {cveID, cveDescription, cvePublished, cveLastModified} =
    toRow (cveID, cveDescription, cvePublished, cveLastModified)

instance FromRow CVE where
  fromRow = do
    let cveMatchers = M.empty
    cveID <- field
    cveDescription <- field
    cvePublished <- field
    cveLastModified <- field
    pure CVE {..}

splitCPE :: Text -> [Maybe Text]
splitCPE =
  map (toMaybe . T.replace "\a" ":") . T.splitOn ":" . T.replace "\\:" "\a"
  where
    toMaybe "*" = Nothing
    toMaybe x = Just x

instance FromJSON CPE where
  parseJSON =
    withObject "CPE" $ \o -> do
      t <- o .: "cpe23Uri"
      cpeVulnerable <- o .: "vulnerable"
      vStartIncluding <- o .:! "versionStartIncluding"
      vEndIncluding <- o .:! "versionEndIncluding"
      vStartExcluding <- o .:! "versionStartExcluding"
      vEndExcluding <- o .:! "versionEndExcluding"
      startBoundary <-
        case (vStartIncluding, vStartExcluding) of
          (Nothing, Nothing) -> pure Unbounded
          (Just start, Nothing) -> pure (Including start)
          (Nothing, Just start) -> pure (Excluding start)
          (Just _, Just _) -> fail "multiple version starts"
      endBoundary <-
        case (vEndIncluding, vEndExcluding) of
          (Nothing, Nothing) -> pure Unbounded
          (Just end, Nothing) -> pure (Including end)
          (Nothing, Just end) -> pure (Excluding end)
          (Just _, Just _) -> fail "multiple version ends"
      let cpeMatcher =
            case (startBoundary, endBoundary) of
              (Unbounded, Unbounded) -> Nothing
              (start, end) -> Just (RangeMatcher start end)
      case splitCPE t of
        [Just "cpe", Just "2.3", cpePart, cpeVendor, cpeProduct, cpeVersion, cpeUpdate, cpeEdition, cpeLanguage, cpeSoftwareEdition, cpeTargetSoftware, cpeTargetHardware, cpeOther] ->
          pure CPE {..}
        _ -> fail $ "unparsable CPE: " <> T.unpack t

guardAttr :: (Eq a, FromJSON a, Show a) => Object -> Text -> a -> Parser ()
guardAttr object attribute expected = do
  actual <- object .: attribute
  unless (actual == expected) $
    fail $
    "unexpected " <>
    T.unpack attribute <>
    ", expected " <> show expected <> ", got " <> show actual

-- Because complex boolean formulas can't be used to determine if a single
-- product/version is vulnerable, we simply use all leaves marked vulnerable.
parseNode :: Object -> Parser [CPE]
parseNode node = do
  maybeChildren <- node .:! "children"
  case maybeChildren of
    Nothing -> do
      matches <- node .:! "cpe_match" .!= []
      pure $ filter cpeVulnerable matches
    Just children -> do
      fmap concat $ sequence $ map parseNode children

parseConfigurations :: Object -> Parser [CPE]
parseConfigurations o = do
  guardAttr o "CVE_data_version" ("4.0" :: Text)
  nodes <- o .: "nodes"
  fmap concat $ sequence $ map parseNode nodes

parseFeed :: BSL.ByteString -> Either Text [CVE]
parseFeed = bimap T.pack cvefItems . eitherDecode

data CVEFeed =
  CVEFeed
    { cvefItems :: [CVE]
    }

instance FromJSON CVEFeed where
  parseJSON = withObject "CVEFeed" $ \o -> CVEFeed <$> o .: "CVE_Items"
