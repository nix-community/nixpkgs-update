{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CVE
  ( parseFeed
  , CVE
  ) where

import OurPrelude

import Data.Aeson
  ( FromJSON
  , Object
  , (.:)
  , (.:!)
  , eitherDecode
  , parseJSON
  , withObject
  , withText
  )
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Time.Clock (UTCTime)
import Utils (Boundary(..), ProductID, VersionMatcher(..))

data CVE =
  CVE
    { cveID :: Text
    , cveYear :: Int
    , cveSubID :: Int
    , cveAffects :: Map ProductID [VersionMatcher]
    , cveDescriptions :: Text
    , cveCPEs :: [CPE]
    , cvePublished :: UTCTime
    , cveLastModified :: UTCTime
    }
  deriving (Show)

data CPE =
  CPE
    { cpePart :: Text
    , cpeVendor :: Text
    , cpeProduct :: Text
    , cpeVersion :: Text
    , cpeUpdate :: Text
    , cpeEdition :: Text
    , cpeLanguage :: Text
    , cpeSoftwareEdition :: Text
    , cpeTargetSoftware :: Text
    , cpeTargetHardware :: Text
    , cpeOther :: Text
    }
  deriving (Show)

-- type CVEID = Text
--type CVEIndex = Map ProductID [(VersionMatcher, [CVEID])]
eitherToParser :: Either String a -> Parser a
eitherToParser (Left e) = fail e
eitherToParser (Right a) = pure a

cveIDNumbers :: Text -> Either String (Int, Int)
cveIDNumbers rest0 = do
  rest1 <- note "invalid cve id" $ T.stripPrefix "CVE-" rest0
  (year, rest2) <- decimal rest1
  rest3 <- note "invalid cve id" $ T.stripPrefix "-" rest2
  (subid, rest4) <- decimal rest3
  guard $ T.null rest4
  pure (year, subid)

parseDescriptions :: Object -> Parser Text
parseDescriptions o = do
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
  case descriptions of
    [d] -> pure d
    -- []  -> pure ""
    _ -> fail "multiple english descriptions"

-- foobar :: (FromJSON a) => [a] -> (a -> Parser [b]) -> Parser [b]
-- foobar l f = fmap concat $ sequence $ map f l
parseAffects :: Object -> Parser (Map ProductID [VersionMatcher])
parseAffects o = do
  vendor <- o .: "vendor"
  vendorData <- vendor .: "vendor_data"
  fmap (M.fromListWith (<>) . concat) $
    sequence $
    flip map vendorData $ \v -> do
      productPart <- v .: "product"
      productData <- productPart .: "product_data"
      sequence $
        flip map productData $ \p -> do
          productID <- p .: "product_name"
          version <- p .: "version"
          versionData <- version .: "version_data"
          matchers <-
            sequence $
            flip map versionData $ \ver -> do
              value <- ver .: "version_value"
              affected :: Text <- ver .: "version_affected"
              case affected of
                "=" -> pure $ FuzzyMatcher value
                "<=" -> pure $ RangeMatcher Unbounded (Including value)
                _ -> fail $ "unknown version comparator: " <> show affected
          pure (productID, matchers)

instance FromJSON CVE where
  parseJSON =
    withObject "CVE" $ \o -> do
      cve <- o .: "cve"
      cfgs <- o .: "configurations"
      cveCPEs <- parseConfigurations cfgs
      meta <- cve .: "CVE_data_meta"
      cveID <- meta .: "ID"
      cvePublished <- o .: "publishedDate"
      cveLastModified <- o .: "lastModifiedDate"
      (cveYear, cveSubID) <- eitherToParser $ cveIDNumbers cveID
      description <- cve .: "description"
      cveDescriptions <- parseDescriptions description
      affects <- cve .: "affects"
      cveAffects <- parseAffects affects
      pure CVE {..}

splitCPE :: Text -> [Text]
splitCPE = map (T.replace "\a" ":") . T.splitOn ":" . T.replace "\\:" "\a"

instance FromJSON CPE where
  parseJSON =
    withText "CPE" $ \t -> do
      case splitCPE t of
        ["cpe", "2.3", cpePart, cpeVendor, cpeProduct, cpeVersion, cpeUpdate, cpeEdition, cpeLanguage, cpeSoftwareEdition, cpeTargetSoftware, cpeTargetHardware, cpeOther] ->
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

-- TODO: Determine how nodes work exactly. What does AND mean and what does
-- vulnerable: false mean? For now, we assume everything with vulnerable: true
-- is vulnerable.
parseNode :: Object -> Parser [CPE]
parseNode node = do
  children <- node .:! "children"
  parseNode' children node

parseNode' :: (Maybe [Object]) -> Object -> Parser [CPE]
parseNode' Nothing node = do
  matches <- node .: "cpe_match"
  fmap concat $
    sequence $
    flip map matches $ \match -> do
      vulnerable <- match .: "vulnerable"
      cpe <- match .: "cpe23Uri"
      pure $
        if vulnerable
          then [cpe]
          else []
parseNode' (Just children) _ = do
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
