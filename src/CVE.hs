{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CVE
  ( parseFeed
  , CVE(..)
  , cveMatcherList
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
import Data.Bifunctor (bimap, second)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text.Read (decimal)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (FromRow, SQLData, ToRow, field, fromRow, toRow)

import Utils (Boundary(..), ProductID, VersionMatcher(..))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type CVEID = Text

data CVE =
  CVE
    { cveID :: CVEID
    , cveYear :: Int
    , cveSubID :: Int
    , cveMatchers :: Map ProductID (Set VersionMatcher)
    , cveDescription :: Text
    , cveCPEs :: [CPE]
    , cvePublished :: UTCTime
    , cveLastModified :: UTCTime
    }
  deriving (Show)

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

data MatcherRow =
  MatcherRow CVEID ProductID VersionMatcher

instance ToRow MatcherRow where
  toRow (MatcherRow cveID productID matcher) = toRow (cveID, productID, matcher)

instance FromRow MatcherRow where
  fromRow = MatcherRow <$> field <*> field <*> field

cveMatcherList :: CVE -> [MatcherRow]
cveMatcherList CVE {cveID, cveMatchers} = do
  (productID, matchers) <- M.assocs cveMatchers
  matcher <- S.elems matchers
  return $ MatcherRow cveID productID matcher

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

parseAffects :: Object -> Parser (Map ProductID (Set VersionMatcher))
parseAffects o = do
  vendor <- o .: "vendor"
  vendorData <- vendor .: "vendor_data"
  fmap (M.fromListWith S.union . concat) $
    sequence $
    flip map vendorData $ \v -> do
      product_ <- v .: "product"
      productData <- product_ .: "product_data"
      sequence $
        flip map productData $ \p -> do
          productID <- p .: "product_name"
          version <- p .: "version"
          versionData <- version .: "version_data"
          matchers <-
            fmap S.fromList $
            sequence $
            flip map versionData $ \ver -> do
              value <- ver .: "version_value"
              affected :: Text <- ver .: "version_affected"
              case affected of
                "=" -> pure $ FuzzyMatcher value
                "<=" -> pure $ RangeMatcher Unbounded (Including value)
                _ -> fail $ "unknown version comparator: " <> show affected
          pure (productID, matchers)

-- TODO: We ignore update, edition and softwareEdition for now, but they might
-- be relevant.
cpeToMatcher :: CPE -> Map ProductID (Set VersionMatcher)
cpeToMatcher CPE {cpeProduct = Just p, cpeVersion = Just v, cpeMatcher = Just m} =
  M.singleton p $ S.fromList [FuzzyMatcher v, m]
cpeToMatcher CPE {cpeProduct = Just p, cpeVersion = Just v} =
  M.singleton p $ S.fromList [FuzzyMatcher v]
cpeToMatcher CPE {cpeProduct = Just p, cpeMatcher = Just m} =
  M.singleton p $ S.fromList [m]
cpeToMatcher _ = M.empty

cpeMatchers :: [CPE] -> Map ProductID (Set VersionMatcher)
cpeMatchers = M.unionsWith S.union . map cpeToMatcher

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
        (cveYear, cveSubID) <- eitherToParser $ cveIDNumbers cveID
        description <- cve .: "description"
        cveDescription <- parseDescription description
        affects <- cve .: "affects"
        affectMatchers <- parseAffects affects
        let cveMatchers =
              M.unionWith S.union affectMatchers (cpeMatchers cveCPEs)
        pure CVE {..}

instance ToRow CVE where
  toRow CVE {cveID, cveDescription, cvePublished, cveLastModified} =
    toRow (cveID, cveDescription, cvePublished, cveLastModified)

instance FromRow CVE where
  fromRow = do
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
          (Nothing, Just start) -> pure (Including start)
          (Just _, Just _) -> fail "multiple starts"
      endBoundary <-
        case (vEndIncluding, vEndExcluding) of
          (Nothing, Nothing) -> pure Unbounded
          (Just end, Nothing) -> pure (Including end)
          (Nothing, Just end) -> pure (Including end)
          (Just _, Just _) -> fail "multiple ends"
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
    "unexpected " <> T.unpack attribute <> ", expected " <> show expected <>
    ", got " <>
    show actual

-- TODO: Determine how nodes work exactly. What does AND mean and what does
-- vulnerable: false mean? For now, we assume everything with vulnerable: true
-- is vulnerable.
parseNode :: Object -> Parser [CPE]
parseNode node = do
  children <- node .:! "children"
  parseNode' children node

parseNode' :: (Maybe [Object]) -> Object -> Parser [CPE]
parseNode' Nothing node = do
  matches <- node .:! "cpe_match" .!= []
  pure $ filter cpeVulnerable matches
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
