{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CVE
  ( parseFeed,
    CPE (..),
    CPEMatch (..),
    CPEMatchRow (..),
    cpeMatches,
    CVE (..),
    CVEID,
    cveLI,
  )
where

import Data.Aeson
  ( FromJSON,
    Key,
    Object,
    eitherDecode,
    parseJSON,
    withObject,
    (.!=),
    (.:),
    (.:!),
  )
import Data.Aeson.Types (Parser, prependFailure)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow)
import Database.SQLite.Simple.ToField (toField)
import OurPrelude
import Utils (Boundary (..), VersionMatcher (..))

type CVEID = Text

data CVE = CVE
  { cveID :: CVEID,
    cveCPEMatches :: [CPEMatch],
    cveDescription :: Text,
    cvePublished :: UTCTime,
    cveLastModified :: UTCTime
  }
  deriving (Show, Eq, Ord)

-- | cve list item
cveLI :: CVE -> Bool -> Text
cveLI c patched =
  "- ["
    <> cveID c
    <> "](https://nvd.nist.gov/vuln/detail/"
    <> cveID c
    <> ")"
    <> p
  where
    p =
      if patched
        then " (patched)"
        else ""

data CPEMatch = CPEMatch
  { cpeMatchCPE :: CPE,
    cpeMatchVulnerable :: Bool,
    cpeMatchVersionMatcher :: VersionMatcher
  }
  deriving (Show, Eq, Ord)

instance FromRow CPEMatch where
  fromRow = do
    cpeMatchCPE <- fromRow
    let cpeMatchVulnerable = True
    cpeMatchVersionMatcher <- field
    pure CPEMatch {..}

-- This decodes an entire CPE string and related attributes, but we only use
-- cpeVulnerable, cpeProduct, cpeVersion and cpeMatcher.
data CPE = CPE
  { cpePart :: (Maybe Text),
    cpeVendor :: (Maybe Text),
    cpeProduct :: (Maybe Text),
    cpeVersion :: (Maybe Text),
    cpeUpdate :: (Maybe Text),
    cpeEdition :: (Maybe Text),
    cpeLanguage :: (Maybe Text),
    cpeSoftwareEdition :: (Maybe Text),
    cpeTargetSoftware :: (Maybe Text),
    cpeTargetHardware :: (Maybe Text),
    cpeOther :: (Maybe Text)
  }
  deriving (Eq, Ord)

instance Show CPE where
  show
    CPE
      { cpePart,
        cpeVendor,
        cpeProduct,
        cpeVersion,
        cpeUpdate,
        cpeEdition,
        cpeLanguage,
        cpeSoftwareEdition,
        cpeTargetSoftware,
        cpeTargetHardware,
        cpeOther
      } =
      "CPE {"
        <> (intercalate ", " . concat)
          [ cpeField "part" cpePart,
            cpeField "vendor" cpeVendor,
            cpeField "product" cpeProduct,
            cpeField "version" cpeVersion,
            cpeField "update" cpeUpdate,
            cpeField "edition" cpeEdition,
            cpeField "language" cpeLanguage,
            cpeField "softwareEdition" cpeSoftwareEdition,
            cpeField "targetSoftware" cpeTargetSoftware,
            cpeField "targetHardware" cpeTargetHardware,
            cpeField "other" cpeOther
          ]
        <> "}"
      where
        cpeField :: (Show a) => String -> Maybe a -> [String]
        cpeField _ Nothing = []
        cpeField name (Just value) = [name <> " = " <> show value]

instance ToRow CPE where
  toRow
    CPE
      { cpePart,
        cpeVendor,
        cpeProduct,
        cpeVersion,
        cpeUpdate,
        cpeEdition,
        cpeLanguage,
        cpeSoftwareEdition,
        cpeTargetSoftware,
        cpeTargetHardware,
        cpeOther
      } =
      fmap -- There is no toRow instance for a tuple this large
        toField
        [ cpePart,
          cpeVendor,
          cpeProduct,
          cpeVersion,
          cpeUpdate,
          cpeEdition,
          cpeLanguage,
          cpeSoftwareEdition,
          cpeTargetSoftware,
          cpeTargetHardware,
          cpeOther
        ]

instance FromRow CPE where
  fromRow = do
    cpePart <- field
    cpeVendor <- field
    cpeProduct <- field
    cpeVersion <- field
    cpeUpdate <- field
    cpeEdition <- field
    cpeLanguage <- field
    cpeSoftwareEdition <- field
    cpeTargetSoftware <- field
    cpeTargetHardware <- field
    cpeOther <- field
    pure CPE {..}

-- | Parse a @description_data@ subtree and return the concatenation of the
-- english descriptions.
parseDescription :: Object -> Parser Text
parseDescription o = do
  dData <- o .: "description_data"
  descriptions <-
    fmap concat $
      sequence $
        flip map dData $
          \dDatum -> do
            value <- dDatum .: "value"
            lang :: Text <- dDatum .: "lang"
            pure $
              case lang of
                "en" -> [value]
                _ -> []
  pure $ T.intercalate "\n\n" descriptions

instance FromJSON CVE where
  parseJSON =
    withObject "CVE" $ \o -> do
      cve <- o .: "cve"
      meta <- cve .: "CVE_data_meta"
      cveID <- meta .: "ID"
      prependFailure (T.unpack cveID <> ": ") $ do
        cfgs <- o .: "configurations"
        cveCPEMatches <- parseConfigurations cfgs
        cvePublished <- o .: "publishedDate"
        cveLastModified <- o .: "lastModifiedDate"
        description <- cve .: "description"
        cveDescription <- parseDescription description
        pure CVE {..}

instance ToRow CVE where
  toRow CVE {cveID, cveDescription, cvePublished, cveLastModified} =
    toRow (cveID, cveDescription, cvePublished, cveLastModified)

instance FromRow CVE where
  fromRow = do
    let cveCPEMatches = []
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

instance FromJSON CPEMatch where
  parseJSON =
    withObject "CPEMatch" $ \o -> do
      t <- o .: "cpe23Uri"
      cpeMatchCPE <-
        case splitCPE t of
          [Just "cpe", Just "2.3", cpePart, cpeVendor, cpeProduct, cpeVersion, cpeUpdate, cpeEdition, cpeLanguage, cpeSoftwareEdition, cpeTargetSoftware, cpeTargetHardware, cpeOther] ->
            pure CPE {..}
          _ -> fail $ "unparsable cpe23Uri: " <> T.unpack t
      cpeMatchVulnerable <- o .: "vulnerable"
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
      cpeMatchVersionMatcher <-
        case (cpeVersion cpeMatchCPE, startBoundary, endBoundary) of
          (Just v, Unbounded, Unbounded) -> pure $ SingleMatcher v
          (Nothing, start, end) -> pure $ RangeMatcher start end
          _ ->
            fail
              ( "cpe_match has both version "
                  <> show (cpeVersion cpeMatchCPE)
                  <> " in cpe, and boundaries from "
                  <> show startBoundary
                  <> " to "
                  <> show endBoundary
              )
      pure (CPEMatch {..})

data CPEMatchRow
  = CPEMatchRow CVE CPEMatch

instance ToRow CPEMatchRow where
  toRow (CPEMatchRow CVE {cveID} CPEMatch {cpeMatchCPE, cpeMatchVersionMatcher}) =
    [toField $ Just cveID]
      ++ toRow cpeMatchCPE
      ++ [toField cpeMatchVersionMatcher]

instance FromRow CPEMatchRow where
  fromRow = do
    let cveCPEMatches = []
    let cveDescription = undefined
    let cvePublished = undefined
    let cveLastModified = undefined
    cveID <- field
    cpeM <- fromRow
    pure $ CPEMatchRow (CVE {..}) cpeM

cpeMatches :: [CVE] -> [CPEMatchRow]
cpeMatches = concatMap rows
  where
    rows cve = fmap (CPEMatchRow cve) (cveCPEMatches cve)

guardAttr :: (Eq a, FromJSON a, Show a) => Object -> Key -> a -> Parser ()
guardAttr object attribute expected = do
  actual <- object .: attribute
  unless (actual == expected) $
    fail $
      "unexpected "
        <> show attribute
        <> ", expected "
        <> show expected
        <> ", got "
        <> show actual

boundedMatcher :: VersionMatcher -> Bool
boundedMatcher (RangeMatcher Unbounded Unbounded) = False
boundedMatcher _ = True

-- Because complex boolean formulas can't be used to determine if a single
-- product/version is vulnerable, we simply use all leaves marked vulnerable.
parseNode :: Object -> Parser [CPEMatch]
parseNode node = do
  maybeChildren <- node .:! "children"
  case maybeChildren of
    Nothing -> do
      matches <- node .:! "cpe_match" .!= []
      pure $
        filter (cpeMatchVersionMatcher >>> boundedMatcher) $
          filter cpeMatchVulnerable matches
    Just children -> do
      fmap concat $ sequence $ map parseNode children

parseConfigurations :: Object -> Parser [CPEMatch]
parseConfigurations o = do
  guardAttr o "CVE_data_version" ("4.0" :: Text)
  nodes <- o .: "nodes"
  fmap concat $ sequence $ map parseNode nodes

parseFeed :: BSL.ByteString -> Either Text [CVE]
parseFeed = bimap T.pack cvefItems . eitherDecode

data CVEFeed = CVEFeed
  { cvefItems :: [CVE]
  }

instance FromJSON CVEFeed where
  parseJSON = withObject "CVEFeed" $ \o -> CVEFeed <$> o .: "CVE_Items"
