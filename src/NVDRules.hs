{-# LANGUAGE OverloadedStrings #-}

module NVDRules where

import CVE (CPE (..), CPEMatch (..), CVE (..))
import Data.Char (isDigit)
import qualified Data.Text as T
import OurPrelude
import Text.Regex.Applicative.Text (RE', anySym, many, psym, (=~))
import Utils (Boundary (..), ProductID, Version, VersionMatcher (..))

-- Return False to discard CVE
filter :: CVE -> CPEMatch -> ProductID -> Version -> Bool
filter _ cpeMatch "socat" v
  | cpeUpdatePresentAndNotPartOfVersion cpeMatch v = False -- TODO consider if this rule should be applied to all packages
filter _ cpeMatch "uzbl" v
  | isNothing (v =~ yearRegex)
      && "2009.12.22"
        `anyVersionInfixOf` cpeMatchVersionMatcher cpeMatch =
      False
  | isNothing (v =~ yearRegex)
      && "2010.04.03"
        `anyVersionInfixOf` cpeMatchVersionMatcher cpeMatch =
      False
filter _ cpeMatch "go" v
  | "."
      `T.isInfixOf` v
      && "-"
        `anyVersionInfixOf` cpeMatchVersionMatcher cpeMatch =
      False
filter _ cpeMatch "terraform" _
  | cpeTargetSoftware (cpeMatchCPE cpeMatch) == Just "aws" = False
filter cve _ "tor" _
  | cveID cve == "CVE-2017-16541" = False
filter _ cpeMatch "arena" _
  | cpeVendor (cpeMatchCPE cpeMatch) == Just "rockwellautomation"
      || cpeVendor (cpeMatchCPE cpeMatch) == Just "openforis" =
      False
filter _ cpeMatch "thrift" _
  | cpeVendor (cpeMatchCPE cpeMatch) == Just "facebook" = False
filter _ cpeMatch "kanboard" _
  | cpeTargetSoftware (cpeMatchCPE cpeMatch) == Just "jenkins" = False
filter _cve _match _productID _version = True

anyVersionInfixOf :: Text -> VersionMatcher -> Bool
anyVersionInfixOf t (SingleMatcher v) = t `T.isInfixOf` v
anyVersionInfixOf t (RangeMatcher (Including v1) (Including v2)) =
  t `T.isInfixOf` v1 || t `T.isInfixOf` v2
anyVersionInfixOf t (RangeMatcher (Excluding v1) (Excluding v2)) =
  t `T.isInfixOf` v1 || t `T.isInfixOf` v2
anyVersionInfixOf t (RangeMatcher (Including v1) (Excluding v2)) =
  t `T.isInfixOf` v1 || t `T.isInfixOf` v2
anyVersionInfixOf t (RangeMatcher (Excluding v1) (Including v2)) =
  t `T.isInfixOf` v1 || t `T.isInfixOf` v2
anyVersionInfixOf t (RangeMatcher Unbounded (Including v)) = t `T.isInfixOf` v
anyVersionInfixOf t (RangeMatcher Unbounded (Excluding v)) = t `T.isInfixOf` v
anyVersionInfixOf t (RangeMatcher (Including v) Unbounded) = t `T.isInfixOf` v
anyVersionInfixOf t (RangeMatcher (Excluding v) Unbounded) = t `T.isInfixOf` v
anyVersionInfixOf _ (RangeMatcher Unbounded Unbounded) = False

-- Four digits at the start followed by any number of anything else
yearRegex :: RE' ()
yearRegex =
  void $
    psym isDigit <* psym isDigit <* psym isDigit <* psym isDigit <* many anySym

cpeUpdatePresentAndNotPartOfVersion :: CPEMatch -> Version -> Bool
cpeUpdatePresentAndNotPartOfVersion cpeMatch v =
  maybe
    False
    (\update -> not (update `T.isInfixOf` v))
    (cpeUpdate (cpeMatchCPE cpeMatch))
