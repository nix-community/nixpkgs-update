{-# LANGUAGE OverloadedStrings #-}

module NVDRules where

import CVE (CPE(..), CPEMatch(..), CVE(..))
import Utils (Boundary(..), ProductID, VersionMatcher(..))

filter :: CVE -> CPEMatch -> ProductID -> Bool
filter _ cpeMatch "uzbl"
  | cpeMatchVersionMatcher cpeMatch ==
      RangeMatcher Unbounded (Including "2009.12.22") = False
filter _ cpeMatch "uzbl"
  | cpeMatchVersionMatcher cpeMatch ==
      RangeMatcher Unbounded (Including "2010.04.03") = False
filter _ cpeMatch "terraform"
  | cpeTargetSoftware (cpeMatchCPE cpeMatch) == Just "aws" = False
filter _cve _match _productID = True
