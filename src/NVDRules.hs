{-# LANGUAGE OverloadedStrings #-}

module NVDRules where

import CVE (CPE(..), CPEMatch(..), CVE(..))
import Utils (ProductID)

filter :: CVE -> CPEMatch -> ProductID -> Bool
filter _ cpeMatch "terraform"
  | cpeTargetSoftware (cpeMatchCPE cpeMatch) == Just "aws" = False
filter _cve _match _productID = True
