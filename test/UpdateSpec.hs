{-# LANGUAGE OverloadedStrings #-}

module UpdateSpec where

import qualified Data.Text.IO as T
import Test.Hspec
import qualified Update
import qualified Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PR message" do
    it "matches a simple mock example" do
      expected <- T.readFile "test_data/expected_pr_description_1.md"
      let options = Utils.Options False False "" False False False False
      let updateEnv = Utils.UpdateEnv "foobar" "1.0" "1.1" (Just "https://update-site.com") options
      let isBroken = False
      let metaDescription = "\"Foobar package description\""
      let metaHomepage = "\"https://foobar-homepage.com\""
      let rewriteMsgs = ["Version Update", "Other Update"]
      let releaseUrl = "https://github.com/foobar/releases"
      let compareUrl = "https://github.com/foobar/compare"
      let resultCheckReport = "- Some other check"
      let commitHash = "af39cf77a0d42a4f6771043ec54221ed"
      let attrPath = "foobar"
      let maintainersCc = "@maintainer1"
      let resultPath = "/nix/store/some-hash-path"
      let opReport = "123 total rebuild path(s)"
      let cveRep = ""
      let cachixTestInstructions = ""
      let nixpkgsReviewMsg = "nixpkgs-review comment body"
      let actual = Update.prMessage updateEnv isBroken metaDescription metaHomepage rewriteMsgs releaseUrl compareUrl resultCheckReport commitHash attrPath maintainersCc resultPath opReport cveRep cachixTestInstructions nixpkgsReviewMsg
      actual `shouldBe` expected
