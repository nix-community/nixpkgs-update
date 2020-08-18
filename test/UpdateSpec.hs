{-# LANGUAGE OverloadedStrings #-}

module UpdateSpec where

import qualified Data.Text.IO as T
import Test.Hspec
import qualified Update
import qualified Utils

main :: IO ()
main = hspec spec

-- Tests here will write their actual results to an actual*.md file next to the
-- expected one. If you are updating the PR description, run the test suite and
-- copy all the actuals over to expected. This can be viewed with `git diff`
-- then to ensure your changes look as expected.
spec :: Spec
spec = do
  describe "PR message" do
    -- Common mock options
    let options = Utils.Options False False "r-ryantm" "" False False False
    let updateEnv = Utils.UpdateEnv "foobar" "1.0" "1.1" (Just "https://update-site.com") options
    let isBroken = False
    let metaDescription = "\"Foobar package description\""
    let metaHomepage = "\"https://foobar-homepage.com\""
    let metaChangelog = "\"https://foobar-homepage.com/changelog/v1.2.3\""
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

    it "matches a simple mock example" do
      expected <- T.readFile "test_data/expected_pr_description_1.md"
      let actual = Update.prMessage updateEnv isBroken metaDescription metaHomepage metaChangelog rewriteMsgs releaseUrl compareUrl resultCheckReport commitHash attrPath maintainersCc resultPath opReport cveRep cachixTestInstructions nixpkgsReviewMsg
      T.writeFile "test_data/actual_pr_description_1.md" actual
      actual `shouldBe` expected

    it "does not include NixPkgs review section when no review was done" do
      expected <- T.readFile "test_data/expected_pr_description_2.md"
      let nixpkgsReviewMsg' = ""
      let actual = Update.prMessage updateEnv isBroken metaDescription metaHomepage metaChangelog rewriteMsgs releaseUrl compareUrl resultCheckReport commitHash attrPath maintainersCc resultPath opReport cveRep cachixTestInstructions nixpkgsReviewMsg'
      T.writeFile "test_data/actual_pr_description_2.md" actual
      actual `shouldBe` expected
