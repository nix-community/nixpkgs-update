{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

import Test.Hspec
import qualified Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let options = Utils.Options False False "" "" False False False False
  let updateEnv = Utils.UpdateEnv "foobar" "1.0" "1.1" (Just "https://update-site.com") options
  describe "PR title" do
    -- This breaks IRC when it tries to link to newly opened pull requests
    it "should not include a trailing newline" do
      let title = Utils.prTitle updateEnv "python37Packages.foobar"
      title `shouldBe` "python37Packages.foobar: 1.0 -> 1.1"

  describe "titleVersion" do
    it "should parse prTitle output" do
      let title = Utils.prTitle updateEnv "python37Packages.foobar"
      let version = Utils.titleVersion title
      version `shouldBe` Just "1.1"

    it "should fail on unexpected commit messages" do
      let version = Utils.titleVersion "not a prTitle-style commit message"
      version `shouldBe` Nothing
