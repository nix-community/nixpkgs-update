{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RewriteSpec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified File
import OurPrelude
import qualified Rewrite
import Test.Hspec
import Text.RawString.QQ (r)
import qualified Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Hello world" do
    it "is alive" do
      2 + 2 `shouldBe` 4
