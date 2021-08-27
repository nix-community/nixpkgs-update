{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RewriteSpec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified File
import OurPrelude
import qualified Polysemy.Error as Error
import qualified Polysemy.Output as Output
import qualified Process
import qualified Rewrite
import Test.Hspec
import qualified Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rewrite.quotedUrls" do
    it "quotes an unquoted meta.homepage URL" do
      nixQuotedHomepageBad <- T.readFile "test_data/quoted_homepage_bad.nix"
      nixQuotedHomepageGood <- T.readFile "test_data/quoted_homepage_good.nix"
      let options = Utils.Options False False "r-ryantm" "" False False False False
      let updateEnv = Utils.UpdateEnv "inadyn" "2.5" "2.6" Nothing options
      -- TODO test correct file is being read
      let rwArgs = Rewrite.Args updateEnv "inadyn" undefined undefined False
      (logs, (newContents, result)) <-
        ( runFinal
            . embedToFinal
            . Output.runOutputList
            . File.runPure [nixQuotedHomepageBad]
            . Process.runPure ["\"http://troglobit.com/project/inadyn/\""]
            . Error.errorToIOFinal
            $ Rewrite.quotedUrls rwArgs
          )
      T.putStrLn $ T.unlines logs
      head logs `shouldBe` "[quotedUrls]"
      result `shouldBe` Right (Just "Quoted meta.homepage for [RFC 45](https://github.com/NixOS/rfcs/pull/45)")
      head newContents `shouldBe` nixQuotedHomepageGood
