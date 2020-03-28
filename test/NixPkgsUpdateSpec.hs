module NixPkgsUpdateSpec (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hello world" $ do
    it "is alive" $ do
      2 + 2 `shouldBe` 4
