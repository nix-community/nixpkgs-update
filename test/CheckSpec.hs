module CheckSpec where

import qualified Check
import qualified Data.Text as T
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "version check" do
    let seaweedVersion234 = T.pack "version 30GB 2.34 linux amd64"
    it "finds the version when present" do
      Check.hasVersion (T.pack "The version is 2.34") (T.pack "2.34") `shouldBe` True
      Check.hasVersion (T.pack "The version is 2.34.") (T.pack "2.34") `shouldBe` True
      Check.hasVersion (T.pack "2.34 is the version") (T.pack "2.34") `shouldBe` True
      Check.hasVersion seaweedVersion234 (T.pack "2.34") `shouldBe` True

    it "doesn't produce false positives" do
      Check.hasVersion (T.pack "The version is 12.34") (T.pack "2.34") `shouldBe` False
      Check.hasVersion (T.pack "The version is 2.345") (T.pack "2.34") `shouldBe` False
      Check.hasVersion (T.pack "The version is 2.35") (T.pack "2.34") `shouldBe` False
      Check.hasVersion (T.pack "2.35 is the version") (T.pack "2.34") `shouldBe` False
      Check.hasVersion (T.pack "2.345 is the version") (T.pack "2.34") `shouldBe` False
      Check.hasVersion (T.pack "12.34 is the version") (T.pack "2.34") `shouldBe` False
      Check.hasVersion seaweedVersion234 (T.pack "2.35") `shouldBe` False

    it "negative lookahead construction" do
      Check.versionWithoutPath "/nix/store/z9l2xakz7cgw6yfh83nh542pvc0g4rkq-geeqie-2.0.1" (T.pack "2.0.1") `shouldBe` "(?<!\\Qz9l2xakz7cgw6yfh83nh542pvc0g4rkq-geeqie-\\E)\\Q2.0.1\\E"
      Check.versionWithoutPath "/nix/store/z9l2xakz7cgw6yfh83nh542pvc0g4rkq-abc" (T.pack "2.0.1") `shouldBe` "\\Q2.0.1\\E"
      Check.versionWithoutPath "/nix/store/z9l2xakz7cgw6yfh83nh542pvc0g4rkq-2.0.1-dev" (T.pack "2.0.1") `shouldBe` "(?<!\\Qz9l2xakz7cgw6yfh83nh542pvc0g4rkq-\\E)\\Q2.0.1\\E"
