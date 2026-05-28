{-# LANGUAGE OverloadedStrings #-}

module ForgeSpec where

import Forge
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseURLMaybe" do
    it "parses GitHub release downloads" do
      parseURLMaybe "https://github.com/blueman-project/blueman/releases/download/2.0.7/blueman-2.0.7.tar.xz"
        `shouldBe` Just (URLParts GitHub "https://github.com" "blueman-project" "blueman" "2.0.7")

    it "parses GitHub archives" do
      parseURLMaybe "https://github.com/arvidn/libtorrent/archive/libtorrent_1_1_11.tar.gz"
        `shouldBe` Just (URLParts GitHub "https://github.com" "arvidn" "libtorrent" "libtorrent_1_1_11")

    it "parses GitLab archives" do
      parseURLMaybe "https://gitlab.com/inkscape/lib2geom/-/archive/1.0/lib2geom-1.0.tar.gz"
        `shouldBe` Just (URLParts GitLab "https://gitlab.com" "inkscape" "lib2geom" "1.0")

    it "parses self-hosted GitLab archives" do
      parseURLMaybe "https://gitlab.gnome.org/GNOME/glib/-/archive/2.80.0/glib-2.80.0.tar.gz"
        `shouldBe` Just (URLParts GitLab "https://gitlab.gnome.org" "GNOME" "glib" "2.80.0")

    it "parses Codeberg archives" do
      parseURLMaybe "https://codeberg.org/dnkl/foot/archive/1.16.1.tar.gz"
        `shouldBe` Just (URLParts Gitea "https://codeberg.org" "dnkl" "foot" "1.16.1")

    it "parses Codeberg release downloads" do
      parseURLMaybe "https://codeberg.org/dnkl/foot/releases/download/1.16.1/foot-1.16.1.tar.gz"
        `shouldBe` Just (URLParts Gitea "https://codeberg.org" "dnkl" "foot" "1.16.1")

    it "parses SourceHut archives" do
      parseURLMaybe "https://git.sr.ht/~sircmpwn/hare/archive/0.24.0.tar.gz"
        `shouldBe` Just (URLParts SourceHut "https://git.sr.ht" "~sircmpwn" "hare" "0.24.0")

    it "returns Nothing for non-forge URLs" do
      parseURLMaybe "https://example.com/foo-1.0.tar.gz"
        `shouldBe` Nothing

  describe "compareUrl" do
    it "generates GitHub compare URLs" do
      compareUrl
        "https://github.com/owner/repo/archive/v1.0.tar.gz"
        "https://github.com/owner/repo/archive/v1.1.tar.gz"
        `shouldBe` Just "https://github.com/owner/repo/compare/v1.0...v1.1"

    it "generates GitLab compare URLs" do
      compareUrl
        "https://gitlab.com/inkscape/lib2geom/-/archive/1.0/lib2geom-1.0.tar.gz"
        "https://gitlab.com/inkscape/lib2geom/-/archive/1.1/lib2geom-1.1.tar.gz"
        `shouldBe` Just "https://gitlab.com/inkscape/lib2geom/-/compare/1.0...1.1"

    it "generates self-hosted GitLab compare URLs" do
      compareUrl
        "https://gitlab.gnome.org/GNOME/glib/-/archive/2.80.0/glib-2.80.0.tar.gz"
        "https://gitlab.gnome.org/GNOME/glib/-/archive/2.82.0/glib-2.82.0.tar.gz"
        `shouldBe` Just "https://gitlab.gnome.org/GNOME/glib/-/compare/2.80.0...2.82.0"

    it "generates Codeberg compare URLs" do
      compareUrl
        "https://codeberg.org/dnkl/foot/archive/1.16.1.tar.gz"
        "https://codeberg.org/dnkl/foot/archive/1.17.0.tar.gz"
        `shouldBe` Just "https://codeberg.org/dnkl/foot/compare/1.16.1...1.17.0"

    it "generates SourceHut compare URLs" do
      compareUrl
        "https://git.sr.ht/~sircmpwn/hare/archive/0.24.0.tar.gz"
        "https://git.sr.ht/~sircmpwn/hare/archive/0.25.0.tar.gz"
        `shouldBe` Just "https://git.sr.ht/~sircmpwn/hare/log/0.24.0..0.25.0"

    it "returns Nothing for non-forge URLs" do
      compareUrl
        "https://example.com/foo-1.0.tar.gz"
        "https://example.com/foo-1.1.tar.gz"
        `shouldBe` Nothing
