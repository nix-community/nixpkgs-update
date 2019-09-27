{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Blacklist
  ( packageName
  , content
  , srcUrl
  , attrPath
  , checkResult
  , python
  ) where

import OurPrelude

import Data.Foldable (find)
import qualified Data.Text as T

type Blacklist = [(Text -> Bool, Text)]

type TextBlacklister m
   = (MonadError Text m) =>
       Text -> m ()

srcUrl :: TextBlacklister m
srcUrl = blacklister srcUrlList

attrPath :: TextBlacklister m
attrPath = blacklister attrPathList

packageName :: TextBlacklister m
packageName name =
  if name == "elementary-xfce-icon-theme" -- https://github.com/ryantm/nixpkgs-update/issues/63
    then return ()
    else blacklister nameList name

content :: TextBlacklister m
content = blacklister contentList

checkResult :: TextBlacklister m
checkResult = blacklister checkResultList

srcUrlList :: Blacklist
srcUrlList = []

attrPathList :: Blacklist
attrPathList =
  [ prefix
      "lua"
      "Packages for lua are currently blacklisted. https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646"
  , prefix "lxqt" "Packages for lxqt are currently blacklisted."
  , prefix
      "altcoins.bitcoin"
      "@roconnor asked for a blacklist on this until something can be done with GPG signatures https://github.com/NixOS/nixpkgs/commit/77f3ac7b7638b33ab198330eaabbd6e0a2e751a9"
  , eq "sqlite-interactive" "it is an override"
  , eq "harfbuzzFull" "it is an override"
  , prefix
      "mate"
      "mate packages are upgraded in lockstep https://github.com/NixOS/nixpkgs/pull/50695#issuecomment-441338593"
  , prefix
      "deepin"
      "deepin packages are upgraded in lockstep https://github.com/NixOS/nixpkgs/pull/52327#issuecomment-447684194"
  , prefix "gnome" "don't update gnome during a release cycle"
  ]

nameList :: Blacklist
nameList =
  [ prefix "r-" "we don't know how to find the attrpath for these"
  , infixOf "jquery" "this isn't a real package"
  , infixOf "google-cloud-sdk" "complicated package"
  , infixOf "github-release" "complicated package"
  , infixOf
      "libxc"
      "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821"
  , infixOf "perl" "currently don't know how to update perl"
  , infixOf "cdrtools" "We keep downgrading this by accident."
  , infixOf "gst" "gstreamer plugins are kept in lockstep."
  , infixOf "electron" "multi-platform srcs in file."
  , infixOf
      "linux-headers"
      "Not updated until many packages depend on it (part of stdenv)."
  , infixOf "xfce" "@volth asked to not update xfce"
  , infixOf "cmake-cursesUI-qt4UI" "Derivation file is complicated"
  , infixOf "iana-etc" "@mic92 takes care of this package"
  , infixOf
      "checkbashism"
      "needs to be fixed, see https://github.com/NixOS/nixpkgs/pull/39552"
  , eq "isl" "multi-version long building package"
  , infixOf "qscintilla" "https://github.com/ryantm/nixpkgs-update/issues/51"
  , eq "itstool" "https://github.com/NixOS/nixpkgs/pull/41339"
  , infixOf
      "virtualbox"
      "nixpkgs-update cannot handle updating the guest additions https://github.com/NixOS/nixpkgs/pull/42934"
  , eq
      "avr-binutils"
      "https://github.com/NixOS/nixpkgs/pull/43787#issuecomment-408649537"
  , eq
      "iasl"
      "two updates had to be reverted, https://github.com/NixOS/nixpkgs/pull/46272"
  , eq
      "meson"
      "https://github.com/NixOS/nixpkgs/pull/47024#issuecomment-423300633"
  , eq
      "burp"
      "temporary blacklist until better versioning schema https://github.com/NixOS/nixpkgs/pull/46298#issuecomment-419536301"
  , eq "chromedriver" "complicated package"
  , eq
      "gitlab-shell"
      "@globin asked to black list in https://github.com/NixOS/nixpkgs/pull/52294#issuecomment-447653417"
  , eq
      "gitlab-workhorse"
      "@globin asked to black list in https://github.com/NixOS/nixpkgs/pull/52286#issuecomment-447653409"
  ]

contentList :: Blacklist
contentList =
  [ infixOf "DO NOT EDIT" "Derivation file says not to edit it."
  , infixOf "Do not edit!" "Derivation file says not to edit it."
    -- Skip packages that have special builders
  , infixOf "buildGoPackage" "Derivation contains buildGoPackage."
  , infixOf "buildRustCrate" "Derivation contains buildRustCrate."
  --, infixOf "buildPythonPackage" "Derivation contains buildPythonPackage." -- FRidh says lets' try python
  , infixOf "buildRubyGem" "Derivation contains buildRubyGem."
  , infixOf "bundlerEnv" "Derivation contains bundlerEnv."
  , infixOf "buildPerlPackage" "Derivation contains buildPerlPackage."
  , infixOf
      "nixpkgs-update: no auto update"
      "Derivation file asks not to auto update it"
  , infixOf "gnome" "don't update gnome during a release cycle"
  ]

checkResultList :: Blacklist
checkResultList =
  [ infixOf
      "busybox"
      "- busybox result is not automatically checked, because some binaries kill the shell"
  , infixOf
      "fcitx"
      "- fcitx result is not automatically checked, because some binaries gets stuck in daemons"
  , infixOf
      "x2goclient"
      "- x2goclient result is not automatically checked, because some binaries don't timeout properly"
  , infixOf
      "kicad"
      "- kicad result is not automatically checekd, because some binaries don't timeout properly"
  ]

blacklister :: Blacklist -> TextBlacklister m
blacklister blacklist input = forM_ result throwError
  where
    result = snd <$> find (\(isBlacklisted, _) -> isBlacklisted input) blacklist

prefix :: Text -> Text -> (Text -> Bool, Text)
prefix part reason = ((part `T.isPrefixOf`), reason)

infixOf :: Text -> Text -> (Text -> Bool, Text)
infixOf part reason = ((part `T.isInfixOf`), reason)

eq :: Text -> Text -> (Text -> Bool, Text)
eq part reason = ((part ==), reason)

python :: Monad m => Int -> Text -> ExceptT Text m ()
python numPackageRebuilds derivationContents =
  tryAssert
    ("Python package with too many package rebuilds " <>
     (T.pack . show) numPackageRebuilds <>
     "  > 10")
    (not isPython || numPackageRebuilds <= 10)
  where
    isPython = "buildPythonPackage" `T.isInfixOf` derivationContents
