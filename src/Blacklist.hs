{-# LANGUAGE OverloadedStrings #-}

module Blacklist
  ( packageName
  , content
  , srcUrl
  , attrPath
  , checkResult
  ) where

import Data.Foldable (find)
import Data.Text (Text)
import qualified Data.Text as T

type Blacklist = [(Text -> Bool, Text)]

srcUrl :: Text -> Either Text ()
srcUrl = blacklister srcUrlList

attrPath :: Text -> Either Text ()
attrPath = blacklister attrPathList

packageName :: Text -> Either Text ()
packageName name =
  if (name == "elementary-xfce-icon-theme") -- https://github.com/ryantm/nixpkgs-update/issues/63
    then Right ()
    else blacklister nameList name

content :: Text -> Either Text ()
content = blacklister contentList

checkResult :: Text -> Either Text ()
checkResult = blacklister checkResultList

srcUrlList :: Blacklist
srcUrlList =
  [(("gnome" `T.isInfixOf`), "Packages from gnome are currently blacklisted.")]

attrPathList :: Blacklist
attrPathList =
  [ prefix
      "lua"
      "Packages for lua are currently blacklisted. https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646"
  , prefix "lxqt" "Packages for lxqt are currently blacklisted."
  , prefix
      "altcoins.bitcoin-xt"
      "nix-prefetch-url has infinite redirect https://github.com/NixOS/nix/issues/2225 remove after Nix upgrade that includes https://github.com/NixOS/nix/commit/b920b908578d68c7c80f1c1e89c42784693e18d5."
  , prefix
      "altcoins.bitcoin"
      "@roconnor asked for a blacklist on this until something can be done with GPG signatures https://github.com/NixOS/nixpkgs/commit/77f3ac7b7638b33ab198330eaabbd6e0a2e751a9"
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
  , infixOf "python" "currently don't know how to update python"
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
  , eq
      "wire-desktop"
      "nixpkgs-update cannot handle this derivation https://github.com/NixOS/nixpkgs/pull/42936#issuecomment-402282692"
  , infixOf
      "virtualbox"
      "nixpkgs-update cannot handle updating the guest additions https://github.com/NixOS/nixpkgs/pull/42934"
  , eq "avr-binutils" "https://github.com/NixOS/nixpkgs/pull/43787#issuecomment-408649537"
  ]

contentList :: Blacklist
contentList =
  [ infixOf "DO NOT EDIT" "Derivation file says not to edit it."
  , infixOf "Do not edit!" "Derivation file says not to edit it."
    -- Skip packages that have special builders
  , infixOf "buildGoPackage" "Derivation contains buildGoPackage."
  , infixOf "buildRustCrate" "Derivation contains buildRustCrate."
  , infixOf "buildPythonPackage" "Derivation contains buildPythonPackage."
  , infixOf "buildRubyGem" "Derivation contains buildRubyGem."
  , infixOf "bundlerEnv" "Derivation contains bundlerEnv."
  , infixOf "buildPerlPackage" "Derivation contains buildPerlPackage."
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
  ]

blacklister :: Blacklist -> Text -> Either Text ()
blacklister blacklist input =
  case result of
    Nothing -> Right ()
    Just msg -> Left msg
  where
    result = snd <$> find (\(isBlacklisted, _) -> isBlacklisted input) blacklist

prefix :: Text -> Text -> (Text -> Bool, Text)
prefix part reason = ((part `T.isPrefixOf`), reason)

infixOf :: Text -> Text -> (Text -> Bool, Text)
infixOf part reason = ((part `T.isInfixOf`), reason)

eq :: Text -> Text -> (Text -> Bool, Text)
eq part reason = ((part ==), reason)
