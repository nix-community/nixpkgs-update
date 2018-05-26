{-# LANGUAGE OverloadedStrings #-}

module Blacklist
  ( packageName
  , content
  , url
  , attrPath
  , checkResult
  ) where

import Data.Foldable (find)
import Data.Text (Text)
import qualified Data.Text as T

url :: [(Text -> Bool, Text)]
url =
  [(("gnome" `T.isInfixOf`), "Packages from gnome are currently blacklisted.")]

attrPath :: [(Text -> Bool, Text)]
attrPath =
  [ (("lua" `T.isPrefixOf`), "Packages for lua are currently blacklisted.")
  , (("lxqt" `T.isPrefixOf`), "Packages for lxqt are currently blacklisted.")
  ]

packageName :: Text -> Maybe Text
packageName pn =
  snd <$> find (\(isBlacklisted, _) -> isBlacklisted pn) nameList

nameList :: [(Text -> Bool, Text)]
nameList =
  [ (("jquery" `T.isInfixOf`), "this isn't a real package")
  , (("google-cloud-sdk" `T.isInfixOf`), "complicated package")
  , (("github-release" `T.isInfixOf`), "complicated package")
  , (("fcitx" `T.isInfixOf`), "gets stuck in daemons")
  , ( ("libxc" `T.isInfixOf`)
    , "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821")
  , (("perl" `T.isInfixOf`), "currently don't know how to update perl")
  , (("python" `T.isInfixOf`), "currently don't know how to update python")
  , (("cdrtools" `T.isInfixOf`), "We keep downgrading this by accident.")
  , (("gst" `T.isInfixOf`), "gstreamer plugins are kept in lockstep.")
  , (("electron" `T.isInfixOf`), "multi-platform srcs in file.")
  , ( ("linux-headers" `T.isInfixOf`)
    , "Not updated until many packages depend on it (part of stdenv).")
  , ( ("mpich" `T.isInfixOf`)
    , "Reported on repology.org as mischaracterized newest version")
  , (("xfce" `T.isInfixOf`), "@volth asked to not update xfce")
  , (("cmake-cursesUI-qt4UI" `T.isInfixOf`), "Derivation file is complicated")
  , ( ("varnish" `T.isInfixOf`)
    , "Temporary blacklist because of multiple versions and slow nixpkgs update")
  , (("iana-etc" `T.isInfixOf`), "@mic92 takes care of this package")
  , ( ("checkbashism" `T.isInfixOf`)
    , "needs to be fixed, see https://github.com/NixOS/nixpkgs/pull/39552")
  , ((== "isl"), "multi-version long building package")
  , ((== "tokei"), "got stuck forever building with no CPU usage")
  , ( ("qscintilla" `T.isInfixOf`)
    , "https://github.com/ryantm/nixpkgs-update/issues/51")
  ]

content :: [(Text, Text)]
content =
  [ ("DO NOT EDIT", "Derivation file says not to edit it.")
  , ("Do not edit!", "Derivation file says not to edit it.")
    -- Skip packages that have special builders
  , ("buildGoPackage", "Derivation contains buildGoPackage.")
  , ("buildRustCrate", "Derivation contains buildRustCrate.")
  , ("buildPythonPackage", "Derivation contains buildPythonPackage.")
  , ("buildRubyGem", "Derivation contains buildRubyGem.")
  , ("bundlerEnv", "Derivation contains bundlerEnv.")
  , ("buildPerlPackage", "Derivation contains buildPerlPackage.")
  ]

checkResult :: Text -> Maybe Text
checkResult pn =
  snd <$> find (\(isBlacklisted, _) -> isBlacklisted pn) checkResultList

checkResultList :: [(Text -> Bool, Text)]
checkResultList =
  [ (("busybox" `T.isInfixOf`), "- busybox result is not automatically checked, because some binaries kill the shell") ]
