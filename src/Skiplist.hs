{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Skiplist
  ( packageName,
    content,
    attrPath,
    checkResult,
    python,
    skipOutpathCalc,
  )
where

import Data.Char (isDigit)
import Data.Foldable (find)
import qualified Data.Text as T
import OurPrelude
import Text.Regex.Applicative.Text (RE', few, psym, string, (=~))

type Skiplist = [(Text -> Bool, Text)]

type TextSkiplister m =
  (MonadError Text m) =>
  Text ->
  m ()

attrPath :: TextSkiplister m
attrPath = skiplister attrPathList

packageName :: TextSkiplister m
packageName name =
  if name == "elementary-xfce-icon-theme" -- https://github.com/ryantm/nixpkgs-update/issues/63
    then return ()
    else skiplister nameList name

content :: TextSkiplister m
content = skiplister contentList

checkResult :: TextSkiplister m
checkResult = skiplister checkResultList

skipOutpathCalc :: TextSkiplister m
skipOutpathCalc = skiplister skipOutpathCalcList

attrPathList :: Skiplist
attrPathList =
  [ prefix
      "lua"
      "Packages for lua are currently skipped. https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646",
    prefix "lxqt" "Packages for lxqt are currently skipped.",
    prefix
      "altcoins.bitcoin"
      "@roconnor asked for a skip on this until something can be done with GPG signatures https://github.com/NixOS/nixpkgs/commit/77f3ac7b7638b33ab198330eaabbd6e0a2e751a9",
    eq "sqlite-interactive" "it is an override",
    eq "harfbuzzFull" "it is an override",
    prefix
      "mate"
      "mate packages are upgraded in lockstep https://github.com/NixOS/nixpkgs/pull/50695#issuecomment-441338593",
    prefix
      "deepin"
      "deepin packages are upgraded in lockstep https://github.com/NixOS/nixpkgs/pull/52327#issuecomment-447684194",
    prefix
      "monero-"
      "monero-cli and monero-gui packages are upgraded in lockstep",
    prefix
      "element-desktop"
      "@Ma27 asked to skip",
    prefix
      "element-web"
      "has to be updated along with element-desktop",
    prefix
      "keybinder"
      "it has weird tags. see nixpkgs-update#232",
    infixOf
      "pysc2"
      "crashes nixpkgs-update",
    infixOf
      "tornado"
      "python updatescript updates pinned versions",
    prefix
      "spire-"
      "spire-server and spire-agent are different outputs for spire package",
    eq "imagemagick_light" "same file and version as imagemagick",
    eq "imagemagickBig" "same file and version as imagemagick",
    eq "libheimdal" "alias of heimdal",
    eq "minio_legacy_fs" "@bachp asked to skip",
    eq "flint" "update repeatedly exceeded the 6h timeout",
    eq "keepmenu" "update repeatedly exceeded the 6h timeout",
    eq "klee" "update repeatedly exceeded the 6h timeout",
    regex
      (string "python" *> few (psym isDigit) *> string "Packages.mmengine")
      "takes way too long to build",
    prefix
      "linuxKernel"
      "creates too many duplicate PRs",
    prefix
      "postgresql"
      "creates too many duplicate PRs",
    prefix
      -- bump this when the default version is changed
      "python312Packages"
      "isn't the default python version"
  ]

nameList :: Skiplist
nameList =
  [ prefix "r-" "we don't know how to find the attrpath for these",
    infixOf "jquery" "this isn't a real package",
    infixOf "google-cloud-sdk" "complicated package",
    infixOf "github-release" "complicated package",
    infixOf "perl" "currently don't know how to update perl",
    infixOf "cdrtools" "We keep downgrading this by accident.",
    infixOf "gst" "gstreamer plugins are kept in lockstep.",
    infixOf "electron" "multi-platform srcs in file.",
    infixOf
      "linux-headers"
      "Not updated until many packages depend on it (part of stdenv).",
    infixOf "xfce" "@volth asked to not update xfce",
    infixOf "cmake-cursesUI-qt4UI" "Derivation file is complicated",
    infixOf "iana-etc" "@mic92 takes care of this package",
    infixOf
      "checkbashism"
      "needs to be fixed, see https://github.com/NixOS/nixpkgs/pull/39552",
    eq "isl" "multi-version long building package",
    infixOf "qscintilla" "https://github.com/ryantm/nixpkgs-update/issues/51",
    eq "itstool" "https://github.com/NixOS/nixpkgs/pull/41339",
    infixOf
      "virtualbox"
      "nixpkgs-update cannot handle updating the guest additions https://github.com/NixOS/nixpkgs/pull/42934",
    eq
      "avr-binutils"
      "https://github.com/NixOS/nixpkgs/pull/43787#issuecomment-408649537",
    eq
      "iasl"
      "two updates had to be reverted, https://github.com/NixOS/nixpkgs/pull/46272",
    eq
      "meson"
      "https://github.com/NixOS/nixpkgs/pull/47024#issuecomment-423300633",
    eq
      "burp"
      "skipped until better versioning schema https://github.com/NixOS/nixpkgs/pull/46298#issuecomment-419536301",
    eq "chromedriver" "complicated package",
    eq
      "gitlab-shell"
      "@globin asked to skip in https://github.com/NixOS/nixpkgs/pull/52294#issuecomment-447653417",
    eq
      "gitlab-workhorse"
      "@globin asked to skip in https://github.com/NixOS/nixpkgs/pull/52286#issuecomment-447653409",
    eq
      "gitlab-elasticsearch-indexer"
      "@yayayayaka asked to skip in https://github.com/NixOS/nixpkgs/pull/244074#issuecomment-1641657015",
    eq "reposurgeon" "takes way too long to build",
    eq "kodelife" "multiple system hashes need to be updated at once",
    eq "openbazaar" "multiple system hashes need to be updated at once",
    eq "eaglemode" "build hangs or takes way too long",
    eq "autoconf" "@prusnak asked to skip",
    eq "abseil-cpp" "@andersk asked to skip"
  ]

contentList :: Skiplist
contentList =
  [ infixOf "nixpkgs-update: no auto update" "Derivation file opts-out of auto-updates",
    infixOf "DO NOT EDIT" "Derivation file says not to edit it",
    infixOf "Do not edit!" "Derivation file says not to edit it",
    -- Skip packages that have special builders
    infixOf "buildRustCrate" "Derivation contains buildRustCrate",
    infixOf "buildRubyGem" "Derivation contains buildRubyGem",
    infixOf "bundlerEnv" "Derivation contains bundlerEnv",
    infixOf "buildPerlPackage" "Derivation contains buildPerlPackage",
    -- Specific skips for classes of packages
    infixOf "teams.gnome.members" "Do not update GNOME during a release cycle",
    infixOf "https://downloads.haskell.org/ghc/" "GHC packages are versioned per file"
  ]

checkResultList :: Skiplist
checkResultList =
  [ infixOf
      "busybox"
      "- busybox result is not automatically checked, because some binaries kill the shell",
    infixOf
      "gjs"
      "- gjs result is not automatically checked, because some tests take a long time to run",
    infixOf
      "casperjs"
      "- casperjs result is not automatically checked, because some tests take a long time to run",
    binariesStickAround "kicad",
    binariesStickAround "fcitx",
    binariesStickAround "x2goclient",
    binariesStickAround "gpg-agent",
    binariesStickAround "dirmngr",
    binariesStickAround "barrier",
    binariesStickAround "fail2ban",
    binariesStickAround "zed",
    binariesStickAround "haveged"
  ]

skipOutpathCalcList :: Skiplist
skipOutpathCalcList =
  [ eq "firefox-beta-bin-unwrapped" "master",
    eq "firefox-devedition-bin-unwrapped" "master"
    -- "firefox-release-bin-unwrapped" is unneeded here because firefox-bin is a dependency of other packages that Hydra doesn't ignore.
  ]

binariesStickAround :: Text -> (Text -> Bool, Text)
binariesStickAround name =
  infixOf name ("- " <> name <> " result is not automatically checked because some binaries stick around")

skiplister :: Skiplist -> TextSkiplister m
skiplister skiplist input = forM_ result throwError
  where
    result = snd <$> find (\(isSkiplisted, _) -> isSkiplisted input) skiplist

prefix :: Text -> Text -> (Text -> Bool, Text)
prefix part reason = ((part `T.isPrefixOf`), reason)

infixOf :: Text -> Text -> (Text -> Bool, Text)
infixOf part reason = ((part `T.isInfixOf`), reason)

eq :: Text -> Text -> (Text -> Bool, Text)
eq part reason = ((part ==), reason)

regex :: RE' a -> Text -> (Text -> Bool, Text)
regex pat reason = (isJust . (=~ pat), reason)

python :: (Monad m) => Int -> Text -> ExceptT Text m ()
python numPackageRebuilds derivationContents =
  tryAssert
    ( "Python package with too many package rebuilds "
        <> (T.pack . show) numPackageRebuilds
        <> "  > "
        <> tshow maxPackageRebuild
    )
    (not isPython || numPackageRebuilds <= maxPackageRebuild)
  where
    isPython = "buildPythonPackage" `T.isInfixOf` derivationContents
    maxPackageRebuild = 100
