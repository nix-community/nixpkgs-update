#!/usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# shellcheck source=setup-nixpkgs.sh
source "$SCRIPT_DIR/setup-nixpkgs.sh"

NIX_PATH=nixpkgs="$NIXPKGS"
export NIX_PATH

PACKAGE_NAME=$1
OLD_VERSION=$2
NEW_VERSION=$3

if [ "$#" -eq 4 ]
then
    OK_TO_PR_AT=$4
else
    OK_TO_PR_AT="0"
fi

BRANCH_NAME="auto-update/$1"

function cleanup {
    git reset --hard
    git checkout master
    git reset --hard upstream/master
    git branch -D "$BRANCH_NAME" || true
}

function error_exit {
    cleanup
    if ( true >&3 ) 2> /dev/null
    then
        echo "$(date -Iseconds) $1" >&3
    else
        echo "$(date -Iseconds) $1"
    fi
    exit 1
}

if ! [ "$(nix eval -f . "(builtins.compareVersions \"$NEW_VERSION\" \"$OLD_VERSION\")")" -eq  1 ];
then
    error_exit "$NEW_VERSION is not newer than $OLD_VERSION according to Nix"
fi

# Package blacklist
case "$PACKAGE_NAME" in
    *jquery*) error_exit "this isn't a real package";;
    *google-cloud-sdk*) error_exit "complicated package";;
    *github-release*) error_exit "complicated package";;
    *fcitx*) error_exit "gets stuck in daemons";;
    *fricas*) error_exit "gets stuck in emacs";;
    *libxc*) error_exit "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821";;
    *perl*) error_exit "currently don't know how to update perl";;
    *python*) error_exit "currently don't know how to update python";;
    *cdrtools*) error_exit "We keep downgrading this by accident.";;
    *gst*) error_exit "gstreamer plugins are kept in lockstep.";;
    *electron*) error_exit "multi-platform srcs in file.";;
    *linuxHeaders*) error_exit "Not updated until many packages depend on it (part of stdenv).";;
    *mpich*) error_exit "Reported on repology.org as mischaracterized newest version";;
    *xfce*) error_exit "@volth asked to not update xfce";;
    *) true;;
esac || error_exit "Package on blacklist."

git fetch --prune --multiple upstream origin

if git branch --remote | grep "origin/auto-update/${PACKAGE_NAME}"
then
    error_exit "Update branch already on origin."
fi

git reset --hard
git checkout master
git reset --hard upstream/master

# This is extremely slow but will give us better results
ATTR_PATH=$(nix-env -qa "$PACKAGE_NAME-$OLD_VERSION" -f . --attr-path | head -n1 | cut -d' ' -f1) || error_exit "nix-env -q failed to find package name with old version"

# Temporarily blacklist gnome sources for lockstep update
if nix eval -f . "pkgs.${ATTR_PATH}.src.urls" | grep "gnome"
then
    error_exit "Packages from gnome are currently blacklisted."
fi

# Temporarily blacklist lua packages at @teto's request
# https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646
if echo "$ATTR_PATH" | grep "^lua"
then
    error_exit "Packages for lua are currently blacklisted."
fi


DERIVATION_FILE=$(EDITOR="echo" nix edit "$ATTR_PATH" -f .) || error_exit "Couldn't find derivation file."

function error_cleanup {
    cleanup
    exit 1
}
trap error_cleanup ERR

(( $(grep -c "fetchurl {" "$DERIVATION_FILE") + $(grep -c "fetchgit {" "$DERIVATION_FILE") + $(grep -c "fetchFromGitHub {" "$DERIVATION_FILE") <= 1 )) || error_exit "More than one fetcher in $DERIVATION_FILE"

if grep -q "DO NOT EDIT" "$DERIVATION_FILE"
then
    error_exit "Derivation file says not to edit it."
fi

# Skip packages that have special builders
if grep -q "buildGoPackage" "$DERIVATION_FILE"
then
    error_exit "Derivation contains buildGoPackage."
fi
if grep -q "buildRustCrate" "$DERIVATION_FILE"
then
    error_exit "Derivation contains buildRustCrate."
fi
if grep -q "buildPythonPackage" "$DERIVATION_FILE"
then
    error_exit "Derivation contains buildPythonPackage."
fi
if grep -q "buildRubyGem" "$DERIVATION_FILE"
then
    error_exit "Derivation contains buildRubyGem."
fi
if grep -q "bundlerEnv" "$DERIVATION_FILE"
then
    error_exit "Derivation contains bundlerEnv."
fi
if grep -q "buildPerlPackage" "$DERIVATION_FILE"
then
    error_exit "Derivation contains buildPerlPackage."
fi

"$SCRIPT_DIR"/check-attrpath-version.sh "$ATTR_PATH" "$NEW_VERSION" || error_exit "Version in attr path $ATTR_PATH not compatible with $NEW_VERSION"

# Make sure it hasn't been updated on master
grep "$OLD_VERSION" "$DERIVATION_FILE" || error_exit "Old version not present in master derivation file."

# Make sure it hasn't been updated on staging
git reset --hard
git checkout staging
git reset --hard upstream/staging
grep "$OLD_VERSION" "$DERIVATION_FILE" || error_exit "Old version not present in staging derivation file."

git checkout "$(git merge-base upstream/master upstream/staging)"

git checkout -B "$BRANCH_NAME"
OLD_HASH=$(nix eval -f . --raw "pkgs.$ATTR_PATH.src.drvAttrs.outputHash" || error_exit "Couldn't find old output hash at ATTR_PATH.src.drvAttrs.outputHash.")

sed -i "s/${OLD_VERSION//\./\\.}/$NEW_VERSION/g" "$DERIVATION_FILE" || error_exit "Could not replace OLD_VERSION with NEW_VERSION."

NEW_HASH=$(nix-prefetch-url -A "$ATTR_PATH.src" || error_exit "Could not prefetch new version URL.")

if [ "$OLD_HASH" = "$NEW_HASH" ]
then
    error_exit "Hashes equal; no update necessary"
fi

sed -i "s/$OLD_HASH/$NEW_HASH/g" "$DERIVATION_FILE" || error_exit "Could not replace OLD_HASH with NEW_HASH."

rm -f result*

nix build -f . "$ATTR_PATH" || error_exit "nix build failed."

RESULT=$(readlink ./result || readlink ./result-bin || error_exit "Couldn't find result link.")



CHECK_RESULT="$("$SCRIPT_DIR"/check-result.sh "$RESULT" "$NEW_VERSION")"

MAINTAINERS=
if nix eval "(let pkgs = import ./. {}; in pkgs.$ATTR_PATH.meta.maintainers)" > /dev/null 2>&1
then
    maintainers=$(nix eval --raw '(let pkgs = import ./. {}; gh = m : m.github or ""; nonempty = s: s != ""; addat = s: "@"+s; in builtins.concatStringsSep " " (map addat (builtins.filter nonempty (map gh pkgs.'"${ATTR_PATH}"'.meta.maintainers))))')
    if [ -n "$maintainers" ]
    then
        MAINTAINERS="

cc $maintainers for review"
    fi
fi

git diff

COMMIT_MESSAGE="$ATTR_PATH: $OLD_VERSION -> $NEW_VERSION

Semi-automatic update generated by https://github.com/ryantm/nix-update tools. These checks were done:

- built on NixOS
$CHECK_RESULT"

git commit -am "$COMMIT_MESSAGE"

# Try to push it three times
function push() {
    if [[ -v DRY_RUN ]]
    then
        return 0
    else
        git push --set-upstream origin "$BRANCH_NAME" --force
    fi
}
push || push || push

PR_MESSAGE="$COMMIT_MESSAGE$MAINTAINERS"

if [[ -v DRY_RUN ]]
then
    true
else
    CURRENT=$(date +%s)
    if (( CURRENT < OK_TO_PR_AT ))
    then
        SLEEP_SECONDS=$(( OK_TO_PR_AT - CURRENT ))
        echo "Sleeping $SLEEP_SECONDS seconds."
        sleep "$SLEEP_SECONDS"
    fi

    hub pull-request -m "$PR_MESSAGE"
fi

git reset --hard
git checkout master
git reset --hard

exit 0
