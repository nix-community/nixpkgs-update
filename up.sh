#! /usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/utils.sh

NIX_PATH=nixpkgs="$(pwd)"
export NIX_PATH

PACKAGE_NAME=$1
OLD_VERSION=$2
NEW_VERSION=$3

BRANCH_NAME="auto-update/$1"

# Package blacklist
case "$PACKAGE_NAME" in
    *jquery*) false;; # this isn't a real package
    *google-cloud-sdk*) false;; # complicated package
    *github-release*) false;; # complicated package
    *fcitx*) false;; # gets stuck in daemons
    *fricas*) false;; # gets stuck in emacs
    *libxc*) false;; # currently people don't want to update this
    *) true;;
esac || error_exit "Package on blacklist."

# Temporarily blacklist gnome sources while a major coordinated update is being made
if nix eval -f . "pkgs.${ATTR_PATH}.src.urls" | grep "gnome"
then
    error_exit "Packages from gnome are currently blacklisted."
fi

if git branch --remote | grep "origin/${BRANCH_NAME}"
then
    error_exit "Update branch already on origin."
fi

git reset --hard
git checkout master
git reset --hard upstream/master

# This is extremely slow but will give us better results
ATTR_PATH=$(nix-env -qa "$PACKAGE_NAME-$OLD_VERSION" -f . --attr-path | head -n1 | cut -d' ' -f1)

function error_cleanup {
    cleanup
    exit 1
}
trap error_cleanup ERR

if [ -x $SCRIPT_DIR/updaters/$ATTR_PATH.sh ]
then
    $SCRIPT_DIR/updaters/$ATTR_PATH.sh $ATTR_PATH $BRANCH_NAME $@
elif [ "$(nix-instantiate --eval -E "with import ./. {}; builtins.hasAttr \"updateScript\" $ATTR_PATH")" = true ]
then
    $SCRIPT_DIR/updaters/update-script.sh $ATTR_PATH $BRANCH_NAME $@
else
    $SCRIPT_DIR/updaters/version.sh $ATTR_PATH $BRANCH_NAME $@
fi

git reset --hard
git checkout master
git reset --hard

exit 0
