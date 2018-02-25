#! /usr/bin/env bash
set -euxo pipefail

NIX_PATH=nixpkgs=$(pwd)

PACKAGE_NAME=$1
OLD_VERSION=$2
NEW_VERSION=$3

BRANCH_NAME="auto-update/$1-$2-to-$3"

DERIVATION_FILE=$(find . | grep "/$1/default.nix" | head -n1)

function error_cleanup() {
    git checkout master
    git reset --hard
    git branch -d "$BRANCH_NAME"
}
trap error_cleanup ERR

git checkout -B "$BRANCH_NAME"
git reset --hard

OLD_HASH=$(nix-prefetch-url -A "$1.src")

grep "$2" "$DERIVATION_FILE"

sed -i "s/$2/$3/g" "$DERIVATION_FILE"

NEW_HASH=$(nix-prefetch-url -A "$1.src")

if [ "$OLD_HASH" = "$NEW_HASH" ]
then

    echo "Hashes equal; no update necesseary"
    exit 0
fi

grep "$OLD_HASH" "$DERIVATION_FILE"

sed -i "s/$OLD_HASH/$NEW_HASH/g" "$DERIVATION_FILE"

nix-build -A $1

git diff

git commit -am  "$1: $2 -> $3

Semi-automatic update.

- [x] built on NixOS"

git push --set-upstream origin "$BRANCH_NAME"
git checkout master
