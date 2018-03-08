#! /usr/bin/env bash
set -euxo pipefail

NIX_PATH=nixpkgs=$(pwd)

PACKAGE_NAME=$1
OLD_VERSION=$2
NEW_VERSION=$3

BRANCH_NAME="auto-update/$1"

# Package blacklist
case "$PACKAGE_NAME" in
    *atlas*) false;; # super slow to build
    *aws-sdk-cpp*) false;; # super slow to build
    *jquery*) false;; # this isn't a real package
esac

if git branch --remote | grep "origin/auto-update/${PACKAGE_NAME}"; then false; fi

DERIVATION_FILE=$(find . | grep "/$1/default.nix" | head -n1)

function error_cleanup() {
    git checkout master
    git reset --hard
    git branch -D "$BRANCH_NAME" || true
    exit 1
}
trap error_cleanup ERR

git reset --hard

# Skip packages that have special builders
if grep -q "buildGoPackage" "$DERIVATION_FILE"; then false; fi
if grep -q "buildRustCrate" "$DERIVATION_FILE"; then false; fi
if grep -q "buildPythonPackage" "$DERIVATION_FILE"; then false; fi
if grep -q "buildRubyGem" "$DERIVATION_FILE"; then false; fi
if grep -q "bundlerEnv" "$DERIVATION_FILE"; then false; fi

# Make sure it hasn't been updated on master
grep "$2" "$DERIVATION_FILE"

# Make sure it hasn't been updated on staging
git checkout staging
grep "$2" "$DERIVATION_FILE"

git checkout master

git checkout -B "$BRANCH_NAME"
OLD_HASH=$(nix eval -f . --raw "pkgs.${PACKAGE_NAME}.src.drvAttrs.outputHash")

sed -i "s/${2//\./\\.}/$3/g" "$DERIVATION_FILE"

NEW_HASH=$(nix-prefetch-url -A "$1.src")

if [ "$OLD_HASH" = "$NEW_HASH" ]
then

    echo "Hashes equal; no update necessary"
    exit 1
fi

sed -i "s/$OLD_HASH/$NEW_HASH/g" "$DERIVATION_FILE"

nix build -f . -o ./result $1

RESULT=$(readlink ./result)

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

CHECK_RESULT=$($SCRIPT_DIR/check-result.sh $RESULT $NEW_VERSION)

MAINTAINERS=
if nix eval "(let pkgs = import ./. {}; in pkgs.$PACKAGE_NAME.meta.maintainers)" > /dev/null 2>&1
then
    maintainers=$(nix eval --raw '(let pkgs = import ./. {}; gh = m : m.github or ""; nonempty = s: s != ""; addat = s: "@"+s; in builtins.concatStringsSep " " (map addat (builtins.filter nonempty (map gh pkgs.'"${PACKAGE_NAME}"'.meta.maintainers))))')
    if [ -n "$maintainers" ]
    then
        MAINTAINERS="

cc $maintainers"
    fi
fi

git diff

COMMIT_MESSAGE="$1: $2 -> $3

Semi-automatic update. These checks were done:

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
   export GITHUB_TOKEN=`cat $SCRIPT_DIR/github_token.txt`
   hub pull-request -m "$PR_MESSAGE"
fi

git checkout master
git reset --hard

exit 0
