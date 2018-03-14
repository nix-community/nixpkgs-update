#! /usr/bin/env bash
set -euxo pipefail

PACKAGE_NAME=$1
BRANCH_NAME="auto-update/$PACKAGE_NAME"

git fetch --multiple upstream origin
git checkout -b "$BRANCH_NAME" "origin/$BRANCH_NAME"
git reset --hard refs/remotes/upstream/staging
git cherry-pick "refs/remotes/origin/$BRANCH_NAME"
git push -f
git checkout master
