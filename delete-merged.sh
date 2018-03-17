#! /usr/bin/env bash
set -euxo pipefail

git fetch --prune origin
git fetch --prune upstream

git checkout master
git reset --hard upstream/master

git branch -ra --merged | grep "origin/auto-update" | sed -e 's|^  remotes/origin/\(.*\)|git push origin :\1|' | while IFS= read -r line; do eval "$line"; done
