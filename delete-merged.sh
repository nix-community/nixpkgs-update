#! /usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

GITHUB_TOKEN="$(cat "$SCRIPT_DIR"/github_token.txt)"
export GITHUB_TOKEN

# shellcheck source=setup-nixpkgs.sh
source "$SCRIPT_DIR/setup-nixpkgs.sh"

git fetch --prune origin
git fetch --prune upstream

git checkout master
git reset --hard upstream/master

git branch -ra --merged | grep "origin/auto-update/" | sed -e 's|^  remotes/origin/\(.*\)|git push origin :\1|' | while IFS= read -r line; do eval "$line"; done || true

git branch -a --merged | grep "auto-update/" | sed -e 's|^\(.*\)|git branch -d \1|' | while IFS= read -r line; do eval "$line"; done
