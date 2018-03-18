#! /usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/../utils.sh

ATTR_PATH=$1
BRANCH_NAME=$2

git checkout "$(git merge-base upstream/master upstream/staging)"

git checkout -B "$BRANCH_NAME"

$(nix eval -f . --raw "pkgs.$ATTR_PATH.updateScript")

git add .

git commit -am "$ATTR_PATH: update"

pull_request "$ATTR_PATH: update"
