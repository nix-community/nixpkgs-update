#! /usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/../utils.sh

BRANCH_NAME=$2

git checkout "$(git merge-base upstream/master upstream/staging)"

git checkout -B "$BRANCH_NAME"

NIXPKGS=$PWD

emacs2nix=$HOME/.cache/emacs2nix
if ! [ -d $emacs2nix ]
then
    git clone https://github.com/matthewbauer/emacs2nix $emacs2nix
    cd $emacs2nix
    git submodule update --init
fi

cd $emacs2nix

./elpa-packages.sh -o $NIXPKGS/pkgs/applications/editors/emacs-modes/elpa-generated.nix

cd $NIXPKGS

git add pkgs/applications/editors/emacs-modes/elpa-generated.nix

git commit -am "elpa-packages $(date -Idate)"

pull_request "elpa-packages $(date -Idate)"
