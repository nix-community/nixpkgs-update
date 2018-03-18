#! /usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $SCRIPT_DIR/../utils.sh

BRANCH_NAME=$2

git checkout "$(git merge-base upstream/master upstream/staging)"

git checkout -B "$BRANCH_NAME"

NIXPKGS=$PWD

melpa=$HOME/.cache/melpa/
if ! [ -d $melpa ]
then
    git clone https://github.com/milkypostman/melpa $melpa
else
    cd $melpa
    git pull origin master
fi

emacs2nix=$HOME/.cache/emacs2nix
if ! [ -d $emacs2nix ]
then
    git clone https://github.com/matthewbauer/emacs2nix $emacs2nix
    cd $emacs2nix
    git submodule update --init
fi

cd $emacs2nix

./melpa-packages.sh --melpa $melpa -o $NIXPKGS/pkgs/applications/editors/emacs-modes/melpa-generated.nix

cd $NIXPKGS

git add pkgs/applications/editors/emacs-modes/melpa-generated.nix

git commit -m "melpa-packages $(date -Idate)"

pull_request "melpa-packages $(date -Idate)"
