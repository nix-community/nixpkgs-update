#!/usr/bin/env bash
set -euxo pipefail

NIXPKGS=$HOME/.cache/nixpkgs

if ! [ -d "$NIXPKGS" ]
then
    hub clone nixpkgs "$NIXPKGS" # requires that user has forked nixpkgs
    cd "$NIXPKGS"
    git remote add upstream https://github.com/NixOS/nixpkgs
    git fetch upstream
    git fetch origin staging
    git fetch upstream staging
fi

export NIXPKGS

cd "$NIXPKGS"
