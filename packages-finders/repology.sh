#! /usr/bin/env bash
set -euxo pipefail

repology=$HOME/.cache/repology

if ! [ -d "$repology" ]
then
    git clone https://github.com/ryantm/repology-api.git $repology > /dev/null
fi

nix-shell -p cabal2nix --run "cabal2nix --shell --hpack $repology" > $repology/shell.nix
$(nix-build --no-out-link $repology/shell.nix)/bin/repology-api
