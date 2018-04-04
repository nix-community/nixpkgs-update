# nixpkgs-update

[![Build Status](https://travis-ci.org/ryantm/nixpkgs-update.svg?branch=master)](https://travis-ci.org/ryantm/nixpkgs-update)

Scripts to try to update nixpkgs packages. Uses `hub` to automatically make PRs.

# Instructions

1. Clone this repo and build the tool:
    ```
    git clone https://github.com/ryantm/nixpkgs-update && cd nixpkgs-update
    nix run nixpkgs.cabal2nix -c cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix
    ```
2. Get a list of oudated packages and place them in a `packages-to-update.txt` file in the root directory of this repository.
    ```
    git clone https://github.com/ryantm/repology-api.git && cd repology-api
    nix run nixpkgs.cabal2nix -c cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api > ../packages-to-update.txt
    ```
3. Return back `cd ..` and run the tool `result/bin/nixpkgs-update`

# Prior work

* https://github.com/NixOS/nixpkgs/blob/master/pkgs/common-updater/scripts/update-source-version
* https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/upstream-updater
