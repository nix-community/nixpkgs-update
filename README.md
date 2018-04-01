# nixpkgs-update

[![Build Status](https://travis-ci.org/ryantm/nixpkgs-update.svg?branch=master)](https://travis-ci.org/ryantm/nixpkgs-update)

Scripts to try to update nixpkgs packages. Uses `hub` to automatically make PRs.

# Instructions

1. Clone this repo:
```
    git clone https://github.com/ryantm/nixpkgs-update
```
2. Get a list of oudated packages and place them in a `packages-to-update.txt` file in the root directory of this repository.

```
    git clone https://github.com/ryantm/repology-api.git
    cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api > packages-to-update.txt
```
3. `./ups.sh`

# prior work

https://github.com/NixOS/nixpkgs/blob/master/pkgs/common-updater/scripts/update-source-version
https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/upstream-updater
