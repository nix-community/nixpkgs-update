# nixpkgs-update

[![Build Status](https://travis-ci.org/ryantm/nixpkgs-update.svg?branch=master)](https://travis-ci.org/ryantm/nixpkgs-update)

> The future is here; let's evenly distribute it!

# Introduction

[nixpkgs-update](https://github.com/ryantm/nixpkgs-update) is the code
used by [@r-ryantm](https://github.com/r-ryantm) to semi-automatically
update nixpkgs. It uses package repository information from
[Repology.org](https://repology.org/repository/nix_unstable) to
generate a list of outdated packages. A package is considered
"outdated" if a newer version of the package is available at any other
[repository tracked by
Repology](https://repology.org/repositories/statistics/newest). nixpkgs-update
tries to update each package in the dumbest way that could work. It
find-replaces the old version number with the new one, uses
`nix-prefetch-url` to try to get the new hash, then tries to build the
package. If it succeeds, it checks the outputs and makes a pull
request. It also uploads the built package to
[Cachix](https://r-ryantm.cachix.org/), which people can use to
manually test the package without building it themselves.


# nixpkgs-update features

## Checks

A number of checks are performed to help nixpkgs maintainers gauge the
liklihood that an update was successful. All the binaries are run with
various flags to see if they have a zero exit code and output the new
version number. The outpath directory tree is searched for files
containing the new version number. A directory tree and disk usage
listing is provided.


## Rebuild reports

 The PRs made by nixpkgs-update say what packages
need to be rebuilt if the pull request is merged. This uses the same
mechanism [OfBorg](https://github.com/NixOS/ofborg) uses to put
rebuild labels on PRs. Not limited by labels, it can report the exact
number of rebuilds and list some of the attrpaths that would need to
be rebuilt.


## PRs against staging

If a PR merge would cause more than 100 packages to be rebuilt, the PR
is made against staging.


## Logs

[Logs from r-ryantm's
runs](https://discourse.nixos.org/t/nixpkgs-update-r-ryantm-logs) are
available on Discourse. There are a lot of packages `nixpkgs-update`
currently has no hope of updating. Please dredge the logs to find out
why your pet package is not receiving updates.


## Cachix

By uploading the build outputs to
[Cachix](https://r-ryantm.cachix.org/), nixpkgs-update allows you to
test a package with one command.


# Instructions

1. Clone this repo and build the tool:
    ```
    git clone https://github.com/ryantm/nixpkgs-update && cd nixpkgs-update
    nix run nixpkgs.haskellPackages.hpack -c hpack && nix run nixpkgs.cabal2nix -c cabal2nix --shell . > shell.nix && nix-build shell.nix
    ```
2. Get a list of oudated packages and place them in a `packages-to-update.txt` file in the root directory of this repository.
    ```
    git clone https://github.com/ryantm/repology-api.git && cd repology-api
    nix run nixpkgs.cabal2nix -c cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api > ../packages-to-update.txt
    ```
3. Return back `cd ..` and run the tool `nix run -f '<nixpkgs>' gitAndTools.hub gist jq tree -c result/bin/nixpkgs-update --update`


# Prior work

* https://github.com/NixOS/nixpkgs/blob/master/pkgs/common-updater/scripts/update-source-version
* https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/upstream-updater
