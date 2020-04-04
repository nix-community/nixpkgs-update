# nixpkgs-update

[![Build Status](https://github.com/ryantm/nixpkgs-update/workflows/CI/badge.svg)](https://github.com/ryantm/nixpkgs-update/actions)
[![Patreon](https://img.shields.io/badge/patreon-donate-blue.svg)](https://www.patreon.com/nixpkgsupdate)

> The future is here; let's evenly distribute it!

# Introduction

[nixpkgs-update](https://github.com/ryantm/nixpkgs-update) is the code
used by [@r-ryantm](https://github.com/r-ryantm) to automatically
update nixpkgs. It uses package repository information from
[Repology.org](https://repology.org/repository/nix_unstable), the
GitHub releases API, and PyPI to generate a lists of outdated
packages. nixpkgs-update tries to update each package in the dumbest
way that could work. It find-replaces the old version number with the
new one, uses `nix-build` to try to get the new hash, then tries to
build the package. If it succeeds, it checks the outputs and makes a
pull request. It also uploads the built package to
[Cachix](https://r-ryantm.cachix.org/), which people can use to
manually test the package without building it themselves.


# nixpkgs-update features

## Checks

A number of checks are performed to help nixpkgs maintainers gauge the
likelihood that an update was successful. All the binaries are run with
various flags to see if they have a zero exit code and output the new
version number. The outpath directory tree is searched for files
containing the new version number. A directory tree and disk usage
listing is provided.


## Security report

Information from the National Vulnerability Database maintained by
NIST is compared against the current and updated package version. The
nixpkgs package name is matched with the Common Platform Enumeration
vendor, product, edition, software edition, and target software fields
to find candidate Common Vulnerabilities and Exposures (CVEs). The
CVEs are filtered by the matching the current and updated versions
with the CVE version ranges.

The general philosophy of the CVE search is to avoid false negatives,
which means we expect to generate many false positives. The false
positives can be carefully removed by manually created rules
implemented in the filter function in the NVDRules module.

If there are no CVE matches, the report is not shown. The report has
three parts: CVEs resolved by this update, CVEs introduced by this
update, and CVEs present in both version.

If you would like to report a problem with the security report, please
use the [nixpkgs-update GitHub
issues](https://github.com/ryantm/nixpkgs-update/issues).

The initial development of the security report was made possible by a
partnership with [Serokell](https://serokell.io/) and the [NLNet
Foundation](https://nlnet.nl/) through their [Next Generation Internet
Zero Discovery initiative](https://nlnet.nl/discovery/) (NGI0
Discovery). NGI0 Discovery is made possible with financial support
from the [European Commission](https://ec.europa.eu/).


## Rebuild report

The PRs made by nixpkgs-update say what packages need to be rebuilt if
the pull request is merged. This uses the same mechanism
[OfBorg](https://github.com/NixOS/ofborg) uses to put rebuild labels
on PRs. Not limited by labels, it can report the exact number of
rebuilds and list some of the attrpaths that would need to be rebuilt.


## PRs against staging

If a PR merge would cause more than 100 packages to be rebuilt, the PR
is made against staging.


## Logs

[Logs from r-ryantm's runs](https://r.ryantm.com/log/) are
available online. There are a lot of packages `nixpkgs-update`
currently has no hope of updating. Please dredge the logs to find out
why your pet package is not receiving updates.


## Cachix

By uploading the build outputs to
[Cachix](https://r-ryantm.cachix.org/), nixpkgs-update allows you to
test a package with one command.


# Instructions

1. Clone this repository and build `nixpkgs-update`:
    ```
    git clone https://github.com/ryantm/nixpkgs-update && cd nixpkgs-update
    nix-build
    ```

2. Setup [hub](https://github.com/github/hub) and give it your GitHub credentials, so it saves an oauth token.

3. To test your config, try to update a single package, like this:

   ```
   ./result/bin/nixpkgs-update update --dry-run --additional-updates "pkg oldVer newVer update-page"`

   # Example:
   ./result/bin/nixpkgs-update update --dry-run --additional-updates "tflint 0.15.0 0.15.1 repology.org"`
   ```

   replacing `tflint` with the attribute name of the package you actually want
   to update, and the old version and new version accordingly.

   If this works, you are now setup to hack on `nixpkgs-update`! Since we passed
   `--dry-run`, it will just print a `diff` of the updated nix expression, then
   exit. If you run it without `--dry-run`, it will actually send a pull
   request, which looks like this: https://github.com/NixOS/nixpkgs/pull/82465


4. If you'd like to send a batch of updates, get a list of outdated packages and
   place them in a `packages-to-update.txt` file:

  ```
  ./result/bin/nixpkgs-update fetch-repology > packages-to-update.txt
  ```

  There also exist alternative sources of updates, these include:

   - PyPI, the Python Package Index:
     [nixpkgs-update-pypi-releases](https://github.com/jonringer/nixpkgs-update-pypi-releases)
   - GitHub releases:
     [nixpkgs-update-github-releases](https://github.com/synthetica9/nixpkgs-update-github-releases)

5. Run the tool in batch mode with `update-list`:

  ```
  ./result/bin/nixpkgs-update update-list
  ```

# Development

Setup a Cabal file (also run this when adding new dependencies):

```
nix run nixpkgs.haskellPackages.hpack -c hpack && nix run nixpkgs.cabal2nix -c cabal2nix --hpack . > nixpkgs-update.nix
```

For incremental building, first make a Cabal file with the above command, then use nix-shell

```
nix run nixpkgs.haskellPackages.hpack -c hpack && nix run nixpkgs.cabal2nix -c cabal2nix --hpack . > nixpkgs-update.nix
nix-shell
cabal new-repl
```

Run a type checker in the background for quicker type checking feedback:

```
nix-shell --run ghcid
```

Source files are formatted with [Ormolu](https://github.com/tweag/ormolu).

There is also a [Cachix cache](https://nixpkgs-update.cachix.org/) available for the dependencies of this program.

# Prior work

* https://github.com/NixOS/nixpkgs/blob/master/pkgs/common-updater/scripts/update-source-version
* https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/upstream-updater
