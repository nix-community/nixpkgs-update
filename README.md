# nixpkgs-update

[![Build Status](https://github.com/ryantm/nixpkgs-update/workflows/CI/badge.svg)](https://github.com/ryantm/nixpkgs-update/actions)
[![Patreon](https://img.shields.io/badge/patreon-donate-blue.svg)](https://www.patreon.com/nixpkgsupdate)

> The future is here; let's evenly distribute it!

# Contents

* [Introduction](#introduction)
* [Interactive updates](#interactive-updates)
* [Batch updates](#batch-updates)
* [Details](#details)
* [Development](#development)

# Introduction

The [nixpkgs-update](https://github.com/ryantm/nixpkgs-update) mission
is to make [nixpkgs](https://github.com/nixos/nixpkgs) the most
up-to-date repository of software in the world by the most ridiculous
margin possible.

It provides an interactive tool for automating single package
updates. Given a package name, old version, and new version, it
updates the version, and fetcher hashes, makes a commit, and
optionally a pull request. Along the way, it does checks to make sure
the update has a baseline quality.

It is the code used by the GitHub bot
[@r-ryantm](https://github.com/r-ryantm) to automatically update
nixpkgs. It uses package repository information from
[Repology.org](https://repology.org/repository/nix_unstable), the
GitHub releases API, and PyPI to generate a lists of outdated
packages.

# Installation

For the Cachix cache to work, your user must be in the trusted-users
list or you can use sudo since root is effectively trusted.

Run without installing:

```
nix run \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -f https://github.com/ryantm/nixpkgs-update/archive/latest.tar.gz \
  -c nixpkgs-update --help
```

Install into your Nix profile:

```
nix-env \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -if https://github.com/ryantm/nixpkgs-update/archive/latest.tar.gz
```

# Interactive updates

nixpkgs-update supports interactive, single package updates via the
`update` subcommand.

# Update tutorial

1. Setup [hub](https://github.com/github/hub) and give it your GitHub
   credentials, so it saves an oauth token. This allows nixpkgs-update
   to query the GitHub API.
2. Go to your local checkout of nixpkgs, and **make sure the working
   directory is clean**. Be on a branch you are okay committing to.
3. Run it like: `nixpkgs-update update "postman 7.20.0 7.21.2"`
   which mean update the package "postman" from version 7.20.0
   to version 7.21.2.
4. It will run the updater, and, if the update builds, it will commit
   the update and output a message you could use for a pull request.

# Flags

* `--pr`&mdash;uses Hub to submit a PR to GitHub.
* `--cve`&mdash;adds CVE vulnerability reporting to the PR message. On
  first invocation with this option, a CVE database is
  built. Subsequent invocations will be much faster.

# Batch updates

nixpkgs-update supports batch updates via the `update-list`
subcommand.

## Update-List tutorial

1. Setup [hub](https://github.com/github/hub) and give it your GitHub
   credentials, so it saves an oauth token. This allows nixpkgs-update
   to query the GitHub API.

2. Clone this repository and build `nixpkgs-update`:
    ```
    git clone https://github.com/ryantm/nixpkgs-update && cd nixpkgs-update
    nix-build
    ```

3. To test your config, try to update a single package, like this:

   ```
   ./result/bin/nixpkgs-update update "pkg oldVer newVer update-page"`

   # Example:
   ./result/bin/nixpkgs-update update "tflint 0.15.0 0.15.1 repology.org"`
   ```

   replacing `tflint` with the attribute name of the package you actually want
   to update, and the old version and new version accordingly.

   If this works, you are now setup to hack on `nixpkgs-update`! If
   you run it with `--pr`, it will actually send a pull request, which
   looks like this: https://github.com/NixOS/nixpkgs/pull/82465


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

# Details

Some of these features only apply to the update-list sub-command or to
features only available to the @r-ryantm bot.

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


# Development

Setup a Cabal file (also run this when adding new dependencies):

```
nix run nixpkgs.haskellPackages.hpack -c hpack && nix run nixpkgs.cabal2nix -c cabal2nix --hpack . > nixpkgs-update.nix
```

For incremental building, first make a Cabal file with the above command, then use nix-shell

```
nix run nixpkgs.haskellPackages.hpack -c hpack && nix run nixpkgs.cabal2nix -c cabal2nix --hpack . > nixpkgs-update.nix
nix-shell
cabal v2-repl
```

Run a type checker in the background for quicker type checking feedback:

```
nix-shell --run ghcid
```

Run a type checker for the app code:

```
nix-shell --run 'ghcid -c "cabal v2-repl exe:nixpkgs-update"'
```

Run a type checker for the test code:

```
nix-shell --run 'ghcid -c "cabal v2-repl tests"'
```


Source files are formatted with [Ormolu](https://github.com/tweag/ormolu).

There is also a [Cachix cache](https://nixpkgs-update.cachix.org/) available for the dependencies of this program.
