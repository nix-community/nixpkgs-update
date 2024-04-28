# nixpkgs-update {#introduction}

> The future is here; let's evenly distribute it!

The [nixpkgs-update](https://github.com/nix-community/nixpkgs-update) mission
is to make [nixpkgs](https://github.com/nixos/nixpkgs) the most
up-to-date repository of software in the world by the most ridiculous
margin possible. [Here's how we are doing so far](https://repology.org/repositories/graphs).

It provides an interactive tool for automating single package
updates. Given a package name, old version, and new version, it
updates the version, and fetcher hashes, makes a commit, and
optionally a pull request. Along the way, it does checks to make sure
the update has a baseline quality.

It is the code used by the GitHub bot
[@r-ryantm](https://github.com/r-ryantm) to automatically update
nixpkgs. It uses package repository information from
[Repology.org](https://repology.org/repository/nix_unstable), the
GitHub releases API, and the package passthru.updateScript to generate a lists of outdated
packages.
