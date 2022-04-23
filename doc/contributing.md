# Contributing {#contributing}

Incremental development:

```bash
nix-shell --run "cabal v2-repl"
```

Run the tests:

```bash
nix-shell --run "cabal v2-test"
```

Run a type checker in the background for quicker type checking feedback:

```bash
nix-shell --run "ghcid"
```

Run a type checker for the app code:

```bash
nix-shell --run 'ghcid -c "cabal v2-repl exe:nixpkgs-update"'
```

Run a type checker for the test code:

```bash
nix-shell --run 'ghcid -c "cabal v2-repl tests"'
```

Updating the Cabal file when adding new dependencies or options:

```bash
nix run nixpkgs#haskellPackages.hpack
```

Source files are formatted with [Ormolu](https://github.com/tweag/ormolu).

There is also a [Cachix cache](https://nixpkgs-update.cachix.org/) available for the dependencies of this program.
