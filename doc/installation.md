# Installation {#installation}

::: note
For the Cachix cache to work, your user must be in the trusted-users
list or you can use sudo since root is effectively trusted.
:::

Run without installing on stable Nix:

```ShellSession
$ nix run \
  --option extra-substituters 'https://nix-community.cachix.org/' \
  --option extra-trusted-public-keys 'nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=' \
  -f https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix command enabled:

```ShellSession
$ nix shell \
  --option extra-substituters 'https://nix-community.cachix.org/' \
  --option extra-trusted-public-keys 'nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=' \
  -f https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix flakes enabled:

```ShellSession
$ nix run \
  --option extra-substituters 'https://nix-community.cachix.org/' \
  --option extra-trusted-public-keys 'nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=' \
  github:nix-community/nixpkgs-update -- --help
```

Install into your Nix profile:

```ShellSession
$ nix-env \
  --option extra-substituters 'https://nix-community.cachix.org/' \
  --option extra-trusted-public-keys 'nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=' \
  -if https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz
```

Declaratively with [niv](https://github.com/nmattia/niv):

```ShellSession
$ niv add nix-community/nixpkgs-update
```

NixOS config with Niv:

```nix
let
  sources = import ./nix/sources.nix;
  nixpkgs-update = import sources.nixpkgs-update {};
in
  environment.systemPackages = [ nixpkgs-update ];
```

home-manager config with Niv:

```nix
let
  sources = import ./nix/sources.nix;
  nixpkgs-update = import sources.nixpkgs-update {};
in
  home.packages = [ nixpkgs-update ];
```
