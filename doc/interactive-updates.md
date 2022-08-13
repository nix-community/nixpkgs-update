# Interactive updates {#interactive-updates}

nixpkgs-update supports interactive, single package updates via the
`update` subcommand.

# Update tutorial

1. Setup [`hub`](https://github.com/github/hub) and give it your
   GitHub credentials.  Alternatively, if you prefer not to install
   and configure `hub`, you can manually create a GitHub token with
   `repo` and `gist` scopes.  Provide it to `nixpkgs-update` by
   exporting it as the `GITHUB_TOKEN` environment variable
   (`nixpkgs-update` _only_ tries to use `hub` to check out the
   `nixpkgs` repo into your XDG cache directory, if you run
   `nixpkgs-update` outside of a `nixpkgs` checkout directory).
2. Go to your local checkout of nixpkgs, and **make sure the working
   directory is clean**. Be on a branch you are okay committing to.
3. Run it like: `nixpkgs-update update "postman 7.20.0 7.21.2"`
   which mean update the package "postman" from version 7.20.0
   to version 7.21.2.
4. It will run the updater, and, if the update builds, it will commit
   the update and output a message you could use for a pull request.

# Flags

`--cve`

: adds CVE vulnerability reporting to the PR message. On
  first invocation with this option, a CVE database is
  built. Subsequent invocations will be much faster.

`--nixpkgs-review`

: runs [nixpkgs-review](https://github.com/Mic92/nixpkgs-review),
  which tries to build all the packages that depend on the one being
  updated and adds a report.
