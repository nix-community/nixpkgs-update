# Package tracking database {#package-tracking-db}

The `rust/` crate maintains a SQLite database of packages and upstream version sources. The Haskell updater can optionally write **classified update failures** and **leaderboard-oriented counts** into the same file when `DATABASE_URL` points at that database.

## Schema migrations

From the `rust/` directory, with `DATABASE_URL` set to your SQLite path:

```bash
diesel migration run
```

New installs apply all migrations, including columns for `last_update_failure_*`, `declared_maintainer_count`, `last_rebuild_path_count`, optional `install_estimate`, and streak / WIP / escalation columns (`consecutive_failures`, `last_success_at`, `failed_update_pr`, `failed_update_pr_opened_at`, `escalated_at`). Migration `2026-03-22-160000_meta_install_estimate` also creates `package_tracking_meta` with key `failure_event_version` (reserved for future NDJSON or event-schema versioning).

## Haskell bridge

When `DATABASE_URL` is set in the environment of `nixpkgs-update` (batch or single update), successful and failed runs update rows in `packages` where `attr_path` matches. Failure **kind** strings match `FailureKind.failureKindCode` in `src/FailureKind.hs`. If no row exists yet, a minimal row is inserted with `id` and `attr_path` both set to the attr path (other version columns stay NULL until the Rust tracker fills them).

Both failure and success paths refresh `last_update_attempt` so you can compare it with Repology or nixpkgs version timestamps for staleness.

### NDJSON failure log

When `NIXPKGS_UPDATE_FAILURE_LOG` points to a file path, each failure appends one JSON object (newline-delimited). Use log rotation or a size cap at the deployment layer if the file grows quickly.

### GitHub failure issues (optional)

When `NIXPKGS_UPDATE_FAILURE_ISSUES=1`, the updater uses the same GitHub token as PR flows (`GITHUB_TOKEN` / options) to open or comment issues for failures. Default target repo is `nix-community/nixpkgs-update`; override with `NIXPKGS_UPDATE_FAILURE_ISSUE_REPO` (`owner/repo`). The token needs `issues:write` (classic: `repo` scope on that repository, or fine-grained access to issues). Deduping uses search for open issues whose title is `[nixpkgs-update failure] <attrpath>`; repeated failures add a comment. API errors are ignored so batch runs are not aborted.

### Maintainer signal: nixpkgs-review

When a failure is investigated, running [nixpkgs-review](https://github.com/Mic92/nixpkgs-review) on a branch with the fix and posting the summary (for example on the GitHub failure issue above) gives reviewers a concrete rebuild signal beyond the bot log excerpt.

### Operator triage

Use `list-package-failures --sort staleness` to surface failures that have been open the longest. For a scored JSON view (rebuild weight plus consecutive same-kind failures), run `failure-leaderboard` (see `doc/failed-update-schema.md`). To stamp `escalated_at` on stale rows that still have a recorded WIP failure PR, run `escalate-stale-failure-prs --days N`. There is no automatic ping to Nixpkgs committers unless you add tooling on top.

## Listing recorded failures

Build the Rust workspace, then run the helper binary (still requires `DATABASE_URL`):

```bash
cd rust
export DATABASE_URL=/path/to/packages.sqlite
cargo run --bin list-package-failures
```

From the repo flake you can build the Rust tools without a local Cargo tree (uses the same `nixpkgs` pin as the rest of this repo):

```bash
nix build .#list-package-failures
export DATABASE_URL=/path/to/packages.sqlite
./result/bin/list-package-failures
./result/bin/failure-leaderboard
./result/bin/escalate-stale-failure-prs --help
```

The same derivation installs all SQLite helper binaries under `result/bin/`.

Flakes that use `git+file:` only include **git-tracked** files. Until new paths such as `pkgs/nixpkgs-update-rust.nix` are committed, use `nix build path:$(pwd)#list-package-failures` from a checkout, or run `cargo run --bin list-package-failures` inside `rust/`.

Tab-separated output is the default (includes `install_estimate` when present). Pass `--json` for one JSON object per line.

Sort modes (`--sort`):

- `rebuild` (default): larger `last_rebuild_path_count` and newer `last_update_failure_at` first.
- `staleness`: oldest `last_update_failure_at` first (failures that have been open the longest).
- `maintainers`: smaller `declared_maintainer_count` first, then larger rebuild counts (rough signal for where help is spread thin).

## Codes

Interpret `last_update_failure_kind` using the same stable codes as the Haskell `FailureKind` type (for example `nix_build_failed`, `fetch_hash_mismatch`, `policy_skiplist`, `unclassified`). The full set is defined beside `failureKindCode` in `src/FailureKind.hs`. Column-level notes live in `doc/failed-update-schema.md`.
