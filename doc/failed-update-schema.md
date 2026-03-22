# Failed-update schema

This document summarizes SQLite columns on `packages` used when the Haskell updater runs with `DATABASE_URL` set (same file as the Rust tracker). Codes in `last_update_failure_kind` match `FailureKind.failureKindCode` in `src/FailureKind.hs`.

## Failure and streak fields

| Column | Meaning |
|--------|---------|
| `last_update_failure_kind` | Stable code (e.g. `nix_build_failed`, `fetch_hash_mismatch`). |
| `last_update_failure_message` | Truncated raw error text from the updater. |
| `last_update_failure_at` | UTC timestamp of the last classified failure write. |
| `last_update_attempt` | Refreshed on both success and failure paths. |
| `declared_maintainer_count` | Optional count from `meta.maintainers` when known. |
| `last_rebuild_path_count` | Optional rebuild estimate when outpaths ran. |
| `consecutive_failures` | Same-kind streak; reset to `0` on success; `1` on kind change. |
| `last_success_at` | Set when a run clears failure state. |

## WIP failure PR and escalation

| Column | Meaning |
|--------|---------|
| `failed_update_pr` | NixOS/nixpkgs PR number for the Tier-A WIP branch, if opened. |
| `failed_update_pr_opened_at` | When that PR was first recorded. |
| `escalated_at` | Set by `escalate-stale-failure-prs` for stale WIP rows (operator triage). |

## Operator tools

- `list-package-failures`: tabular or NDJSON listing of rows with `last_update_failure_kind` set.
- `failure-leaderboard`: single JSON document with `schema_version`, `weights`, and ranked `packages` (see `rust/src/leaderboard.rs`).
- `escalate-stale-failure-prs`: marks old WIP-failure rows with `escalated_at` (see `--help`).

## Environment and CLI (batch bot)

- `NIXPKGS_UPDATE_FAILURE_WIP_PR_MAX`: cap on Tier-A WIP PRs per `nixpkgs-update` process (overridable with `--failure-wip-pr-max` on `update` / `update-batch`).
- `NIXPKGS_UPDATE_FAILURE_LOG`: optional NDJSON failure log path (see `doc/package-tracking-db.md`).

## Evidence expectation

Maintainers should run `nixpkgs-review` on a branch containing the fix and paste the report on the WIP PR before dropping the `WIP:` prefix. The nixpkgs label `needs-review-evidence` is applied best-effort when the WIP PR is created.
