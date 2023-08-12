CREATE TABLE packages (
    id TEXT PRIMARY KEY NOT NULL
  , attr_path TEXT NOT NULL
  , version_nixpkgs_master TEXT
  , version_nixpkgs_staging TEXT
  , version_nixpkgs_staging_next TEXT
  , version_repology TEXT
  , version_github TEXT
  , version_gitlab TEXT
  , version_pypi TEXT
  , project_repology TEXT
  , nixpkgs_name_replogy TEXT
  , owner_github TEXT
  , repo_github TEXT
  , owner_gitlab TEXT
  , repo_gitlab TEXT
  , last_checked_repology DATETIME
  , last_checked_github DATETIME
  , last_hecked_gitlab DATETIME
  , last_hecked_pypi DATETIME
  , last_checked_pending_pr DATETIME
  , last_update_attempt DATETIME
  , pending_pr INTEGER
  , pending_pr_owner TEXT
  , pending_pr_branch_name TEXT
  , last_update_log DATETIME
)
