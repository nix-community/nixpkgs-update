CREATE TABLE packages (
    id TEXT PRIMARY KEY NOT NULL
  , attr_path TEXT NOT NULL
  , last_update_attempt DATETIME
  , last_update_log DATETIME
  , version_nixpkgs_master TEXT
  , last_checked_nixpkgs_master DATETIME
  , version_nixpkgs_staging TEXT
  , last_checked_nixpkgs_staging DATETIME
  , version_nixpkgs_staging_next TEXT
  , last_checked_nixpkgs_staging_next DATETIME
  , version_repology TEXT
  , project_repology TEXT
  , nixpkgs_name_replogy TEXT
  , last_checked_repology DATETIME
  , version_github TEXT
  , owner_github TEXT
  , repo_github TEXT
  , last_checked_github DATETIME
  , version_gitlab TEXT
  , owner_gitlab TEXT
  , repo_gitlab TEXT
  , last_checked_gitlab DATETIME
  , package_name_pypi TEXT
  , version_pypi TEXT
  , last_checked_pypi DATETIME
  , pending_pr INTEGER
  , pending_pr_owner TEXT
  , pending_pr_branch_name TEXT
  , last_checked_pending_pr DATETIME
)
