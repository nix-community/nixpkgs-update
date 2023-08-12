CREATE TABLE packages (
    id text PRIMARY KEY NOT NULL
  , attr_path text NOT NULL
  , version_nixpkgs_master text
  , version_nixpkgs_staging text
  , version_nixpkgs_staging_next text
  , version_repology text
  , version_github text
  , version_gitlab text
  , version_pypi text
  , project_repology text
  , nixpkgs_name_replogy text
  , owner_github text
  , repo_github text
  , owner_gitlab text
  , repo_gitlab text
  , last_checked_repology text
  , last_checked_github text
  , last_hecked_gitlab text
  , last_hecked_pypi text
  , last_checked_pending_pr text
  , last_update_attempt text
  , pending_pr integer
  , pending_pr_owner text
  , pending_pr_branch_name text
  , last_update_log text
)
