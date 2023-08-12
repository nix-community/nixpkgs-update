use chrono::NaiveDateTime;
use diesel::prelude::*;

#[derive(Queryable, Selectable)]
#[diesel(table_name = crate::schema::packages)]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct Package {
    pub id: String,
    pub attr_path: String,
    pub last_update_attempt: Option<NaiveDateTime>,
    pub last_update_log: Option<NaiveDateTime>,
    pub version_nixpkgs_master: Option<String>,
    pub last_checked_nixpkgs_master: Option<NaiveDateTime>,
    pub version_nixpkgs_staging: Option<String>,
    pub last_checked_nixpkgs_staging: Option<NaiveDateTime>,
    pub version_nixpkgs_staging_next: Option<String>,
    pub last_checked_nixpkgs_staging_next: Option<NaiveDateTime>,
    pub project_repology: Option<String>,
    pub nixpkgs_name_replogy: Option<String>,
    pub version_repology: Option<String>,
    pub last_checked_repology: Option<NaiveDateTime>,
    pub owner_github: Option<String>,
    pub repo_github: Option<String>,
    pub version_github: Option<String>,
    pub last_checked_github: Option<NaiveDateTime>,
    pub owner_gitlab: Option<String>,
    pub repo_gitlab: Option<String>,
    pub version_gitlab: Option<String>,
    pub last_checked_gitlab: Option<NaiveDateTime>,
    pub package_name_pypi: Option<String>,
    pub version_pypi: Option<String>,
    pub last_checked_pypi: Option<NaiveDateTime>,
    pub pending_pr: Option<i32>,
    pub pending_pr_owner: Option<String>,
    pub pending_pr_branch_name: Option<String>,
    pub last_checked_pending_pr: Option<NaiveDateTime>,
}
