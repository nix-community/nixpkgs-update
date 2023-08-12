// @generated automatically by Diesel CLI.

diesel::table! {
    packages (id) {
        id -> Text,
        attr_path -> Text,
        version_nixpkgs_master -> Nullable<Text>,
        version_nixpkgs_staging -> Nullable<Text>,
        version_nixpkgs_staging_next -> Nullable<Text>,
        version_repology -> Nullable<Text>,
        version_github -> Nullable<Text>,
        version_gitlab -> Nullable<Text>,
        version_pypi -> Nullable<Text>,
        project_repology -> Nullable<Text>,
        nixpkgs_name_replogy -> Nullable<Text>,
        owner_github -> Nullable<Text>,
        repo_github -> Nullable<Text>,
        owner_gitlab -> Nullable<Text>,
        repo_gitlab -> Nullable<Text>,
        last_checked_repology -> Nullable<Timestamp>,
        last_checked_github -> Nullable<Timestamp>,
        last_hecked_gitlab -> Nullable<Timestamp>,
        last_hecked_pypi -> Nullable<Timestamp>,
        last_checked_pending_pr -> Nullable<Timestamp>,
        last_update_attempt -> Nullable<Timestamp>,
        pending_pr -> Nullable<Integer>,
        pending_pr_owner -> Nullable<Text>,
        pending_pr_branch_name -> Nullable<Text>,
        last_update_log -> Nullable<Timestamp>,
    }
}
