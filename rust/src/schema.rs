// Schema for packages table. Diesel type annotations must match the SQLite
// column types declared in migrations/.
//
// last_update_failure_* codes match FailureKind.failureKindCode in src/FailureKind.hs.

diesel::table! {
    package_tracking_meta (key) {
        key -> Text,
        value -> Text,
    }
}

diesel::table! {
    packages (id) {
        id -> Text,
        attr_path -> Text,
        last_update_attempt -> Nullable<Timestamp>,
        last_update_log -> Nullable<Timestamp>,
        version_nixpkgs_master -> Nullable<Text>,
        last_checked_nixpkgs_master -> Nullable<Timestamp>,
        version_nixpkgs_staging -> Nullable<Text>,
        last_checked_nixpkgs_staging -> Nullable<Timestamp>,
        version_nixpkgs_staging_next -> Nullable<Text>,
        last_checked_nixpkgs_staging_next -> Nullable<Timestamp>,
        version_repology -> Nullable<Text>,
        project_repology -> Nullable<Text>,
        nixpkgs_name_replogy -> Nullable<Text>,
        last_checked_repology -> Nullable<Timestamp>,
        version_github -> Nullable<Text>,
        owner_github -> Nullable<Text>,
        repo_github -> Nullable<Text>,
        last_checked_github -> Nullable<Timestamp>,
        version_gitlab -> Nullable<Text>,
        owner_gitlab -> Nullable<Text>,
        repo_gitlab -> Nullable<Text>,
        last_checked_gitlab -> Nullable<Timestamp>,
        package_name_pypi -> Nullable<Text>,
        version_pypi -> Nullable<Text>,
        last_checked_pypi -> Nullable<Timestamp>,
        pending_pr -> Nullable<Integer>,
        pending_pr_owner -> Nullable<Text>,
        pending_pr_branch_name -> Nullable<Text>,
        last_checked_pending_pr -> Nullable<Timestamp>,
        last_update_failure_kind -> Nullable<Text>,
        last_update_failure_message -> Nullable<Text>,
        last_update_failure_at -> Nullable<Timestamp>,
        declared_maintainer_count -> Nullable<BigInt>,
        last_rebuild_path_count -> Nullable<BigInt>,
        install_estimate -> Nullable<BigInt>,
        consecutive_failures -> Integer,
        last_success_at -> Nullable<Timestamp>,
        failed_update_pr -> Nullable<Integer>,
        failed_update_pr_opened_at -> Nullable<Timestamp>,
        escalated_at -> Nullable<Timestamp>,
    }
}
