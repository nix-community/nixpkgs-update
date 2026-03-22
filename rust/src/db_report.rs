//! Query helpers for classified update failures stored in `packages` (Haskell
//! `FailureDb` and Diesel schema).

use chrono::NaiveDateTime;
use diesel::prelude::*;
use diesel::sql_query;
use diesel::sql_types::{BigInt, Nullable, Text, Timestamp};
use diesel::RunQueryDsl;

/// How to order rows returned by [`fetch_packages_with_failures_sorted`].
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum FailureReportSort {
    /// Larger rebuild sets and newer failures first (default).
    #[default]
    RebuildImpact,
    /// Oldest `last_update_failure_at` first (longest-open failures).
    OldestFailureFirst,
    /// Fewer declared maintainers first, then larger rebuild counts (rough "load" signal).
    FewMaintainersHighRebuild,
}

impl FailureReportSort {
    /// SQL `ORDER BY` fragment (for tests and dynamic queries).
    pub(crate) fn order_clause(self) -> &'static str {
        match self {
            Self::RebuildImpact => {
                r#"ORDER BY (last_rebuild_path_count IS NULL), last_rebuild_path_count DESC,
                    (last_update_failure_at IS NULL), last_update_failure_at DESC"#
            }
            Self::OldestFailureFirst => {
                r#"ORDER BY (last_update_failure_at IS NULL), last_update_failure_at ASC"#
            }
            Self::FewMaintainersHighRebuild => {
                r#"ORDER BY (declared_maintainer_count IS NULL), declared_maintainer_count ASC,
                    (last_rebuild_path_count IS NULL), last_rebuild_path_count DESC"#
            }
        }
    }
}

#[derive(QueryableByName, Debug, Clone)]
pub struct PackageFailureReport {
    #[diesel(sql_type = Text)]
    pub attr_path: String,
    #[diesel(sql_type = Nullable<Text>)]
    pub last_update_failure_kind: Option<String>,
    #[diesel(sql_type = Nullable<Text>)]
    pub last_update_failure_message: Option<String>,
    #[diesel(sql_type = Nullable<Timestamp>)]
    pub last_update_failure_at: Option<NaiveDateTime>,
    #[diesel(sql_type = Nullable<Timestamp>)]
    pub last_update_attempt: Option<NaiveDateTime>,
    #[diesel(sql_type = Nullable<BigInt>)]
    pub declared_maintainer_count: Option<i64>,
    #[diesel(sql_type = Nullable<BigInt>)]
    pub last_rebuild_path_count: Option<i64>,
    #[diesel(sql_type = Nullable<BigInt>)]
    pub install_estimate: Option<i64>,
    #[diesel(sql_type = Nullable<Text>)]
    pub version_repology: Option<String>,
    #[diesel(sql_type = Nullable<Text>)]
    pub version_nixpkgs_master: Option<String>,
}

/// Rows with a non-null `last_update_failure_kind`, ordered by `sort`.
pub fn fetch_packages_with_failures_sorted(
    conn: &mut SqliteConnection,
    sort: FailureReportSort,
) -> QueryResult<Vec<PackageFailureReport>> {
    let q = format!(
        r#"SELECT attr_path, last_update_failure_kind, last_update_failure_message,
                  last_update_failure_at, last_update_attempt, declared_maintainer_count, last_rebuild_path_count,
                  install_estimate, version_repology, version_nixpkgs_master
           FROM packages
           WHERE last_update_failure_kind IS NOT NULL
           {}"#,
        sort.order_clause()
    );
    sql_query(q).load(conn)
}

/// Same as [`fetch_packages_with_failures_sorted`] with [`FailureReportSort::RebuildImpact`].
pub fn fetch_packages_with_failures(
    conn: &mut SqliteConnection,
) -> QueryResult<Vec<PackageFailureReport>> {
    fetch_packages_with_failures_sorted(conn, FailureReportSort::default())
}

#[cfg(test)]
mod tests {
    use super::FailureReportSort;

    #[test]
    fn failure_report_sort_order_contains_order_by() {
        for s in [
            FailureReportSort::RebuildImpact,
            FailureReportSort::OldestFailureFirst,
            FailureReportSort::FewMaintainersHighRebuild,
        ] {
            assert!(
                s.order_clause().contains("ORDER BY"),
                "sort {:?} missing ORDER BY",
                s
            );
        }
    }
}
