//! JSON leaderboard for classified failures.

use chrono::{NaiveDateTime, Utc};
use diesel::prelude::*;
use diesel::sql_query;
use diesel::sql_types::{BigInt, Integer, Nullable, Text, Timestamp};
use json::JsonValue;

/// Bump when the JSON shape or score formula changes (operators and Rust consumers).
pub const LEADERBOARD_SCHEMA_VERSION: i32 = 1;

/// Weight for `consecutive_failures` in the composite score.
pub const WEIGHT_CONSECUTIVE: i64 = 25;
/// Cap applied to `last_rebuild_path_count` before adding to the score.
pub const REBUILD_SCORE_CAP: i64 = 5000;

#[derive(QueryableByName, Debug, Clone)]
pub struct FailureLeaderboardRow {
    #[diesel(sql_type = Text)]
    pub attr_path: String,
    #[diesel(sql_type = Nullable<Text>)]
    pub last_update_failure_kind: Option<String>,
    #[diesel(sql_type = Nullable<Timestamp>)]
    pub last_update_failure_at: Option<NaiveDateTime>,
    #[diesel(sql_type = Integer)]
    pub consecutive_failures: i32,
    #[diesel(sql_type = Nullable<BigInt>)]
    pub last_rebuild_path_count: Option<i64>,
    #[diesel(sql_type = Nullable<BigInt>)]
    pub declared_maintainer_count: Option<i64>,
    #[diesel(sql_type = Nullable<Integer>)]
    pub failed_update_pr: Option<i32>,
    #[diesel(sql_type = BigInt)]
    pub score: i64,
}

fn fmt_ts(t: Option<NaiveDateTime>) -> String {
    t.map(|x| x.format("%Y-%m-%dT%H:%M:%S").to_string())
        .unwrap_or_default()
}

fn row_json(r: &FailureLeaderboardRow) -> JsonValue {
    json::object! {
        "attr_path" => r.attr_path.as_str(),
        "last_update_failure_kind" => r.last_update_failure_kind.as_deref().unwrap_or(""),
        "last_update_failure_at" => fmt_ts(r.last_update_failure_at),
        "consecutive_failures" => r.consecutive_failures,
        "last_rebuild_path_count" => r.last_rebuild_path_count,
        "declared_maintainer_count" => r.declared_maintainer_count,
        "failed_update_pr" => r.failed_update_pr,
        "score" => r.score,
    }
}

/// Rows with an active failure classification, ordered by computed score descending.
pub fn fetch_failure_leaderboard(
    conn: &mut SqliteConnection,
    limit: i64,
) -> QueryResult<Vec<FailureLeaderboardRow>> {
    let q = format!(
        r#"SELECT attr_path, last_update_failure_kind, last_update_failure_at,
                  consecutive_failures, last_rebuild_path_count, declared_maintainer_count,
                  failed_update_pr,
                  (CAST(COALESCE(consecutive_failures, 0) AS INTEGER) * {w} +
                   MIN(COALESCE(last_rebuild_path_count, 0), {cap})) AS score
           FROM packages
           WHERE last_update_failure_kind IS NOT NULL
           ORDER BY score DESC, last_update_failure_at DESC
           LIMIT {lim}"#,
        w = WEIGHT_CONSECUTIVE,
        cap = REBUILD_SCORE_CAP,
        lim = limit
    );
    sql_query(q).load(conn)
}

pub fn leaderboard_document(rows: &[FailureLeaderboardRow]) -> JsonValue {
    let generated = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let pkgs: Vec<JsonValue> = rows.iter().map(row_json).collect();
    json::object! {
        "schema_version" => LEADERBOARD_SCHEMA_VERSION,
        "generated_at_utc" => generated.as_str(),
        "weights" => json::object! {
            "consecutive_failures" => WEIGHT_CONSECUTIVE,
            "rebuild_paths_capped_at" => REBUILD_SCORE_CAP,
        },
        "packages" => JsonValue::Array(pkgs),
    }
}
