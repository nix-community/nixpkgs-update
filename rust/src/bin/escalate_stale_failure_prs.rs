//! Set `escalated_at` for rows with an open failed-update WIP PR and stale `last_update_failure_at`.

use diesel::sql_query;
use diesel::sql_types::BigInt;
use diesel::QueryableByName;
use diesel::RunQueryDsl;
use nixpkgs_update::establish_connection;

fn usage() {
    eprintln!(
        "Usage: escalate-stale-failure-prs --days N [--dry-run]\n\
\n\
Reads DATABASE_URL. Updates packages where:\n\
  - last_update_failure_kind IS NOT NULL\n\
  - failed_update_pr IS NOT NULL\n\
  - escalated_at IS NULL\n\
  - last_update_failure_at older than N days (SQLite datetime comparison)\n\
\n\
Default N is 14. Use --dry-run to print the count only."
    );
}

#[derive(QueryableByName)]
struct Cnt {
    #[diesel(sql_type = BigInt)]
    c: i64,
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.iter().any(|a| a == "-h" || a == "--help") {
        usage();
        return;
    }

    let dry_run = args.iter().any(|a| a == "--dry-run");
    let mut days: i64 = 14;
    let mut i = 0usize;
    while i < args.len() {
        match args[i].as_str() {
            "--dry-run" => {
                i += 1;
            }
            "--days" => {
                if i + 1 >= args.len() {
                    eprintln!("--days requires a number");
                    std::process::exit(2);
                }
                days = args[i + 1].parse().unwrap_or_else(|_| {
                    eprintln!("invalid --days value");
                    std::process::exit(2);
                });
                if days < 0 {
                    eprintln!("--days must be non-negative");
                    std::process::exit(2);
                }
                i += 2;
            }
            other => {
                eprintln!("unknown argument: {other}");
                usage();
                std::process::exit(2);
            }
        }
    }

    let conn = &mut establish_connection();

    let count_sql = format!(
        "SELECT COUNT(*) AS c FROM packages WHERE last_update_failure_kind IS NOT NULL \
         AND failed_update_pr IS NOT NULL AND escalated_at IS NULL \
         AND last_update_failure_at IS NOT NULL \
         AND datetime(last_update_failure_at) < datetime('now', '-{days} days')"
    );

    let pending: i64 = sql_query(&count_sql)
        .load::<Cnt>(conn)
        .map(|v| v.into_iter().next().map(|r| r.c).unwrap_or(0))
        .unwrap_or_else(|e| {
            eprintln!("count query failed: {e}");
            std::process::exit(1);
        });

    if dry_run {
        println!("would_escalate_rows\t{pending}");
        return;
    }

    let update_sql = format!(
        "UPDATE packages SET escalated_at = datetime('now') \
         WHERE last_update_failure_kind IS NOT NULL \
         AND failed_update_pr IS NOT NULL AND escalated_at IS NULL \
         AND last_update_failure_at IS NOT NULL \
         AND datetime(last_update_failure_at) < datetime('now', '-{days} days')"
    );

    let n = sql_query(&update_sql).execute(conn).unwrap_or_else(|e| {
        eprintln!("update failed: {e}");
        std::process::exit(1);
    });
    println!("escalated_rows\t{n}");
}
