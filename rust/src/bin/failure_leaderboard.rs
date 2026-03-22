//! Emit a JSON document of failing packages ranked by leaderboard score (stdin: none; needs DATABASE_URL).

use nixpkgs_update::establish_connection;
use nixpkgs_update::leaderboard::{fetch_failure_leaderboard, leaderboard_document};

fn usage() {
    eprintln!(
        "Usage: failure-leaderboard [--limit N]\n\
\n\
Reads DATABASE_URL. Prints one JSON object with schema_version, weights, and packages[].\n\
Default --limit is 500."
    );
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.iter().any(|a| a == "-h" || a == "--help") {
        usage();
        return;
    }

    let mut limit: i64 = 500;
    let mut i = 0usize;
    while i < args.len() {
        if args[i] == "--limit" {
            if i + 1 >= args.len() {
                eprintln!("--limit requires a number");
                std::process::exit(2);
            }
            limit = args[i + 1].parse().unwrap_or_else(|_| {
                eprintln!("invalid --limit value");
                std::process::exit(2);
            });
            if limit <= 0 || limit > 500_000 {
                eprintln!("--limit must be between 1 and 500000");
                std::process::exit(2);
            }
            i += 2;
        } else {
            eprintln!("unknown argument: {}", args[i]);
            usage();
            std::process::exit(2);
        }
    }

    let conn = &mut establish_connection();
    let rows = fetch_failure_leaderboard(conn, limit).unwrap_or_else(|e| {
        eprintln!("query failed: {e}");
        std::process::exit(1);
    });
    let doc = leaderboard_document(&rows);
    println!("{}", doc.pretty(2));
}
