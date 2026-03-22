//! Print packages with a recorded update failure (see `doc/package-tracking-db.md`).

use chrono::NaiveDateTime;
use json::JsonValue;
use nixpkgs_update::{
    establish_connection, fetch_packages_with_failures_sorted, FailureReportSort,
    PackageFailureReport,
};

fn usage() {
    eprintln!(
        "Usage: list-package-failures [--json] [--sort MODE]\n\
\n\
Reads DATABASE_URL (SQLite path). Prints rows where last_update_failure_kind is set.\n\
Default: tab-separated columns. With --json: one JSON object per line.\n\
\n\
  --sort rebuild     High rebuild count and recent failures first (default).\n\
  --sort staleness   Oldest failure_at first (longest stuck).\n\
  --sort maintainers Fewest declared maintainers first, then by rebuild count."
    );
}

fn parse_sort(args: &[String]) -> FailureReportSort {
    let mut mode = FailureReportSort::default();
    let mut i = 0usize;
    while i < args.len() {
        if args[i] == "--sort" {
            if i + 1 >= args.len() {
                eprintln!("--sort requires rebuild, staleness, or maintainers");
                std::process::exit(2);
            }
            mode = match args[i + 1].as_str() {
                "rebuild" => FailureReportSort::RebuildImpact,
                "staleness" => FailureReportSort::OldestFailureFirst,
                "maintainers" => FailureReportSort::FewMaintainersHighRebuild,
                other => {
                    eprintln!(
                        "unknown --sort value: {other} (use rebuild, staleness, or maintainers)"
                    );
                    std::process::exit(2);
                }
            };
            i += 2;
        } else {
            i += 1;
        }
    }
    mode
}

fn json_line(r: &PackageFailureReport) -> JsonValue {
    json::object! {
        "attr_path" => r.attr_path.as_str(),
        "last_update_failure_kind" => r.last_update_failure_kind.as_deref().unwrap_or(""),
        "last_update_failure_message" => r.last_update_failure_message.as_deref().unwrap_or(""),
        "last_update_failure_at" => r.last_update_failure_at.map(|t| t.format("%Y-%m-%dT%H:%M:%S").to_string()).unwrap_or_default(),
        "last_update_attempt" => r.last_update_attempt.map(|t| t.format("%Y-%m-%dT%H:%M:%S").to_string()).unwrap_or_default(),
        "declared_maintainer_count" => r.declared_maintainer_count,
        "last_rebuild_path_count" => r.last_rebuild_path_count,
        "install_estimate" => r.install_estimate,
        "version_repology" => r.version_repology.as_deref().unwrap_or(""),
        "version_nixpkgs_master" => r.version_nixpkgs_master.as_deref().unwrap_or(""),
    }
}

fn fmt_time(t: Option<NaiveDateTime>) -> String {
    t.map(|x| x.format("%Y-%m-%d %H:%M").to_string())
        .unwrap_or_default()
}

fn sanitize_tsv_cell(s: &str) -> String {
    s.chars()
        .map(|c| if matches!(c, '\t' | '\n') { ' ' } else { c })
        .collect()
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.iter().any(|a| a == "--help" || a == "-h") {
        usage();
        return;
    }
    let as_json = args.iter().any(|a| a == "--json");
    let sort = parse_sort(&args);

    let conn = &mut establish_connection();
    let rows = fetch_packages_with_failures_sorted(conn, sort).unwrap_or_else(|e| {
        eprintln!("query failed: {e}");
        std::process::exit(1);
    });

    if as_json {
        for r in &rows {
            println!("{}", json_line(r));
        }
    } else {
        println!(
            "attr_path\tfailure_kind\tfailure_at\tlast_attempt\tmaintainers\trebuild_paths\tinstall_estimate\trepology\tnixpkgs_master\tmessage_preview"
        );
        for r in &rows {
            let preview = r
                .last_update_failure_message
                .as_deref()
                .map(|s| {
                    let max = 200;
                    if s.chars().count() <= max {
                        sanitize_tsv_cell(s)
                    } else {
                        let truncated: String = s.chars().take(max).collect();
                        format!("{}...", sanitize_tsv_cell(&truncated))
                    }
                })
                .unwrap_or_default();
            println!(
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                r.attr_path,
                r.last_update_failure_kind.as_deref().unwrap_or(""),
                fmt_time(r.last_update_failure_at),
                fmt_time(r.last_update_attempt),
                r.declared_maintainer_count
                    .map(|n| n.to_string())
                    .unwrap_or_default(),
                r.last_rebuild_path_count
                    .map(|n| n.to_string())
                    .unwrap_or_default(),
                r.install_estimate
                    .map(|n| n.to_string())
                    .unwrap_or_default(),
                r.version_repology.as_deref().unwrap_or(""),
                r.version_nixpkgs_master.as_deref().unwrap_or(""),
                preview
            );
        }
    }
}
