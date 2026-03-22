pub mod db_report;
pub mod leaderboard;
pub mod models;
pub mod schema;

pub use db_report::{
    fetch_packages_with_failures, fetch_packages_with_failures_sorted, FailureReportSort,
    PackageFailureReport,
};

use diesel::prelude::*;
use std::env;

pub fn establish_connection() -> SqliteConnection {
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    SqliteConnection::establish(&database_url)
        .unwrap_or_else(|_| panic!("Error connecting to {}", database_url))
}
