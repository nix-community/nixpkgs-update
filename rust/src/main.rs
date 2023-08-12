mod github;
mod nix;
mod repology;

use chrono::offset::Utc;
use diesel::prelude::*;
use nixpkgs_update::models::*;
use nixpkgs_update::*;

fn version_in_nixpkgs_branch(branch: &str, attr_path: &String) -> Option<String> {
    nix::eval(branch, attr_path, "(drv: drv.version)")
}

fn version_in_nixpkgs_master(attr_path: &String) -> Option<String> {
    version_in_nixpkgs_branch("master", attr_path)
}

fn version_in_nixpkgs_staging(attr_path: &String) -> Option<String> {
    version_in_nixpkgs_branch("staging", attr_path)
}

fn version_in_nixpkgs_staging_next(attr_path: &String) -> Option<String> {
    version_in_nixpkgs_branch("staging-next", attr_path)
}

fn main() {
    use nixpkgs_update::schema::packages::dsl::*;

    let connection = &mut establish_connection();
    let results: Vec<Package> = packages.load(connection).expect("Error loading packages");

    println!("Displaying {} packages", results.len());
    for package in results {
        println!("{} {}", package.id, package.attr_path);

        let result: String = repology::latest_version(&package.attr_path)
            .unwrap()
            .to_string();
        println!("newest repology version {}", result);

        let version_in_nixpkgs_master: String =
            version_in_nixpkgs_master(&package.attr_path).unwrap();
        println!("nixpkgs master version {}", version_in_nixpkgs_master);

        let version_in_nixpkgs_staging: String =
            version_in_nixpkgs_staging(&package.attr_path).unwrap();
        println!("nixpkgs staging version {}", version_in_nixpkgs_staging);

        let version_in_nixpkgs_staging_next: String =
            version_in_nixpkgs_staging_next(&package.attr_path).unwrap();
        println!(
            "nixpkgs staging_next version {}",
            version_in_nixpkgs_staging_next
        );

        let now = Some(Utc::now().naive_utc());
        diesel::update(packages.find(&package.id))
            .set((
                version_nixpkgs_master.eq(Some(version_in_nixpkgs_master)),
                last_checked_nixpkgs_master.eq(now),
                version_nixpkgs_staging.eq(Some(version_in_nixpkgs_staging)),
                last_checked_nixpkgs_staging.eq(now),
                version_nixpkgs_staging_next.eq(Some(version_in_nixpkgs_staging_next)),
                last_checked_nixpkgs_staging_next.eq(now),
                version_repology.eq(Some(result)),
                last_checked_repology.eq(now),
            ))
            .execute(connection)
            .unwrap();

        if let Some(github) = github::from(&package.attr_path) {
            println!("found github for {}", package.attr_path);

            let vgithub: String = github::latest_release(&github).unwrap().to_string();
            println!("version github {}", vgithub);
            let now = Some(Utc::now().naive_utc());
            diesel::update(packages.find(&package.id))
                .set((
                    version_github.eq(Some(vgithub)),
                    owner_github.eq(Some(github.owner)),
                    repo_github.eq(Some(github.repo)),
                    last_checked_github.eq(now),
                ))
                .execute(connection)
                .unwrap();
        }
    }
}
