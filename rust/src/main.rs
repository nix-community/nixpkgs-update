use std::process::Command;
use diesel::prelude::*;
use nixpkgs_update::models::*;
use nixpkgs_update::*;
use chrono::offset::Utc;

fn fetch_repology(project_name: &String) -> Result<json::JsonValue, &'static str> {
    let body = ureq::get(&format!(
        "https://repology.org/api/v1/project/{}",
        project_name
    ))
    .call()
    .unwrap()
    .into_string()
    .unwrap();
    let json = json::parse(&body).unwrap();
    if let json::JsonValue::Array(projects) = json {
        for project in projects {
            if let json::JsonValue::Object(project_repo) = project {
                if project_repo["status"] == "newest" {
                    return Ok(project_repo.get("version").unwrap().clone());
                }
            }
        }
    }
    Err("Couldn't find")
}

fn fetch_github_latest_release(github: &Github) -> Result<json::JsonValue, &'static str> {
    let body = ureq::get(&format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        github.owner,
        github.repo,
    ))
    .call()
    .unwrap()
    .into_string()
    .unwrap();
    if let json::JsonValue::Object(response) = json::parse(&body).unwrap() {
        return Ok(response["tag_name"].clone());
    }
    Err("Couldn't find")
}

fn version_in_nixpkgs_branch(branch: &str, attr_path: &String) -> String {
    let output = Command::new("nix")
        .arg("eval")
        .arg("--raw")
        .arg("--refresh")
        .arg(&format!("github:nixos/nixpkgs/{}#{}", branch, attr_path))
        .arg("--apply")
        .arg("(drv: drv.version)")
        .output()
        .expect(&format!("Failed to execute nix eval {}", attr_path));
    String::from_utf8_lossy(&output.stdout).to_string()
}

fn version_in_nixpkgs_master(attr_path: &String) -> String {
    version_in_nixpkgs_branch("master", &attr_path)
}

fn version_in_nixpkgs_staging(attr_path: &String) -> String {
    version_in_nixpkgs_branch("staging", &attr_path)
}

fn version_in_nixpkgs_staging_next(attr_path: &String) -> String {
    version_in_nixpkgs_branch("staging-next", &attr_path)
}

struct Github {
    owner: String,
    repo: String,
}

fn maybe_github(attr_path: &String) -> Option<Github> {
    let output = Command::new("nix")
        .arg("eval")
        .arg("--raw")
        .arg("--refresh")
        .arg(&format!("github:nixos/nixpkgs/master#{}", attr_path))
        .arg("--apply")
        .arg("(drv: drv.src.url)")
        .output()
        .expect(&format!("Failed to execute nix eval for {}", attr_path));
    if !String::from_utf8_lossy(&output.stdout).contains("github") {
      return None;
    }

    let output = Command::new("nix")
        .arg("eval")
        .arg("--raw")
        .arg("--refresh")
        .arg(&format!("github:nixos/nixpkgs/master#{}", attr_path))
        .arg("--apply")
        .arg("(drv: drv.src.owner)")
        .output()
        .expect(&format!("Failed to execute nix eval for {}", attr_path));

    let owner = String::from_utf8_lossy(&output.stdout);

    let output = Command::new("nix")
        .arg("eval")
        .arg("--raw")
        .arg("--refresh")
        .arg(&format!("github:nixos/nixpkgs/master#{}", attr_path))
        .arg("--apply")
        .arg("(drv: drv.src.repo)")
        .output()
        .expect(&format!("Failed to execute nix eval for {}", attr_path));

    let repo = String::from_utf8_lossy(&output.stdout);

    return Some(Github {
        owner: owner.to_string(),
        repo: repo.to_string(),
    });
}

fn main() {
    use nixpkgs_update::schema::packages::dsl::*;

    let connection = &mut establish_connection();
    let results: Vec<Package> = packages.load(connection).expect("Error loading packages");

    println!("Displaying {} packages", results.len());
    for package in results {
        println!("{} {}", package.id, package.attr_path);

        let result : String = fetch_repology(&package.attr_path).unwrap().to_string();
        println!("newest repology version {}", result);

        let version_in_nixpkgs_master : String = version_in_nixpkgs_master(&package.attr_path);
        println!("nixpkgs master version {}", version_in_nixpkgs_master);

        let version_in_nixpkgs_staging : String = version_in_nixpkgs_staging(&package.attr_path);
        println!("nixpkgs staging version {}", version_in_nixpkgs_staging);

        let version_in_nixpkgs_staging_next : String = version_in_nixpkgs_staging_next(&package.attr_path);
        println!("nixpkgs staging_next version {}", version_in_nixpkgs_staging_next);

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

        if let Some(github) = maybe_github(&package.attr_path) {
            println!("found github for {}", package.attr_path);

            let vgithub : String = fetch_github_latest_release(&github).unwrap().to_string();
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
