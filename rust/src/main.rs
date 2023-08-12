use diesel::prelude::*;
use nixpkgs_update::models::*;
use nixpkgs_update::*;
use chrono::offset::Utc;

fn fetch_repology(project_name: String) -> Result<json::JsonValue, &'static str> {
    let body = ureq::get(&format!(
        "https://repology.org/api/v1/project/{}",
        project_name
    ))
    .call()
    .unwrap()
    .into_string()
    .unwrap();
    let json = json::parse(&body).unwrap();
    match json {
        json::JsonValue::Array(projects) => {
            for project in projects {
                match project {
                    json::JsonValue::Object(project_repo) => {
                        if project_repo["status"] == "newest" {
                            return Ok(project_repo.get("version").unwrap().clone());
                        }
                    }
                    _ => continue,
                }
            }
        }
        _ => (),
    }
    Err("Couldn't find")
}

fn main() {
    use nixpkgs_update::schema::packages::dsl::*;

    let connection = &mut establish_connection();
    let results: Vec<Package> = packages.load(connection).expect("Error loading packages");

    println!("Displaying {} packages", results.len());
    for package in results {
        println!("{} {}", package.id, package.attr_path);
        let result : String = fetch_repology(package.attr_path).unwrap().to_string();
        println!("newest repology version {}", result);
        diesel::update(packages.find(package.id))
            .set((
                version_repology.eq(Some(result)),
                last_checked_repology.eq(Some(Utc::now().naive_utc())),
             ))
            .execute(connection)
            .unwrap();
    }
}
