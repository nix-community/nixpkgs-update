use nixpkgs_update::models::*;
use nixpkgs_update::*;
use diesel::prelude::*;

fn main() {
    use nixpkgs_update::schema::packages::dsl::*;

    let connection = &mut establish_connection();
    let results : Vec<Package> =
        packages.
        load(connection)
        .expect("Error loading packages");

    println!("Displaying {} packages", results.len());
    for package in results {
        println!("{} {}", package.id, package.attr_path);
    }
}
