pub fn latest_version(project_name: &String) -> Result<json::JsonValue, &'static str> {
    let body = ureq::get(&format!(
        "https://repology.org/api/v1/project/{}",
        project_name
    ))
    .set("User-Agent", "nixpkgs-update")
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
