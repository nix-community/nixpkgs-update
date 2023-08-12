use crate::nix;

fn token() -> Option<String> {
    if let Ok(token) = std::env::var("GH_TOKEN") {
        return Some(token);
    }
    if let Ok(token) = std::env::var("GITHUB_TOKEN") {
        return Some(token);
    }
    None
}

pub fn latest_release(github: &Github) -> Result<json::JsonValue, &'static str> {
    let mut request = ureq::get(&format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        github.owner, github.repo,
    ))
    .set("Accept", "application/vnd.github+json")
    .set("X-GitHub-Api-Version", "2022-11-28");

    if let Some(token) = token() {
        request = request.set("Authorization", &format!("Bearer {}", token));
    }

    let body = request.call().unwrap().into_string().unwrap();
    if let json::JsonValue::Object(response) = json::parse(&body).unwrap() {
        return Ok(response["tag_name"].clone());
    }
    Err("Couldn't find")
}

pub struct Github {
    pub owner: String,
    pub repo: String,
}

pub fn from(attr_path: &String) -> Option<Github> {
    let url = nix::eval("master", attr_path, "(drv: drv.src.url)").unwrap();
    if !url.contains("github") {
        return None;
    }
    let owner = nix::eval("master", attr_path, "(drv: drv.src.owner)").unwrap();
    let repo = nix::eval("master", attr_path, "(drv: drv.src.repo)").unwrap();
    Some(Github {
        owner: owner.to_string(),
        repo: repo.to_string(),
    })
}
