use std::process::Command;

pub fn eval(branch: &str, attr_path: &String, apply: &str) -> Option<String> {
    let output = Command::new("nix")
        .arg("eval")
        .arg("--raw")
        .arg("--refresh")
        .arg(&format!("github:nixos/nixpkgs/{}#{}", branch, attr_path))
        .arg("--apply")
        .arg(apply)
        .output();
    match output {
        Ok(output) => Some(String::from_utf8_lossy(&output.stdout).to_string()),
        Err(_) => None,
    }
}
