{ lib
, stdenv
, rustPlatform
, nixpkgsUpdateRustTree
, sqlite
, darwin
}:

rustPlatform.buildRustPackage {
  pname = "nixpkgs-update-rust";
  version = "0.1.0";

  src = lib.cleanSource nixpkgsUpdateRustTree;

  cargoLock.lockFile = nixpkgsUpdateRustTree + "/Cargo.lock";

  buildInputs =
    [ sqlite ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin (
      with darwin.apple_sdk.frameworks;
      [ Security SystemConfiguration ]
    );

  meta = with lib; {
    description =
      "Rust SQLite tracker utilities for nixpkgs-update (list-package-failures, failure-leaderboard, escalate-stale-failure-prs)";
    license = licenses.cc0;
    mainProgram = "list-package-failures";
  };
}
