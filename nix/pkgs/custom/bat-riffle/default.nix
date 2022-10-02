# based on the example code from the link below
# https://github.com/NixOS/nixpkgs/blob/2387396258eb6ff2c436a6a1a5ba5bd05c6862d3/pkgs/tools/nix/nixpkgs-fmt/default.nix

# more info on rust + nix can be found at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md

{ pkgs }:

with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "riffle";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "sharkdp";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-6xvg76js+CHqn9Ia5DVU2ILTCQlzpA4lEhDVTrXWLRw=";
  };

  # # actually no need to override this
  # cargoBuildFlags = [ "--bin=bat-riffle" ];

  cargoPatches = [
    ./update-cargo-lock.patch
    ./update-cargo-toml.patch
  ];

  cargoSha256 = "sha256-FW7qOJhFIsoRMfdCFpk0kIYHv0Yo30fhbvsFmD0RC6A=";
}
