# based on the example code from the link below
# https://github.com/NixOS/nixpkgs/blob/2387396258eb6ff2c436a6a1a5ba5bd05c6862d3/pkgs/tools/nix/nixpkgs-fmt/default.nix

# more info on rust + nix can be found at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md

{ pkgs }:

with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "hired";
  version = "0.9.0-alpha4";
  src = fetchFromGitHub {
    owner = "sidju";
    repo = pname;
    rev = "ccc02c192c9f0b96662bf18709b49d8b9d2d53b5";
    sha256 = "sha256-VP6NQltnQ6kFmYO6IIbwejJ6x4wDhnWvgtA4Pf/K8Yk=";
    fetchSubmodules = true;
  };

  cargoSha256 = "sha256-LJV0oW3F7IIF5ar3o7X04I+AT42tJXN0oAVlt10KVjw=";
}
