# based on the example code from the link below
# https://github.com/NixOS/nixpkgs/blob/2387396258eb6ff2c436a6a1a5ba5bd05c6862d3/pkgs/tools/nix/nixpkgs-fmt/default.nix

# more info on rust + nix can be found at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md

{ pkgs }:

with pkgs;
rustPlatform.buildRustPackage rec {
  # https://github.com/jmmv/ssh-agent-switcher/
  pname = "ssh-agent-switcher";
  version = "ssh-agent-switcher-1.0.2";
  src = fetchFromGitHub {
    owner = "jmmv";
    repo = pname;
    rev = version;

    hash = "sha256-XAIupGVU8D4tmZXZ3/5lKiHbvBlxgNQXL0T9Htp7Zmo=";
  };
  cargoHash = "sha256-dbeUye20E2nQcJPyUCpZT68T95dopgoIlBm8rOoaZ6Y=";
}
