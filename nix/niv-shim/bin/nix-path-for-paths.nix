# look at ./nix-path-via-niv.sh for a usage
paths:
# e.g.:
# ```nix
# paths = [
#   { src = "nixos"; name = "nixpkgs"; }
#   { src = "nixos-hardware"; }
# ];
# ```
let
  nix-path-for = import ./nix-path-for.nix;
in
nix-path-for paths
# e.g.:
# nixpkgs=/nix/store/...-nixos-src:nixos-hardware=/nix/store/...-nixos-hardware-src
