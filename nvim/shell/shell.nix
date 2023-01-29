# to assist building things on the fly when installing plugins
# search `nix-shell` at ../lua/plugins to see usages

{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  nativeBuildInputs = import ../../nix/build-deps.nix { } ++ [
    rustc
    cargo
  ];
}
