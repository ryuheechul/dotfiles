# for now it's only a testing purpose with `nix-shell`
{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = import ./pkgs {pkgs=pkgs;} ++ import ./build-deps.nix {pkgs=pkgs;};
}
