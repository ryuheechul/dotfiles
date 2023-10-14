# to assist building things on the fly when installing plugins
# search `nix-shell` at ../lua/plugins to see usages

{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  nativeBuildInputs = import ../../nix/build-deps.nix { } ++ [
    rustc
    cargo
  ];

  # thanks to https://github.com/bvaisvil/zenith/issues/19#issuecomment-635515703
  CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER = "gcc";
}
