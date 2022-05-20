# this is not expected to be installed via home-manager as it wouldn't do much
# but running `nix-shell` in this directory should include this - check ./shell.nix for the truth

# these are the ones that should be listed at `nativeBuildInputs` of shell.nix for any project list its own specific dependencies.
# I don't expect other shell.nix to import this file but that can be done if it's for quick resolvement of dependencies for one-off works
# i.e. debugging an arbitrary python project for one-off occasion

# packages here might overlap with ./pkgs/default.nix as they are being used a bit differently:
# - ./pkgs/default.nix: let home-manager to install packages to expose binaries
# - ./build-deps.nix: let build dependencies available to help build

# this also could serve a purpose of documenting what libraries are frequently used

{ pkgs ? import <nixpkgs> {} }:

with pkgs;
[
  pkg-config # a helper tool used when compiling applications and libraries
  libcxx
  openssl
] ++
# darwin specifics
lib.optionals stdenv.isDarwin [
  # this probably prevent many build issues to appear
  # for any other frameworks, go to - https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/darwin/apple-sdk/frameworks.nix
  darwin.apple_sdk.frameworks.CoreServices
]
