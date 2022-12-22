# can use it to setup dev environment - currently it's for ../lua/packer-groups/debug.lua
{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = import ../../nix/build-deps.nix { };
}
