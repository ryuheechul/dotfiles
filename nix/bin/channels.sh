#!/usr/bin/env bash

set -x

# main channel - nixpkgs
nix-channel --add https://nixos.org/channels/nixpkgs-unstable

# darwin channel -  nixpkgs-fallback-darwin
case "$(uname -s)" in
  Darwin*) nix-channel --add https://nixos.org/channels/nixpkgs-20.09-darwin nixpkgs-fallback-darwin
esac

# channel for home-manager
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager

# update all the channels
nix-channel --update
