#!/usr/bin/env bash

set -x

init-channel()
{
  url="${1}"
  name="${2}"

  # skip if it's already added
  candidate="$(nix-channel --list | awk '{print $1;}' | grep "${name}")"
  test "${name}" = "${candidate}" && { echo "${name} channel already exist"; return 0; }

  # add and unpack channel to init when it has not been added yet
  echo "adding and unpacking ${name} channel"
  nix-channel --add "${url}" "${name}" && nix-channel --update "${name}"
}

# main channel - nixpkgs
init-channel https://nixos.org/channels/nixpkgs-unstable nixpkgs

# darwin channel - nixpkgs-stable-darwin for fallback purposes - see ../pkgs/fallback for details
case "$(uname -s)" in
  Darwin*) init-channel https://nixos.org/channels/nixpkgs-22.05-darwin nixpkgs-stable-darwin
esac

# channel for home-manager
init-channel https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
