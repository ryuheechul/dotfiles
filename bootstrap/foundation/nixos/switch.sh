#!/usr/bin/env bash

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

./gen-files.sh
sudo nixos-rebuild switch -I nixos-config=./configuration.nix
