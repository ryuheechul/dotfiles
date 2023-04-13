#!/usr/bin/env bash

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

./switch-nixos.sh

# home-manager init and switch ("idempotent")
../../../nix/bin/channels.sh
../../../nix/bin/init-home-manager.sh
home-manager switch
