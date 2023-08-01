#!/usr/bin/env bash

set -e

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

./nixos-rebuild.sh switch

# home-manager init and switch ("idempotent")
../../../nix/bin/channels.sh
../../../nix/bin/init-home-manager.sh
home-manager switch
