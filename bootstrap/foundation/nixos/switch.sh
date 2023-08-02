#!/usr/bin/env bash

set -e

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

./nixos-rebuild.sh switch

# home-manager init and switch ("idempotent")
../../../nix/bin/channels.sh
../../../nix/bin/init-home-manager.sh

echo ${NIX_PATH} | grep ${USER} && home-manager switch || {
  export NIX_PATH="${HOME}/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH"
  home-manager switch
}
