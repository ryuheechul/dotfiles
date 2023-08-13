#!/usr/bin/env bash

set -e

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

./nixos-rebuild.sh switch

# assume that I only want to run `home-manager switch` for bootstrapping time only
# since I'm almost always in TMUX or ZELLIJ after bootstrapping,
# this is a good way to skip `home-manager switch` when I only want to rebuild nixos
# this is not perfect but I don't actually care to run `home-manager switch`
# in some cases if it's rare enough
# don't apply this filter when it's on a SSH connection
printenv TMUX ZELLIJ | xargs test -z \
  || printenv SSH_CLIENT > /dev/null \
  || exit 0

# home-manager init and switch ("idempotent")
../../../nix/bin/channels.sh
../../../nix/bin/init-home-manager.sh

echo ${NIX_PATH} | grep ${USER} && home-manager switch || {
  # fallback to set path to include channels in case not added yet
  export NIX_PATH="${HOME}/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH"
  home-manager switch
}
