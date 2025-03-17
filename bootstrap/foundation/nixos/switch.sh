#!/usr/bin/env bash

set -e

curr_d="$(dirname "$0")"
pushd "${curr_d}"

./nixos-rebuild.sh switch

# assume that I only want to run `home-manager switch` for bootstrapping time only
# since I'm almost always in TMUX or ZELLIJ after bootstrapping,
# this is a good way to skip `home-manager switch` when I only want to rebuild nixos
# this is not perfect but I don't actually care to run `home-manager switch`
# in some cases if it's rare enough
# don't apply this filter when it's on a SSH connection
printenv TMUX ZELLIJ | xargs test -z ||
  printenv SSH_CLIENT >/dev/null ||
  exit 0

nix_d="../../../nix"
# home-manager init and switch ("idempotent")
"${nix_d}/bin/init-hm.sh"
"${nix_d}/bin/hm.sh" switch
