#!/usr/bin/env bash

# using this script ensures that `nix-shell` wrapper is used
# in case `nix-shell` path is not exported yet

curr_d="$(dirname "$0")"

# to make sure `export NIX_CONFIG='extra-experimental-features = nix-command flakes'`
source "${curr_d}/../../nix/bin/source/config.sh"

ns_wrapper="${curr_d}/../../bin/path/default/nix-shell"
"${ns_wrapper}" "$@"
