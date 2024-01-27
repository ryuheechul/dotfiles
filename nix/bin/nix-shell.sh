#!/usr/bin/env bash

# this is to be used regardless of the bootstrapping happened or not

curr_d="$(dirname "$0")"
ns_wrapper="${curr_d}/../../bin/path/default/nix-shell"
"${ns_wrapper}" "$@"
