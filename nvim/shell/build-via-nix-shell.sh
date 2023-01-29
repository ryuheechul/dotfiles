#!/usr/bin/env bash

# basically simplify this a bit of the snippet from "nix-shell --command 'command that build' ~/.config/dfs-rhc/nvim/shell/shell.nix" to below
# "np-build-via-nix-shell 'command that build'" also thanks to ../../zsh/aliases

script_d="$(dirname "$0")"

nix-shell --command "${@}" "${script_d}/shell.nix"
