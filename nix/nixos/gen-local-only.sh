#!/usr/bin/env bash

script_d="$(dirname "$0")"

test -f "${script_d}/system-pkgs-local.nix" || cat <<EOF >> "${script_d}/system-pkgs-local.nix"
{ pkgs }:

with pkgs;
[
]
EOF

test -f "${script_d}/mix-and-match.nix" || cp "${script_d}/mix-and-match-template.nix" "${script_d}/mix-and-match.nix"
