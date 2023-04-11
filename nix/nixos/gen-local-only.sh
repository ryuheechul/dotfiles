#!/usr/bin/env bash

script_d="$(dirname "$0")"

cat <<EOF >> "${script_d}/system-pkgs-local.nix"
{ pkgs }:

with pkgs;
[
]
EOF
