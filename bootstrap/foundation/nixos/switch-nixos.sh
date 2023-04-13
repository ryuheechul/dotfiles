#!/usr/bin/env bash

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

# - https://stackoverflow.com/questions/74792938/nix-rebuild-switch-cause-fchmod-of-tmp-x11-unix-failed-read-only-file-syst
test -n "${WSL_DISTRO_NAME}" && sudo mount -o remount,rw /tmp/.X11-unix

./gen-configuration.sh
sudo nixos-rebuild switch -I nixos-config=./configuration.nix

# Troubleshooting:
# - use https://gitlab.com/khumba/nvd to see the diff between generations
