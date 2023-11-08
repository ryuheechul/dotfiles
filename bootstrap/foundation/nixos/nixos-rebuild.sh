#!/usr/bin/env bash

action=${1:-dry-activate}

curr_dir="$(dirname "$0")"
pushd "${curr_dir}"

# - https://stackoverflow.com/questions/74792938/nix-rebuild-switch-cause-fchmod-of-tmp-x11-unix-failed-read-only-file-syst
test -n "${WSL_DISTRO_NAME}" && sudo mount -o remount,rw /tmp/.X11-unix

# to support hardwares
sudo nix-channel --list | grep nixos-hardware > /dev/null || {
  sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/627bc9b.tar.gz nixos-hardware
  sudo nix-channel --update
}

./gen-configuration.sh

# to prevent CPU being too busy during paralleled compilations (happened with building a custom kernel)
# I rather take time than compilations taking complete control especially with low resource machines
# https://nixos.org/manual/nix/stable/advanced-topics/cores-vs-jobs.html
nix_build_cores="$(getconf _NPROCESSORS_ONLN | xargs -I _ expr _ / 2)"

sudo NIX_BUILD_CORES=${nix_build_cores} nixos-rebuild "${action}" --max-jobs 1 -I nixos-config=./configuration.nix

# Troubleshooting:
# - use https://gitlab.com/khumba/nvd to see the diff between generations
