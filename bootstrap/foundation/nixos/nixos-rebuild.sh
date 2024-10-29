#!/usr/bin/env bash

action=${1:-dry-activate}

curr_dir="$(dirname "$0")"
cd "${curr_dir}" || exit

# - https://stackoverflow.com/questions/74792938/nix-rebuild-switch-cause-fchmod-of-tmp-x11-unix-failed-read-only-file-syst
test -n "${WSL_DISTRO_NAME}" && sudo mount -o remount,rw /tmp/.X11-unix

./gen-configuration.sh

# to prevent CPU being too busy during paralleled compilations (happened with building a custom kernel)
# I rather take time than compilations taking complete control especially with low resource machines
# https://nixos.org/manual/nix/stable/advanced-topics/cores-vs-jobs.html
nix_build_cores="$(getconf _NPROCESSORS_ONLN | xargs -I _ expr _ / 2)"

nix_d="../../../nix"
path_for="${nix_d}/niv-shim/bin/nix-path-via-niv.sh"
# to use the source from a deterministic way (powered by niv) instead of relying on a channel
alt_paths="$("${path_for}" nixpkgs=nixos:nixos-hardware)"
alt_config=./configuration.nix

nix_path="${alt_paths}:nixos-config=${alt_config}"

echo "[info] The default 'NIX_PATH=$(sudo printenv NIX_PATH)' will be overridden by..."
echo "[info] '${nix_path}'"

# shim nix-output-monitor since it doesn't exist on bare nixos
function nom {
  ../../../nix/bin/nix-shell.sh -p nix-output-monitor --command "nom"
}

sudo NIX_BUILD_CORES="${nix_build_cores}" nix_path="${nix_path}" action=${action} \
  bash -c 'NIX_PATH="${nix_path}" nixos-rebuild ${action} --max-jobs 1' |& nom
# wrapping once more like above because below doesn't work as $NIX_PATH somehow get overridden
# `sudo NIX_BUILD_CORES="${nix_build_cores}" NIX_PATH="${nix_path}" nixos-rebuild ${action} --max-jobs 1`

# Troubleshooting:
# - use https://gitlab.com/khumba/nvd to see the diff between generations
# - e.g. `nvd diff /run/current-system result`
