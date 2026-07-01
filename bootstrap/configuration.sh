#!/usr/bin/env sh

# Either machine or human can run this script (there should be no interactions)

# Configuration.sh is platform agnostic and assumes dependant packages are installed via `./foundation/`
# Also make sure to configure right $PATH for this script to work properly

# This script is meant to be idempotent - safe to re-run anytime (e.g. after
# `git pull` on an already-bootstrapped machine) to converge to the latest
# state. Keep it that way: guard any one-off/imperative step (existence
# checks, etc.) instead of adding unconditional appends or unconditional
# destructive recreates. `mise bootstrap` already handles this for anything
# declared in ../mise/home/conf.d/*.toml - only what can't live there (see
# comments below) needs its own guard here.

set -e
set -x

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../ || exit

# enable nix for the rest of script - wrap with `set +e` to be compatible with older version `[da]sh` like the one the macOS
set +e
source ./nix/bin/source/nix.sh
set -e

if ! command -v nix >/dev/null 2>&1; then
  echo 'Warning: `nix` is still not found but trying to run the rest of this script anyway'
fi

# get repo path
if command -v greadlink >/dev/null 2>&1; then
  this_repo_path="$(greadlink -f "$(pwd)")"
elif command -v readlink >/dev/null 2>&1; then
  this_repo_path="$(readlink -f "$(pwd)")"
else
  echo "readlink binary is required to continue"
  exit 1
fi

XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

echo "[INFO] \$XDG_CONFIG_HOME: ${XDG_CONFIG_HOME}"

# make sure ${XDG_CONFIG_HOME} exist
mkdir -p "${XDG_CONFIG_HOME}"

# symlink this repo so it's discoverable no matter where this is located at
# dfs-rhc: shorthand for dotfiles-ryuheechul
# -n so a re-run replaces the symlink itself instead of following it into the
# directory it already points at and dropping a self-referential link inside
ln -sfn "${this_repo_path}" "${XDG_CONFIG_HOME}/dfs-rhc"

dfs_rhc="${XDG_CONFIG_HOME}/dfs-rhc"

# in case my bins are being used in the configuration step already
# - one discovered usage is that `current-base16` is being used in ../nvim/lua/plugins/theme.lua
export PATH="${dfs_rhc}/bin/path/default:${PATH}"

# mise - deploy config first so `mise bootstrap` below can see it; see
# ../mise/README.md for why ../mise/home/ is a single unit symlinked as a
# whole rather than symlinking config.toml and conf.d/ separately.
# clear a real (non-symlink) ~/.config/mise first - -n on ln only guards
# against re-pointing an existing symlink, not a real leftover directory
if test -e "${XDG_CONFIG_HOME}/mise" && test ! -L "${XDG_CONFIG_HOME}/mise"; then
  rm -rf "${XDG_CONFIG_HOME}/mise"
fi
ln -sfn "${dfs_rhc}/mise/home" "${XDG_CONFIG_HOME}/mise"

# clones [bootstrap.repos], applies [dotfiles], installs [tools], then runs
# the `bootstrap` task - see ../mise/home/conf.d/*.toml for what this covers
mise bootstrap --yes || true

## zsh

## "linking" these are now done via ../nix/home/programs/shells.nix
# # source dotfiles' env
# echo "source '${dfs_rhc}/zsh/env'" >> "${HOME}/.zshenv"
# # source dotfiles' zlogin
# echo "source '${dfs_rhc}/zsh/zlogin'" >> "${HOME}/.zlogin"
# # source dotfiles' zshrc
# echo "source '${dfs_rhc}/zsh/zshrc'" >> "${HOME}/.zshrc"

# seed ~/.local.zshrc once - it's meant to be hand-edited after install,
# so this never touches it again once it exists
[ -f "${HOME}/.local.zshrc" ] || cat "${dfs_rhc}/bootstrap/local.zshrc.template" >"${HOME}/.local.zshrc"

echo "configuration.sh seemed to have run successfully!"
