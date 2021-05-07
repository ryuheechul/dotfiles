#!/usr/bin/env sh

# run this script with nix-shell -p coreutils especially on macOS

set -x

# to be able to be call from anywhere
cd "$(dirname "$0")" || exit

echo $(dirname $0)
# symlink to be read by zshrc
this_dir_path="$(readlink -f .)"

rm -rf ~/.config/karabiner
ln -sf "${this_dir_path}" ~/.config/karabiner

