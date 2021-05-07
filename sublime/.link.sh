#!/usr/bin/env sh

# run this script with nix-shell -p coreutils especially on macOS

set -x

# to be able to be call from anywhere
cd "$(dirname "$0")" || exit

# symlink to be read by zshrc
this_dir_path="$(readlink -f .)"

# keymap
ln -sf "${this_dir_path}"/keymap ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Default\ \(OSX\).sublime-keymap

# settings
ln -sf "${this_dir_path}"/settings ~/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings
