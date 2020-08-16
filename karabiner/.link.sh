#!/usr/bin/env sh

set -x

# to be able to be call from anywhere
cd "$(dirname "$0")" || exit

# symlink to be read by zshrc
this_dir_path="$(greadlink -f "$(dirname "$0")")"

ln -s "${this_dir_path}" ~/.config

