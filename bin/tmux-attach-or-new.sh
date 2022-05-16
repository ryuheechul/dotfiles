#!/usr/bin/env bash

## set path for nix otherwise tmux server can't access some binaries like `fpp`

if [ -z "$(command -v nix)" ]; then
  [ -f ~/.nix-profile/etc/profile.d/nix.sh ] && \
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if [ -z "$(command -v nix)" ]; then
  [ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ] && \
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

# let brew (binaries) takes more priority then nix in case of apple silicon
if test "Darwin" = "$(uname)" && test "arm64" = "$(arch)"; then
  [ -x /opt/homebrew/bin/brew ] && eval $(/opt/homebrew/bin/brew shellenv)
fi

SHELL=$(which zsh)
# set it `default` as default sessions name when no name is given
session_name="${1:-default}"

# recommended via https://stackoverflow.com/a/49134974/1570165
tmux new-session -A -s "${session_name}"
