#!/usr/bin/env bash

## set path for nix otherwise tmux/zellij server can't access some binaries like `fpp`

if [ -z "$(command -v nix)" ]; then
  [ -f ~/.nix-profile/etc/profile.d/nix.sh ] &&
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if [ -z "$(command -v nix)" ]; then
  [ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ] &&
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

# let brew (binaries) takes more priority then nix in case of apple silicon
if test "Darwin" = "$(uname)" && test "arm64" = "$(uname -m)"; then
  [ -x /opt/homebrew/bin/brew ] && eval $(/opt/homebrew/bin/brew shellenv)
  true
fi

SHELL=$(which zsh)
# set it `default` as default sessions name when no name is given
session_name="${1:-default}"
exec zellij a -c "${session_name}"
