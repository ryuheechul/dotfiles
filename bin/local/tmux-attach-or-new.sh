#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "you will need to provide a session name. Try \`$0 [my-session-name]\`."
  exit 1
fi

# set path for nix otherwise tmux server can't access some binaries like `fpp`
. ~/.nix-profile/etc/profile.d/nix.sh

# let brew (binaries) takes more priority then nix in case of apple silicon
if test "Darwin" = "$(uname)" && test "arm64" = "$(arch)"; then
  [ -x /opt/homebrew/bin/brew ] && \
    eval $(/opt/homebrew/bin/brew shellenv)
fi

SHELL=$(which zsh)
session_name="$1"
# recommended via https://stackoverflow.com/a/3432749
tmux a -t "${session_name}" || tmux new -s "${session_name}"
