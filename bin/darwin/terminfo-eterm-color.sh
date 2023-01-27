#!/usr/bin/env bash

emacs_path="$(nix-outpath emacs)"
# https://stackoverflow.com/a/3162500/1570165
emacs_ver="${emacs_path##*-}"

# might want to run this in case one from ncurses is not satisfying
/usr/bin/tic -x "${emacs_path}/share/emacs/${emacs_ver}/etc/e/eterm-color.ti"

echo 'A "proper" `eterm-color` terminfo on this system (darwin) was missing and now it is installed by this script.'
echo 'You should restart the shell/terminal to apply the change.'
