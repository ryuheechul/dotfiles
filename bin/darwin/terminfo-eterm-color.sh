#!/usr/bin/env bash

# might want to run this in case one from ncurses is not satisfying
/usr/bin/tic -x "$(nix-outpath emacs)/share/emacs/28.1/etc/e/eterm-color.ti"

echo 'A "proper" `eterm-color` terminfo on this system (darwin) was missing and now it is installed by this script.'
echo 'Restart the shell/terminal to take effect'
