#!/usr/bin/env bash

# might want to run this in case one from ncurses is not satisfying
/usr/bin/tic -x "$(nix-outpath emacs)/share/emacs/28.1/etc/e/eterm-color.ti"
