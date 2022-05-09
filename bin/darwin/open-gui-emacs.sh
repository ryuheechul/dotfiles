#!/usr/bin/env bash

# this script can be called from an Automator app to mitigate https://github.com/nix-community/home-manager/issues/1341
# and put the (Automator) app in either /Applications or ~/Applications so either Spotlight or Alfred can open (or activate the (Emacs) app quickly

open -a ~/.nix-profile/Applications/Emacs.app
