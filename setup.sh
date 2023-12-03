#!/usr/bin/env bash

# This is intended to be used by github codespace as explained in here - https://docs.github.com/en/github/developing-online-with-codespaces/personalizing-codespaces-for-your-account

set -e
set -x

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit

####### nix #######

# install nix
./nix/bin/install/single-user.sh

# source nix
. ${HOME}/.nix-profile/etc/profile.d/nix.sh

# home-manager init and switch ("idempotent")
./nix/bin/init-hm.sh

# install packages for current user
./nix/bin/hm.sh switch

####### configuration #######

# run the rest of personalization
./bootstrap/configuration.sh
