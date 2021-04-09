#!/usr/bin/env sh

set -x

# If you are on linux on Windows via WSL2, do this first before ./bootstrap.sh!
# Human can run this script as there will be interactions via prompt

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../ || exit

# install nix
./nix/bin/install/single-user.sh

sudo ./nix/bin/shim/path.sh

# enable nix for the rest of script
. ~/.nix-profile/etc/profile.d/nix.sh
. /etc/profile.d/user-shim-for-nix-path.sh

# init channels
./nix/bin/channels.sh

# set up local ~/.config/nixpkgs/home.nix
./nix/bin/init-home-manager.sh

# install packages for current user
home-manager switch
