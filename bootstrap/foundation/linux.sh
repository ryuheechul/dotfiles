#!/usr/bin/env sh

set -e
set -x

# If you are on linux, do this first before ./bootstrap.sh!
# Either machine or human can run this script (there should be no interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../ || exit

# install nix
./nix/bin/install/multi-user.sh

sudo ./nix/bin/shim/path.sh

# enable nix for the rest of script
. /etc/profile.d/nix.sh
. /etc/profile.d/user-shim-for-nix-path.sh

# init channels
./nix/bin/channels.sh

# set up local ~/.config/nixpkgs/home.nix
./nix/bin/init-home-manager.sh

# install packages for current user
home-manager switch

echo "You may continue the rest with $(readlink -f ./bootstrap/configuration.sh)"
