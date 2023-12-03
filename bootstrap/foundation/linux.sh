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

# enable nix for the rest of script
. /etc/profile.d/nix.sh

# home-manager init and switch ("idempotent")
./nix/bin/init-hm.sh

# install packages for current user
./nix/bin/hm.sh switch

echo "You may continue the rest with $(readlink -f ./bootstrap/configuration.sh)"
