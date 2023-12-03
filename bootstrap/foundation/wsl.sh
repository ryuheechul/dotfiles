#!/usr/bin/env sh

set -x

# If you are on linux on Windows via WSL2, do this first before ./bootstrap.sh!
# Human can run this script as there will be interactions via prompt

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../ || exit

# install nix
./nix/bin/install/single-user.sh

# enable nix for the rest of script
. ~/.nix-profile/etc/profile.d/nix.sh

# home-manager init and switch ("idempotent")
./nix/bin/init-hm.sh

# install packages for current user
./nix/bin/hm.sh switch

echo "You may continue the rest with $(readlink -f ./bootstrap/configuration.sh)"
