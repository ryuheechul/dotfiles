#!/usr/bin/env sh

set -x

# If you are on linux, do this first before ./bootstrap.sh!
# Either machine or human can run this script (there should be no interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../ || exit

# install nix
./nix/bin/install.sh

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

# skip brew installation for now since nix is replacing it

exit 0

[ -d /home/linuxbrew/.linuxbrew/bin ] && PATH=/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:$PATH

# install homebrew
if ! [ -x "$(command -v brew)" ]; then
    echo "brew not found so installing"
    sleep 1
    yes | /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    exit
else
    echo "brew exists, so skipping installation"
fi

# install via Brewfile
if [ -z "${SKIP_INSTALL_BREW}" ]; then
  brew update --verbose
  brew bundle --verbose --file brew/Brewfile

  # fzf shell integration to enable history and directory search

  yes | $(brew --prefix fzf)/install
fi
