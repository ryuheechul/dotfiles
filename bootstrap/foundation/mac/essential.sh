#!/usr/bin/env bash

# set -x # uncomment this for debugging

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../../ || exit


####### Nix #######

# install nix
./nix/bin/install/darwin.sh

# source nix profile only for this script to work
. ~/.nix-profile/etc/profile.d/nix.sh

# init channels
./nix/bin/channels.sh

# set up a local ~/.config/nixpkgs/home.nix
nix-shell -p coreutils --run ./nix/bin/init-home-manager.sh

if [ -x "$(command -v nix)" ]; then
  SKIP_BREW_BUNDLE=1
  home-manager switch
fi

####### brew #######

[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# install homebrew
if ! command -v brew &> /dev/null; then
    echo "brew not found so installing"
    sleep 1
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    echo "brew exist, so skipping installation"
fi

# in case of apple silicon because:
# - nix currently only run with Rosetta 2 
# - at least terminal and shell should run natively 
# - to prevent being forced to run only Intel based apps as child processes of the shell and terminal
if test "arm64" = "$(arch)"; then
  brew install zsh bash tmux
fi

####### Brewfile #######

# install via Brewfile
if [ -z "${SKIP_BREW_BUNDLE}" ]; then
  brew update --verbose
  brew bundle --verbose --file brew/Brewfile

  # fzf shell integration to enable history and directory search
  yes | $(brew --prefix fzf)/install
fi


####### system #######
set -x

defaults read -g TISRomanSwitchState
echo "use the Caps Lock key to switch between input sources via setting it to 1"

defaults read -g "com.apple.trackpad.scaling"
echo "change trackpad speed via changing it to 2.5"

echo "allow three fingers to drag in Accessibility > Pointer Control > Trackpad Options"

echo "reading key repeat values before set"
defaults read -g InitialKeyRepeat
defaults read -g KeyRepeat

# leave it to normal since that works better for me now
# defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
# defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)

# enable repeating in VSCode on macOS - https://github.com/VSCodeVim/Vim
# defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
defaults write -g ApplePressAndHoldEnabled -bool false

echo "keyboard repeating, press and hold is set, you may want to restart applications"

set +x
sleep 2

# iCloudDrive symlink
ln -sf ~/Library/Mobile\ Documents/com~apple~CloudDocs/ ~/icd

# to prevent loading zsh stuff twice in case tmux is the default shell command for terminal emulators
# but skip in ssh - https://unix.stackexchange.com/a/9606
cat <<EOF >> ~/.zshrc
if [ -n "\$SSH_CLIENT" ] || [ -n "\$SSH_TTY" ] || [ -n "\$SSH_CONNECTION" ] ; then
  true
else
  export HOST_ALWAYS_USE_TMUX=1
fi
EOF

echo "All done with this script!"

echo "You may continue the rest with $(readlink -f ./bootstrap/configuration.sh)"

echo "Also optionally $(readlink -f ./bootstrap/foundation/mac/extra.sh)"
