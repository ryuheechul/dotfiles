#!/usr/bin/env bash

set -e
# set -x # uncomment this for debugging

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../../ || exit


####### Nix #######

## TODO: add a step to workaround absence of T2 chip and in the meantime read below to workaround manually
# check if filevault is enabled (most likely with non-T2 mac) - https://osxdaily.com/2013/11/25/check-filevault-status-command-line-mac-os-x/
# `fdesetup status == `FileVault is On.`
# because it will fail to install nix
# you can workaround that by creating volume and mount it to `/nix` manually
# create volume by `Disk Utility` GUI app
# `disktuil list` to check what is map to `/dev/disk1sX`
# `mount_apfs /dev/disk1sX /nix`
# and you should be good to go
## end of TODO

[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ] && . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

# install nix
if ! command -v nix &> /dev/null
then
  ./nix/bin/install/darwin.sh

  # source nix profile only for this script to work
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

# home-manager init and switch ("idempotent")
./nix/bin/nix-shell.sh -p coreutils --run ./nix/bin/init-hm.sh

if [ -x "$(command -v nix)" ]; then
  SKIP_BREW_BUNDLE=1
  ./nix/bin/hm.sh switch
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
if uname -m | xargs test "arm64" =; then
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

defaults read -g TISRomanSwitchState || true
echo "use the Caps Lock key to switch between input sources via setting it to 1"

defaults read -g "com.apple.trackpad.scaling" || true
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
ln -sf "${HOME}/Library/Mobile Documents/com~apple~CloudDocs" "${HOME}/icd"

# fixing for TERM=tmux-256color - thanks to https://gist.github.com/bbqtd/a4ac060d6f6b9ea6fe3aabe735aa9d95
# another article for a same topic - https://gpanders.com/blog/the-definitive-guide-to-using-tmux-256color-on-macos/

echo "Mitigating tmux terminfo"
./bin/path/default/update-terminfo-tmux

echo "All done with this script!"

echo "You may continue the rest with $(readlink -f ./bootstrap/configuration.sh)"

echo "Also optionally $(readlink -f ./bootstrap/foundation/darwin/extra.sh)"
