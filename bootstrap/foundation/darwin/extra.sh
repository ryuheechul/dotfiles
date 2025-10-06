#!/usr/bin/env bash

# set -x # uncomment this for debugging

# If you are on mac, do this first before ./bootstrap.sh!
# It's assumed that this script will be runned by a human (expect minimal interactions)

# go to repo's root to be able to be call from anywhere
cd "$(dirname "$0")" || exit
cd ../../../ || exit

. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

######## Screenshots ########

mkdir -p ~/Screenshots && defaults write com.apple.screencapture location ~/Screenshots

# remote url local opener - https://github.com/superbrothers/opener
brew install superbrothers/opener/opener
brew services start opener

####### Mac App Store #######
# TODO: give a handle to skip this when not signed in

# mas let you install Mac App Store app with CLI
brew install mas
mas install 869223134  # KakaoTalk
mas install 1480933944 # Vimari
mas install 1142151959 # Just Focus
mas install 803453959  # Slack
mas install 1475387142 # Tailscale
mas install 1480933944 # Vimari
mas install 1284863847 # Unsplash Wallpapers
mas install 441258766  # magnet

######### Brew Cask #########

_is_admin() {
  groups | tr '[:space:]' '\n' | grep '^admin$' >/dev/null
}

_install_cask() {
  brew list --cask $1 || {
    if is_admin; then
      brew install --no-quarantine --cask $1
    else
      brew install --no-quarantine --cask --appdir="${HOME}/Applications" $1
    fi
  }
}

# iterm2
_install_cask iterm2

# for iterm2 and tmux scrolling issue.
# https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022
echo "read https://github.com/NHDaly/tmux-better-mouse-mode/issues/42#issuecomment-579612022 for tmux scrolling issue with iterm"

echo "Go to Profiles > Command > Command and set '~/.config/dfs-rhc/bin/tmux-attach-or-new.sh default'"

sleep 3

_install_cask stats
_install_cask alacritty
_install_cask warp
_install_cask vanilla
_is_admin && _install_cask 1password # since it won't work if it's installed a location other than /Applications
_install_cask amethyst
_install_cask google-chrome
_install_cask zen
_install_cask notion
_install_cask utm
_install_cask orbstack
_install_cask logseq
# _install_cask sublime-text
_install_cask zed
./sublime/.link.sh
_install_cask secretive
_install_cask visual-studio-code
_install_cask karabiner-elements
_install_cask pearcleaner
_install_cask moonlight
./karabiner/.link.sh
# _install_cask alfred
_install_cask raycast
_install_cask ghostty
_install_cask lm-studio
_install_cask claude
_install_cask cursor

echo "run ./karabiner-uk/.link.sh instead if the hardware keyboard layout is British"

sleep 2

######### Keytty #########

# go to Keytty download page
open https://github.com/keytty/shelter/releases

####### ScriptKit ########

# go to ScriptKit download page
open https://www.scriptkit.com/
open https://github.com/ryuheechul/kenv

######### Vimac ##########

# go to vimac and homerow download page
open https://vimacapp.com/
open https://www.homerow.app/

########## font ##########

## now fonts are being installed via ../../../nix/home/default.nix instead of using brew
# # download nerd font patched jetbrains and fira mono fonts
# brew tap homebrew/cask-fonts
# _install_cask font-jetbrains-mono-nerd-font
# _install_cask font-fira-mono-nerd-font

echo "You might want to do the followings:"
# https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
echo "- System Preference > Accessibility > Display > Reduce Motion checkbox"
echo "- System Preference > Keyboard > Adjust repeat speed and delay"
echo "- Open Karabiner Element and allow permissions"
echo "- Open Amethyst to have tiling start working"
echo "- System Preference > Display > change the dominant display if you use multi screen setup"
echo "- Install Keytty from ~/Downloads/Keytty.x.x.x.dmg"
echo "- Open Just Focus to start using Pomodoro technic"
echo '- run `brew services ls` to see if any services you want to run (e.g. syncthing)'

sleep 2

echo "All done in this script!"

echo 'you may want to run `softwareupdate --install-rosetta` to install rosetta'
echo 'you may want to go to ~/.config/dfs-rhc/nix/darwin to utilize nix-darwin to automate configurations'
echo 'you also may want to take a look at https://gist.github.com/ryuheechul/3918366306f6b7f02c250dcb0cbee4ec'
