#!/usr/bin/env sh

# If you are on mac, do this first before ./bootstrap.sh!

# install homebrew
if ! command -v brew &> /dev/null
then
    echo "brew not found so installing"
    sleep 1
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    exit
else
    echo "brew exist, so skipping installation"
fi

# mas let you install Mac App Store app with CLI
brew install mas
mas install 869223134 # KakaoTalk
mas install 1480933944 # Vimari
mas install 1142151959 # Just Focus

brew cask install iterm2
brew cask install alfred
brew cask install vanilla
brew cask install 1password
brew cask install amethyst
brew cask install google-chrome
brew cask install karabiner-elements
echo "if you have karabiner files already then simlink to ~/.config/karabiner"

sleep 2

echo "read key repeat values before set"
defaults read -g InitialKeyRepeat
defaults read -g KeyRepeat

defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)

echo "Settings > Accessibility > Display > Reduce Motion checkbox" # https://github.com/mathiasbynens/dotfiles/issues/711#issuecomment-278265644
sleep 2

## Maybe JetBrainsMono is enough? so comment this out for now
# brew cask install homebrew/cask-fonts/font-hack-nerd-font

# download and install JetBrains Mono
wget -O JetBrainsMono.zip https://download.jetbrains.com/fonts/JetBrainsMono-2.001.zip
unzip JetBrainsMono.zip -d .JetBrainsMono
rm JetBrainsMono.zip
echo "You will need to click stuff to add fonts"
sleep 2

open .JetBrainsMono/ttf/*.ttf
echo ".JetBrainsMono will be cleaned up after a minute"
(sleep 5; rm -rf .JetBrainsMono ) &

sleep 5
# go to Keytty download page
open https://github.com/keytty/shelter/releases
