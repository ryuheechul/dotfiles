#!/usr/bin/env sh

# If you are on linux, do this first before ./bootstrap.sh!
# Either machine or human can run this script (there should be no interactions)

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
