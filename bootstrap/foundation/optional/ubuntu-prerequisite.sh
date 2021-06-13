#!/usr/bin/env sh

set -e
set -x

# since ubuntu may not come with these 
sudo apt update && \
  sudo apt install git curl openssh-server
