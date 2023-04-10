#!/usr/bin/env bash

test -d /nix/var/nix/profiles/per-user/root/channels/home-manager && exit 0

# reach here only when there is no home-manager channel added already
sudo nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
sudo nix-channel --update
