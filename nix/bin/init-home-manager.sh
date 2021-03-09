#!/usr/bin/env bash

set -x

nix-shell '<home-manager>' -A install

# use this as a template to replace ~/.config/nixpkgs/home.nix

cat << EOF > ~/.config/nixpkgs/home.nix
let
  imports = [
    ~/dotfiles/nix/home.nix
  ];
in
{
  inherit imports;

  home.username = "$(whoami)";
  home.homeDirectory = "$(echo $HOME)";
}
EOF
