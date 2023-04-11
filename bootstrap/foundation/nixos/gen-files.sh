#!/usr/bin/env bash

./channel.sh

cat << EOF > ./hm.nix
# generated by ./gen-files.sh
let
  home-nix-path = ../../../nix/home;
  imports = [ home-nix-path ];
in
{
  imports = [ <home-manager/nixos> ];
  home-manager.users.$(whoami) = { pkgs, ... }: {
    inherit imports;

    home.username = "$(whoami)";
    home.homeDirectory = "$(echo "${HOME}")";
  };
}
EOF

cat << EOF > ./configuration.nix
# generated by ./gen-files.sh

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  my-nixos = ../../../nix/nixos/configuration.nix;
  user = import ../../../nix/nixos/user.nix { username = "$(whoami)"; pkgs = pkgs; };
in
{
  imports =
    [
      # Include the results of the hardware scan.
      my-nixos
      ./hm.nix
    ];

  users.users.$(whoami) = user;
}
EOF
