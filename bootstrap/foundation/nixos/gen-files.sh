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

{
  imports =
    [
      # Include the results of the hardware scan.
      ./base-configuration.nix
    ];

  # Bootloader.
$(grep 'boot\.loader\.' /etc/nixos/configuration.nix)

  # Set your time zone.
$(grep 'time\.timeZone' /etc/nixos/configuration.nix | head -n1)

  # Select internationalisation properties.
$(grep 'i18n\.defaultLocale' /etc/nixos/configuration.nix | head -n1)

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.$(whoami) = {
    isNormalUser = true;
    description = "user $(whoami)";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [ ] ++ pkgs.lib.optionals (builtins.pathExists ./local-pkgs.nix) (import ./local-pkgs.nix { pkgs = pkgs; });
    shell = pkgs.zsh;
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = "$(whoami)";
}
EOF
