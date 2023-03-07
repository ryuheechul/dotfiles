{ config, pkgs, ... }:

let
  packages = import ../pkgs { };
  prgs = import ./programs { pkgs = pkgs; config = config; };
  imports = [ prgs ];
in
{
  inherit imports;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  #
  # home.username = "my-user";
  # home.homeDirectory = "/home/my-user";
  #
  # to avoid having to check out username and home directory in dotfiles repo
  # set values like above in ~/.config/nixpkgs/home.nix
  # and import this file from ~/.config/nixpkgs/home.nix

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  home.packages = packages;
}
