{ config, pkgs, lib, ... }:

let
  packages = import ../pkgs { };
  prgs = import ./programs { pkgs = pkgs; config = config; };
  svcs = import ./services { pkgs = pkgs; };
  dconf = import ./dconf.nix { lib = lib; };
  shims = import ./shims.nix { pkgs = pkgs; };
  imports = [ prgs svcs dconf shims ];
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

  # ../pkgs/fonts will be included via ../pkgs
  fonts.fontconfig.enable = true;
  # debug/find fonts per OS:
  # - linux: https://github.com/nix-community/home-manager/blob/master/modules/misc/fontconfig.nix
  # - darwin: https://github.com/nix-community/home-manager/blob/master/modules/targets/darwin/fonts.nix
  #   - ~/Library/Fonts/HomeManager
}
