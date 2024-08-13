{ pkgs, ... }:

# https://nixos.wiki/wiki/Dropbox
{
  environment.systemPackages = with pkgs;[
    maestral
    maestral-gui
  ];
}
