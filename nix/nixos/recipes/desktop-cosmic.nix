{
  config,
  pkgs,
  lib,
  ...
}:

# https://wiki.nixos.org/wiki/COSMIC
let
  isGnomeEnabled = config.services.xserver.desktopManager.gnome.enable;
in
{
  # this is original for ./desktop-kde.nix - https://github.com/NixOS/nixpkgs/issues/75867#issuecomment-974084634
  # is there any better option than this? it works though
  programs.ssh.askPassword = "${pkgs.kdePackages.ksshaskpass.out}/bin/ksshaskpass";

  # due to gnome complains on not being able to lock screen without the use of gdm
  services.displayManager.cosmic-greeter.enable = pkgs.lib.mkForce (
    if isGnomeEnabled then false else true
  );
  services.desktopManager.cosmic.enable = true;
}
