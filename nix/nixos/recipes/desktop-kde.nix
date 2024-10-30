{ pkgs, lib, config, ... }:

# https://nixos.wiki/wiki/KDE

# also see ./desktop-gnome.nix
# read this when you go back to gnome - https://discourse.nixos.org/t/gnome-broke-after-trying-plasma/16019/3
let
  isGdmEnabled = config.services.xserver.displayManager.gdm.enable;
in
{
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # services.xserver.displayManager.gdm.enable = lib.mkForce false;
  # https://github.com/NixOS/nixpkgs/issues/75867#issuecomment-974084634
  programs.ssh.askPassword = lib.mkIf (isGdmEnabled) (lib.mkForce "${pkgs.ksshaskpass.out}/bin/ksshaskpass");

  services.displayManager.sddm.enable = if isGdmEnabled then false else true;
  services.desktopManager.plasma6.enable = true;

  # for Settings > About this system > More System Information > Firmware Security
  services.fwupd.enable = true;
}
