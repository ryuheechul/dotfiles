{
  pkgs,
  lib,
  config,
  ...
}:

# https://wiki.nixos.org/wiki/KDE
# https://www.reddit.com/r/linux_gaming/comments/d0njrr/to_reduce_cpu_consumption_when_using_kde/

# also see ./desktop-gnome.nix
# read this when you go back to gnome - https://discourse.nixos.org/t/gnome-broke-after-trying-plasma/16019/3
let
  isGnomeEnabled = config.services.xserver.desktopManager.gnome.enable;
in
{
  # https://github.com/NixOS/nixpkgs/issues/75867#issuecomment-974084634
  programs.ssh.askPassword = lib.mkIf isGnomeEnabled (
    lib.mkForce "${pkgs.kdePackages.ksshaskpass.out}/bin/ksshaskpass"
  );

  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # for Settings > About this system > More System Information > Firmware Security
  services.fwupd.enable = true;

  environment.systemPackages = with pkgs.kdePackages; [
    koi # Theme scheduling for the KDE Plasma Desktop
  ];

  # https://wiki.nixos.org/wiki/KDE_Connect
  programs.kdeconnect = {
    enable = true;
    package = pkgs.kdePackages.kdeconnect-kde;
  };
}
