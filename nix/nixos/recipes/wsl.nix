{ pkgs, ... }:

{
  # https://github.com/nix-community/NixOS-WSL/issues/241
  systemd.services.systemd-udevd.enable = pkgs.lib.mkForce true;
  services.udev.enable = pkgs.lib.mkForce true;
  security.polkit.enable = true;
}
