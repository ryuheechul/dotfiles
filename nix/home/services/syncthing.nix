{ pkgs, ... }:

{
  # may require to run this `systemctl --user start syncthing.service`
  services.syncthing.enable = pkgs.lib.mkIf pkgs.stdenv.isLinux true;
  # may require to run this `systemctl --user start syncthingtray.service`
  # but currently having an error "Failed to start syncthingtray.service: Unit tray.target not found."
  services.syncthing.tray.enable = pkgs.lib.mkIf pkgs.stdenv.isLinux true;
}
