{ pkgs, ... }:

let
  checkEnv = import ../../utils/checkEnv.nix;
in
{
  # may require to run this `systemctl --user start syncthing.service`
  services.syncthing.enable = pkgs.lib.mkIf pkgs.stdenv.isLinux true;
  # may require to run this `systemctl --user start syncthingtray.service`
  # but currently having an error "Failed to start syncthingtray.service: Unit tray.target not found."
  #
  # the tray drags in Qt (a big GUI closure), so gate it behind
  # MY_NIX_EXTRA_SYNCTHING_TRAY - headless/minimal builds skip it
  services.syncthing.tray.enable = pkgs.lib.mkIf (
    pkgs.stdenv.isLinux && checkEnv "MY_NIX_EXTRA_SYNCTHING_TRAY"
  ) true;
}
