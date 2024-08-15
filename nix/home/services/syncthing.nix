{ ... }:

{
  # may require to run this `systemctl --user start syncthing.service`
  services.syncthing.enable = true;
  # may require to run this `systemctl --user start syncthingtray.service`
  # but currently having an error "Failed to start syncthingtray.service: Unit tray.target not found."
  services.syncthing.tray.enable = true;
}
