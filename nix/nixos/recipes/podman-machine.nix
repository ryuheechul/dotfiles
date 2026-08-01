{ config, pkgs, ... }:

# also see ./podman.nix

{
  # Ensure Podman is installed and enabled
  virtualisation.podman.enable = true;

  # Define the systemd user service to start the Podman machine
  systemd.user.services.podman-machine-autostart = {
    description = "Podman Machine Autostart";
    after = [ "network-online.target" ];
    wantedBy = [ "default.target" ];

    # Injects QEMU explicitly into this service's environment PATH
    path = [ pkgs.qemu ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      # Automatically resolves the correct absolute path to the podman binary
      ExecStart = "${config.virtualisation.podman.package}/bin/podman machine start";
      ExecStop = "${config.virtualisation.podman.package}/bin/podman machine stop";
    };
  };
}
