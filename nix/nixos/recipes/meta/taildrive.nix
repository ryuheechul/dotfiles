username:
{ config, ... }:
# to mount taildrive - https://tailscale.com/docs/features/taildrive?tab=linux
{
  services.davfs2.enable = true;

  # dummy secret
  environment.etc."davfs2/secrets" = {
    text = ''
      http://100.100.100.100:8080 guest guest
    '';
    mode = "0600";
  };

  fileSystems."/mount/tailscale" = {
    device = "http://100.100.100.100:8080";
    fsType = "davfs";
    options = [
      "rw" # Read-write access
      "noauto" # Optional: don't mount automatically at boot if Tailscale isn't ready
      "x-systemd.automount" # Better: mount it automatically the first time you access the folder
      "uid=${toString config.users.users.${username}.uid}"
      "gid=${toString config.users.groups.users.gid}"
    ];
  };
}
