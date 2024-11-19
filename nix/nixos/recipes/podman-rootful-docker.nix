{ config, ... }:

# check out `./podman.nix` and `./docker-rootful.nix`
{
  imports = [ ./podman.nix ];

  # shim for other docker tools
  virtualisation.podman.dockerSocket.enable =
    if config.virtualisation.docker.enable then false else true;
  # requires `systemctl restart podman.socket` to take effect
  # could this silently fail if there is existing `/run/docker.sock`? (e.g. from residue of previous rootful docker setup)
  # https://github.com/systemd/systemd/issues/6920
  # also undoing this doesn't clean up `/run/docker.sock` (even after `systemctl restart podman.socket`) either; thus need a manual clean up
}
