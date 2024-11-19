{ pkgs, ... }:

# https://nixos.wiki/wiki/Podman
# https://wiki.nixos.org/wiki/Distrobox
# https://wiki.nixos.org/wiki/NixOS_Containers
# https://wiki.archlinux.org/title/Podman
{
  virtualisation.podman.enable = true;

  environment.systemPackages = with pkgs; [
    toolbox # this is not necessary but why not
    podman-tui # Podman Terminal UI - requires running `systemctl --user start podman.socket` (once) to work
    podman-compose # An implementation of docker-compose with podman backend
  ];

  # run `systemctl --user start podman.socket` in case providing the socket API that expect $DOCKER_HOST socket to be listening
  # run `man podman-system-service` and see ./docker.nix to learn more
}
