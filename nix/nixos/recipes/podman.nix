{ pkgs, ... }:

# https://nixos.wiki/wiki/Podman
# https://wiki.nixos.org/wiki/Distrobox
# https://wiki.nixos.org/wiki/NixOS_Containers
# https://wiki.archlinux.org/title/Podman
{
  # debug with `podman info` - https://www.reddit.com/r/podman/comments/14dgdf8/how_can_i_figure_out_which_storage_driver_podman/
  virtualisation.podman.enable = true;

  environment.systemPackages = with pkgs; [
    podman-tui # Podman Terminal UI - requires running `systemctl --user start podman.socket` (once) to work
    podman-compose # An implementation of docker-compose with podman backend
  ];

  # run `systemctl --user start podman.socket` in case providing the socket API that expect $DOCKER_HOST socket to be listening
  # run `man podman-system-service` and see ./docker.nix to learn more
}
