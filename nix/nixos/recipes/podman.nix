{ pkgs, ... }:

# https://nixos.wiki/wiki/Podman
# https://wiki.nixos.org/wiki/Distrobox
# https://wiki.nixos.org/wiki/NixOS_Containers
# https://wiki.archlinux.org/title/Podman
{
  virtualisation.podman.enable = true;

  environment.systemPackages = with pkgs; [
    toolbox # this is not necessary but why not
    podman-compose # An implementation of docker-compose with podman backend
  ];
}
