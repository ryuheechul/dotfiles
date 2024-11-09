{ pkgs, ... }:

# https://nixos.wiki/wiki/Podman
# https://wiki.nixos.org/wiki/Distrobox
# https://wiki.nixos.org/wiki/NixOS_Containers
# https://wiki.archlinux.org/title/Podman
{
  virtualisation.podman.enable = true;

  environment.systemPackages = [
    pkgs.toolbox # this is not necessary but why not
  ];
}
