# copy this file to ./mix-and-match.nix and tailor to the local device
{ ... }:

{
  imports = [
    # hardware specifics
    ./recipes/surface-pro-intel.nix
    # when it's virtualised
    ./recipes/vm.nix
    ./recipes/wsl.nix
    # services
    ./recipes/openssh.nix
    ./recipes/desktop-gnome.nix
    ./recipes/docker.nix
    ./recipes/virtualization.nix
    ./recipes/flatpak.nix
    ./recipes/tailscale.nix
    ./recipes/1password.nix
    ./recipes/keyd.nix
  ];
}
