# copy this file to ./mix-and-match.nix and tailor to the local device
{ ... }:

{
  imports = [
    # hardware specifics
    ./recipes/surface-pro-intel.nix
    # handling oom to prevent freezing
    ./recipes/oom.nix
    # when it's virtualised
    ./recipes/vm.nix
    ./recipes/wsl.nix
    # services
    ./recipes/openssh.nix
    ./recipes/desktop-gnome.nix
    ./recipes/docker.nix
    ./recipes/rosetta.nix
    ./recipes/waydroid.nix
    ./recipes/virtualization.nix
    ./recipes/flatpak.nix
    ./recipes/tailscale.nix
    ./recipes/1password.nix
    ./recipes/keyd-host-windows.nix
    ./recipes/keyd-guest.nix
  ];
}
