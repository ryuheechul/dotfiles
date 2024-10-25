{ ... }:

# https://nixos.wiki/wiki/Tailscale
{
  imports = [
    ./resolved.nix
  ];

  services.tailscale.enable = true;
}
