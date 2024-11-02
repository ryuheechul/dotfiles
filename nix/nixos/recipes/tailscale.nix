{ ... }:

# https://nixos.wiki/wiki/Tailscale
{
  imports = [
    ./resolved.nix
  ];

  services.tailscale.enable = true;

  # if using exit node or subnet routing;
  # https://discourse.nixos.org/t/tailscale-exit-node-not-working-on-nixos/39897
  # set it like this somewhere `services.tailscale.useRoutingFeatures = "both"`; # or "client" or "server" or none
  # `nixos-option services.tailscale.useRoutingFeatures` to read more about and;
  # see this for the detailed behavior https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/tailscale.nix
}
