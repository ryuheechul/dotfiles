{ config, ... }:

# https://nixos.wiki/wiki/Tailscale

let
  ifName = config.services.tailscale.interfaceName;
in
{
  imports = [
    ./resolved.nix
  ];

  services.tailscale.enable = true;
  services.tailscale.useRoutingFeatures = "both";

  # if using exit node or subnet routing;
  # https://discourse.nixos.org/t/tailscale-exit-node-not-working-on-nixos/39897
  # set it like this somewhere `services.tailscale.useRoutingFeatures = "both"`; # or "client" or "server" or none
  # `nixos-option services.tailscale.useRoutingFeatures` to read more about and;
  # see this for the detailed behavior https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/tailscale.nix

  # Optional: to allow DNS query in case the DNS server is in another interface,
  # (e.g. `incusbr0` - see `./incus.nix`).
  # Debug with `sudo nft list ruleset` and look at "chain input-allow".
  networking.firewall.interfaces.${ifName} = {
    allowedUDPPorts = [ 53 ];
    allowedTCPPorts = [ 53 ];
  };
}
