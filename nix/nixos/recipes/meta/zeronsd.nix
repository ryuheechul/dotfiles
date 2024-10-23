{ network, tokenPath ? "/var/lib/zeronsd/apitoken" }:
{ config, lib, ... }:

# this is a to run a name server that works like a authoritative name server for a zero tier network
# (and also forward the rest according to `/etc/resolv.conf`)
#
# learn more at:
# - https://docs.zerotier.com/dns/
# - https://github.com/zerotier/zeronsd
#
# Once enabled, the current host starts listening on #53 at zerotier network interface only;
# make sure to not run this more than one node per network; choose the most stable node to run this (aka no laptops)
#
# This module doesn't alter the client side of configuration (e.g. /etc/resolv.conf); for that look at ./zerotier-dns-client.nix

{
  services.zeronsd.servedNetworks = {
    ${network} =
      if (builtins.pathExists tokenPath) then {
        settings = {
          token = tokenPath;
          log_level = "trace";
        };
      } else builtins.trace "token file (${tokenPath}) doesn't exist on the path" { };
  };

  # need to open port 53 to serve traffic from other machines
  networking = {
    firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
