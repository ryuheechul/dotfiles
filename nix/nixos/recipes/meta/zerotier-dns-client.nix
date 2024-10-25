{ address, searchDomain ? "home.arpa" }:
{ config, lib, ... }:

## NOTE: if you are using systemd-resolved, this module is unnecessary;
## read and run ./bin/zerotier-dns-client.sh instead

# this is to make the machine to explicitly use a provided address (supposed to be the name server for a Zerotier network) for domain name resolving
#
# for a server side, go to ./zeronsd.nix
#
# also make sure ./zerotier.nix is active as well (otherwise this will be useless)
let
  usingResolved = config.services.resolved.enable;
in
{
  # This module is working in the assumption of something is editing `/etc/resolv.conf` and fighting for.
  # which means inserted nameservers can be overwritten or missing at any moment with some event such as `tailscale [up|down]` ;
  # in case `/etc/resolv.conf` is missing the address, the quick and dirty fix is turn off and on WiFi (`nmcli radio wifi off && nmcli radio wifi on`)

  networking.networkmanager.insertNameservers = if usingResolved then [ ] else [ address ]; # this is safer than below in case the address become unable
  # networking.nameservers = lib.mkForce [ address ];

  # only set when `systemd-resolved` is not in use;
  # since the use of `systemd-resolved` changes the behavior of resolving quite differently;
  # read more on that at ./bin/zerotier-dns-client.sh
  networking.search = if usingResolved then [ ] else [ searchDomain ];
}
