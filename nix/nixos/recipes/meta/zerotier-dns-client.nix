{ address, }:
{ lib, ... }:

# this is to make the machine to explicitly use a provided address (supposed to be the name server for a Zerotier network) for domain name resolving
#
# for a server side, go to ./zeronsd.nix
#
# also make sure ./zerotier.nix is active as well (otherwise this will be useless)
{

  networking.networkmanager.insertNameservers = [ address ]; # this is safer than below in case the address become unable
  # networking.nameservers = lib.mkForce [ address ];
}
