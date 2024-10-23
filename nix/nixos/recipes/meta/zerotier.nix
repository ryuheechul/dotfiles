{ network, searchDomain ? "home.arpa" }:
{ lib, ... }:

{
  services.zerotierone = {
    enable = true;
    joinNetworks = [
      network
    ];
  };

  networking.search = [ searchDomain ];
}
