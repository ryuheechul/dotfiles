{ network, useResolved ? true }:
{ lib, ... }:

{
  imports = [ ]
    ++ lib.optionals useResolved [
    ../resolved.nix
  ];

  services.zerotierone = {
    enable = true;
    joinNetworks = [
      network
    ];
  };
}
