{ pkgs, ... }:

let
  # use mkDefault just in case of conflict with ./uxplay.nix
  mkDefault = pkgs.lib.mkDefault;
in
{
  # https://wiki.archlinux.org/title/Avahi
  services.avahi = {
    # avahi to manage mdns
    enable = mkDefault true;
    # this allows to query *.local - aka being a mDNS client
    nssmdns4 = mkDefault true;
    publish = {
      enable = mkDefault true;
      # this publish the addresses. e.g. [hostname].local - aka being a mDNS server
      addresses = mkDefault true;
    };
  };
}
