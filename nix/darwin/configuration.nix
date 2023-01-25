{ pkgs, ... }:

{
  networking.knownNetworkServices = [
    "Wi-Fi"
    "USB 10/100/1000 LAN"
  ];

  # Networking
  networking.dns = [
    "1.1.1.1"
    "8.8.8.8"
  ];

  nix.useDaemon = true;
}
