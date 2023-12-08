{ pkgs, ... }:

# there is ./mdns.nix if all you need is just mdns feature

# inspired by https://github.com/SimonBrandner/dotfiles/blob/main/Root/etc/nixos/configuration.nix#L96
{
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      userServices = true;
      addresses = true;
      workstation = true;
      domain = true;
    };
    nssmdns = true;
    openFirewall = true;
  };

  environment.systemPackages = with pkgs;[
    uxplay # AirPlay Unix mirroring server
  ];

  networking = {
    # more info at https://github.com/antimof/UxPlay
    firewall = {
      enable = true;
      allowedTCPPorts = [
        7000
        7001
        7100
      ];
      allowedUDPPorts = [
        6000
        6001
        7011
      ];
    };
  };
}
