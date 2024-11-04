{ pkgs, ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
    ./chrony-step.nix
    ./laptop.nix
  ];

  # works only for intel
  services.thermald.enable = true;

  # improve pen experience
  services.iptsd.config.Touchscreen = {
    DisableOnPalm = true;
    DisableOnStylus = true;
  };

  environment.systemPackages = [
    pkgs.nvtopPackages.intel
  ];
}
