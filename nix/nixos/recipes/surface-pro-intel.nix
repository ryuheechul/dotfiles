{ ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
    ./chrony-step.nix
    ./laptop.nix
  ];

  # works only for intel
  services.thermald.enable = true;

  # improve pen experience
  services.iptsd.config.Touchscren = {
    DisableOnPalm = true;
    DisableOnStylus = true;
  };
}
