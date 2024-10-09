{ ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
    ./laptop.nix
  ];

  # works only for intel
  services.thermald.enable = true;

  # improve pen experience
  services.iptsd.config.Touch.DisableOnPalm = true;
  services.iptsd.config.Touch.DisableOnStylus = true;
}
