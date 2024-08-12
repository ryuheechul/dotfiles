{ ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
    ./laptop.nix
  ];

  # works only for intel
  services.thermald.enable = true;
}
