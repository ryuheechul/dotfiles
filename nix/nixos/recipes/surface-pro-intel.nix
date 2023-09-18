{ ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
  ];

  microsoft-surface.kernelVersion = "6.4.16";
}
