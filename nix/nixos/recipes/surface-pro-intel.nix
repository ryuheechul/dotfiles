{ ... }:

{
  imports = [
    <nixos-hardware/microsoft/surface/surface-pro-intel>
  ];

  microsoft-surface.kernelVersion = "6.1.18";
}
