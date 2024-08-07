{ ... }:

# Dealing with gotchas with surface 9 - inspired mostly by https://github.com/iwanders/nixos-surface
{
  imports = [
    <nixos-hardware/microsoft/surface-pro/9>
  ];

  # until https://github.com/linux-surface/linux-surface/issues/1183
  boot.blacklistedKernelModules = [
    "surface_gpe"
  ];
}
