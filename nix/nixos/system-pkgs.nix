{ pkgs }:

let
  # system pkgs for local only
  local-only = pkgs.lib.optionals (builtins.pathExists ./system-pkgs-local.nix) (import ./system-pkgs-local.nix { pkgs = pkgs; });
in
# system pkgs for any nixOS
with pkgs; [
  zip # Compressor/archiver for creating and modifying zipfiles
  vim # would you rather use nano?
  alacritty # A cross-platform, GPU-accelerated terminal emulator
  nvd # Nix/NixOS package version diff tool
  # https://www.reddit.com/r/linux_gaming/comments/ynue9u/comment/ivat383
  glxinfo # Test utilities for OpenGL - `glxinfo -B`
  libdrm # for drmdevice
  drm_info # Small utility to dump info about DRM devices
  libva-utils # for vainfo
  pciutils # for `lspci`
  kmod # for `modinfo`
  glmark2 # OpenGL (ES) 2.0 benchmark
  usbutils # for lsusb
  lsof
  lshw
  lsscsi
]
  # ++ lib.optionals stdenv.isx86_64 [
  #   intel-gpu-tools # for intel_gpu_top
  # ]
++ local-only
