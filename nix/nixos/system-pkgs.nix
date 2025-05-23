{ pkgs }:

let
  # system pkgs for local only
  local-only = pkgs.lib.optionals (builtins.pathExists ./system-pkgs-local.nix) (
    import ./system-pkgs-local.nix { pkgs = pkgs; }
  );
  pop-shell-stuff = with pkgs; [
    # this is need for pop-shell to use launcher
    # https://github.com/NixOS/nixpkgs/issues/174353#issuecomment-1416152982
    pop-launcher # Modular IPC-based desktop launcher service
    # gnomeExtensions required to be enabled first before being used
    # one way to do it is via `Extensions` app but I automated that with ../home/dconf.nix
    gnomeExtensions.pop-shell # Keyboard-driven layer for GNOME Shell
    gnomeExtensions.night-theme-switcher # Automatically toggle your desktop’s color scheme between light and dark, switch backgrounds and run custom commands at sunset and sunrise.
  ];
  nix-utils = with pkgs; [
    nvd # Nix/NixOS package version diff tool
    dconf2nix # Convert dconf files to Nix, as expected by Home Manager
  ];
  missing-commons = with pkgs; [
    dnsutils # including dig and nslookup
    zip # Compressor/archiver for creating and modifying zipfiles
    vim # The most popular clone of the VI editor
    file # A program that shows the type of files
    lsof # A tool to list open files
    libdrm # for drmdevice
    libinput # Handles input devices in Wayland compositors and provides a generic X.Org input driver
    drm_info # Small utility to dump info about DRM devices
    kmod # for `modinfo`
    file # A program that shows the type of files
    # with the below, no more missing entries regarding system calls
    man-pages # Linux development manual pages
  ];
  hardware-support = with pkgs; [
    glmark2 # OpenGL (ES) 2.0 benchmark
    usbutils # for lsusb
    lshw # Provide detailed information on the hardware configuration of the machine
    lsscsi
    libva-utils # for vainfo
    pciutils # for `lspci`
    # https://www.reddit.com/r/linux_gaming/comments/ynue9u/comment/ivat383
    glxinfo # Test utilities for OpenGL - `glxinfo -B`
    bindfs # A FUSE filesystem for mounting a directory to another location
    powertop # Analyze power consumption on Intel-based laptops
    resources # Monitor your system resources and processes
    # comment below out since it doesn't work but the flatpak version works great via ./recipes/flatpak.nix
    # gpu-viewer # A front-end to glxinfo, vulkaninfo, clinfo and es2_info
  ];
in
# system pkgs for any nixOS
with pkgs;
[
  alacritty # A cross-platform, GPU-accelerated terminal emulator
]
++ missing-commons
++ nix-utils
++ hardware-support
++ pop-shell-stuff
++ local-only
