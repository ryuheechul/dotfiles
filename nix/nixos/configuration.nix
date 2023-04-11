{ pkgs
  # , config
, ...
}:

let
  system-pkgs = import ./system-pkgs.nix { pkgs = pkgs; };
in
{
  imports =
    [
      # Include the default configuration that is generated on installation as a good default
      /etc/nixos/configuration.nix # will import `/etc/nixos/hardware-configuration.nix` too
    ];

  # assuming it's a VM running via QEMU
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };

  environment.systemPackages = system-pkgs;
  programs.zsh.enable = true; # for shell = pkgs.zsh; at ./user.nix
}
