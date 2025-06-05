{ pkgs, ... }:

# some tools that are helpful for quicker ways to spin virtual machines to complement ./libvirt.nix
{
  environment.systemPackages = with pkgs; [
    quickemu # Quickly create and run optimised Windows, macOS and Linux virtual machines
    quickgui # Elegant virtual machine manager for the desktop
    vagrant # Tool for building complete development environments
    lima # Linux virtual machines (on macOS, in most cases)
  ];
}
