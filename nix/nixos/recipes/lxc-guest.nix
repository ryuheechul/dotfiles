{ pkgs, modulesPath, ... }:

{
  imports = [
    # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/lxc-container.nix
    "${modulesPath}/virtualisation/lxc-container.nix"

    # The above is for generic LXC and I'm using it for incus LXC guest.

    # For Proxmox guest, this should be better at
    # https://wiki.nixos.org/wiki/Proxmox_Virtual_Environment#Deploying_on_Proxmox_VE_2
    # which uses `(modulesPath + "/virtualisation/proxmox-lxc.nix")` instead
  ];

  # Container just should follow the host kernel's clock
  services.chrony.enable = pkgs.lib.mkForce false;

  # to handle errror like below
  # ```
  # There was an error running ping: exit status: 2
  # Stderr: ping: socktype: SOCK_RAW
  # ping: socket: Operation not permitted
  # ping: => missing cap_net_raw+p capability or setuid?
  # ```
  security.wrappers.ping = {
    owner = "root";
    group = "root";
    capabilities = "cap_net_raw+ep";
    source = "${pkgs.iputils}/bin/ping";
  };

  services.openssh.extraConfig = ''
    # `networkConfig.IPv6PrivacyExtensions = "kernel"` from `virtualisation/lxc-container.nix`
    # makes it difficult (if not impossible) to connect via IPv6 so limit only to IPv4
    AddressFamily inet
    # After this it will cleanly refuse even with `ssh -6` so no more hanging.
    # It may requires reboot or at the least restarting of sshd
  '';
}
