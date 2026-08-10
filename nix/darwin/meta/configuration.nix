# this file is to be imported by ../configuration.nix
{ username }:
{ pkgs, config, ... }:
# reference - https://daiderd.com/nix-darwin/manual/index.html

{
  imports = [
    # It will bring these modules:
    # - `security.pam.sshAgentAuth`
    # - `security.pam.sshAgentAuthKeys`
    ../modules/security/pam-ssh-agent-keys.nix
  ];

  system.primaryUser = username;
  environment.systemPackages = with pkgs; [ iproute2mac ];

  ## System Settings

  # https://nix-darwin.github.io/nix-darwin/manual/#opt-system.stateVersion
  system.stateVersion = 6;

  # also read this, https://gist.github.com/ryuheechul/9515381570a0cea994e62647d92a864f
  services.openssh = {
    enable = true;
    extraConfig = ''
      PasswordAuthentication no
      AllowAgentForwarding yes
    '';
  };

  # NSGlobalDomain, dock, spaces, and trackpad user defaults moved to
  # ../../../mise/home/conf.d/50-macos-defaults.toml - `mise bootstrap` applies
  # them on every run instead of requiring nix-darwin activation

  # `defaults read com.apple.universalaccess`
  system.defaults.universalaccess = {
    # actually not able to change these so just do it manually for now
    # closeViewScrollWheelToggle = true;
    # reduceTransparency = true;
  };

  # Requirement for networking.dns
  networking.knownNetworkServices = [
    "Wi-Fi"
  ];

  ## Misc

  # Networking
  networking.dns = [
    "1.1.1.1"
    "8.8.8.8"
  ];

  ## pam/sudo related stuff:
  # - debug with:
  #   - `/etc/pam.d/sudo_local` (for all pam)
  #   - `/etc/ssh/pam-ssh-agent/authorized_keys.d` (for pam-ssh-agent-keys)
  # - https://write.rog.gr/writing/using-touchid-with-tmux/
  # - ../modules/security/pam-ssh-agent.nix adds ssh-agent auth on top of these

  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.watchIdAuth = true;

  # Whether to enable reattaching a program to the user's bootstrap session.
  # This fixes Touch ID for sudo not working inside tmux and screen.
  # This allows programs like tmux and screen that run in the background to survive across user sessions to work with PAM services that are tied to the bootstrap session.
  security.pam.services.sudo_local.reattach = (
    with config.security.pam.services.sudo_local; touchIdAuth || watchIdAuth
  );

  # ssh-agent based sudo auth - this replaces typing the password when the
  # ssh-agent is loaded, see ../modules/security/pam-ssh-agent.nix.
  security.pam.sshAgentAuth.enable = true;
  # sshAgentAuthKeys.enable syncs a root-owned copy of ~/.ssh/authorized_keys
  # to <directory>/<user> on every switch and wires the pam module to read
  # <directory>/%u; user picks whose keys - both must be set, see
  # ../modules/security/pam-ssh-agent-keys.nix
  security.pam.sshAgentAuthKeys.enable = true;
  # `user` is required for `sshAgentAuthKeys` to work
  security.pam.sshAgentAuthKeys.user = username;

  # for devenv to use cachix cache
  nix.settings = {
    trusted-users = [
      username
    ];
  };
}
