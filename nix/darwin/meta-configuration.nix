# this is file is used to create ./configuration.nix by ./gen-configuration.sh
{ username }:
{ pkgs, ... }:
# reference - https://daiderd.com/nix-darwin/manual/index.html

let
  inherit (pkgs.lib) mkIf;
  enableBrew = true;
in
{
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
  # ../../mise/home/conf.d/50-macos-defaults.toml - `mise bootstrap` applies
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
  # - debug with `cat /etc/pam.d/sudo_local`:
  # - https://write.rog.gr/writing/using-touchid-with-tmux/

  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.watchIdAuth = true;

  # Whether to enable reattaching a program to the user’s bootstrap session.
  # This fixes Touch ID for sudo not working inside tmux and screen.
  # This allows programs like tmux and screen that run in the background to survive across user sessions to work with PAM services that are tied to the bootstrap session.
  security.pam.services.sudo_local.reattach = true;

  # for devenv to use cachix cache
  nix.settings = {
    trusted-users = [
      username
    ];
  };
}
