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

  ## System Settings

  # https://nix-darwin.github.io/nix-darwin/manual/#opt-system.stateVersion
  system.stateVersion = 6;

  # also read this, https://gist.github.com/ryuheechul/9515381570a0cea994e62647d92a864f
  services.openssh = {
    enable = true;
    extraConfig = ''
      PasswordAuthentication no
    '';
  };

  # `defaults read "Apple Global Domain"`
  system.defaults.NSGlobalDomain = {
    AppleInterfaceStyleSwitchesAutomatically = true;
    InitialKeyRepeat = 15;
    KeyRepeat = 2;
    NSAutomaticCapitalizationEnabled = false;
    NSAutomaticDashSubstitutionEnabled = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
    NSAutomaticQuoteSubstitutionEnabled = false;
    ApplePressAndHoldEnabled = false;
  };

  # `defaults read com.apple.universalaccess`
  system.defaults.universalaccess = {
    # actually not able to change these so just do it manually for now
    # closeViewScrollWheelToggle = true;
    # reduceTransparency = true;
  };

  # Dock and Mission Control - `defaults read com.apple.dock`
  system.defaults.dock = {
    autohide = true;
  };

  # Spaces - `defaults read com.apple.spaces`
  system.defaults.spaces.spans-displays = false;

  # Trackpad - `defaults read com.apple.AppleMultitouchTrackpad`
  system.defaults.trackpad = {
    Clicking = true;
    Dragging = true;
    TrackpadRightClick = true;
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
}
