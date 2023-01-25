{ pkgs, ... }:
# reference - https://daiderd.com/nix-darwin/manual/index.html

let
  inherit (pkgs.lib) mkIf;
  enableBrew = true;
in
{
  imports = [
    ./modules/pam.nix
  ];

  nix.useDaemon = true;

  ## System Settings

  # `defaults read "Apple Global Domain"`
  system.defaults.NSGlobalDomain = {
    AppleInterfaceStyleSwitchesAutomatically = true;
    InitialKeyRepeat = 15;
    KeyRepeat = 2;
    NSAutomaticCapitalizationEnabled = false;
    NSAutomaticDashSubstitutionEnabled = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
    NSAutomaticQuoteSubstitutionEnabled = false;
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
    TrackpadRightClick = true;
    TrackpadThreeFingerDrag = true;
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

  # Manage Homebrew
  homebrew.enable = enableBrew;
  homebrew.brews = [
    "pam-reattach"
  ];

  # Add ability to used TouchID for sudo authentication
  # security.pam.enableSudoTouchIdAuth = true;
  # above doesn't work with tmux hence use the below.
  # thanks to https://github.com/LnL7/nix-darwin/pull/228#issuecomment-873388257
  security.pamCustom.enableSudoTouchIdAuth = mkIf enableBrew true;
}
