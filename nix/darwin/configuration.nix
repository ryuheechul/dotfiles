{ pkgs, ... }:
# reference - https://daiderd.com/nix-darwin/manual/index.html

{
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

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;
}
