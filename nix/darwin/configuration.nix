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

  # additional "activation" to deal with a ungraceful experience of discovering apps via spotlight
  # so basically this script will fix that
  # it's currently just "refresh" apps via copying from real files
  # - it's probably not the most clever and optimal way but it works for me
  #
  # inspired by these below:
  # - https://github.com/LnL7/nix-darwin/blob/master/modules/system/activation-scripts.nix
  # - https://github.com/LnL7/nix-darwin/issues/214#issuecomment-1230730292
  # - https://github.com/NixOS/nix/issues/956#issuecomment-1367457122 -
  system.activationScripts.extraUserActivation.text = ''
    echo "setting up ~/Applications..." >&2
    applications="$HOME/Applications"
    nix_apps="$applications/Nix Apps"

    # Needs to be writable by the user so that home-manager can symlink into it
    if ! test -d "$applications"; then
        mkdir -p "$applications"
        chown $(whoami): "$applications"
        chmod u+w "$applications"
    fi

    # Delete the directory to remove old links
    rm -rf "$nix_apps"
    mkdir -p "$nix_apps"

    # be aware this script result in two ways:
    # - 'Nix Apps/[app]' when there is more than one app under ~/.nix-profile/Applications
    # - 'Nix Apps/Applications/[app]' when there is just one app under ~/.nix-profile/Applications
    # I don't know why exactly it is like that but that is what it is for now
    find $(readlink "$HOME/.nix-profile/Applications") -type l -exec readlink '{}' + |
        while read src; do
            cp -rL "$src" "$nix_apps"
        done

    chmod -R +w "$nix_apps" # makes it easier to delete at next run
  '';
}
