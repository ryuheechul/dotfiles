{ pkgs, ... }:

let
  startup_session_file = pkgs.writeTextFile {
    name = "kitty_startup_session";
    # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
    # (originally I didn't know about `os_window_state` at https://github.com/kovidgoyal/kitty/issues/856)
    text = ''
      # `maximized` provides better experience than `fullscreen` on linux
      os_window_state maximized
      launch zsh -c 'source ~/.base16_theme; exec-tmux-attach kitty'
    '';
  };
  # below works the same way as above for current purpose
  # startup_session_file = pkgs.writeText "kitty_startup_session"
  #   ''
  #     launch zsh -c 'tmux-attach kitty'
  #   '';
  startup_session_path = "${startup_session_file}";
in
{
  programs.kitty = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.kitty.enable
    enable = true;

    font = {
      name = "FiraMono Nerd Font Mono";
      size = 15;
    };

    settings = {
      hide_window_decorations = true;
      startup_session = startup_session_path;
    };

    # https://sw.kovidgoyal.net/kitty/actions/#action-paste
    keybindings = {
      "shift+insert" = "paste_from_clipboard";
    };
  };

  # this shadows the original desktop file that should have been at ~/.nix-profile/share/applications/kitty.desktop
  # debug via `cat ~/.nix-profile/share/applications/kitty.desktop` both for before and after (by commenting this block below if it's for before)
  xdg.desktopEntries.kitty = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    name = "kitty";
    genericName = "Terminal emulator";
    exec = "kitty"; # this is the main fix and the rest is to conform with original
    icon = "kitty";
    comment = "Fast, feature-rich, GPU based terminal";
    categories = [
      "System"
      "TerminalEmulator"
    ];
    settings = {
      TryExec = "kitty";
    };
  };
  # below is from the original content at the time of capturing
  # ```
  # [Desktop Entry]
  # Version=1.0
  # Type=Application
  # Name=kitty
  # GenericName=Terminal emulator
  # Comment=Fast, feature-rich, GPU based terminal
  # TryExec=kitty
  # Exec=kitty
  # Icon=kitty
  # Categories=System;TerminalEmulator;
  # ```

  # referencing https://github.com/ghostty-org/ghostty/blob/main/dist/linux/app.desktop
  xdg.desktopEntries.ghostty = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    name = "Ghostty";
    exec = "ghostty"; # this is the main fix and the rest is to conform with original
    icon = "com.mitchellh.ghostty";
    comment = "A terminal emulator";
    categories = [
      "System"
      "TerminalEmulator"
    ];
    settings = {
      Keywords = "terminal;tty;pty;";
    };
    startupNotify = true;
  };
}
