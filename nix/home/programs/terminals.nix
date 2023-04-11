{ pkgs, ... }:

let
  startup_session_file = pkgs.writeTextFile {
    name = "kitty_startup_session";
    text = ''
      launch zsh -c 'tmux-attach kitty'
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

    settings = {
      hide_window_decorations = true;
      startup_session = startup_session_path;
    };
  };
}
