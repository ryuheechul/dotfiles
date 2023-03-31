{ config, ... }:

let
  env-vars = config.home.sessionVariables;
in
{
  # compatibility with linux inspired by https://github.com/marlonrichert/zsh-snap/blob/de5f789de661883bc118d96c8fd862935b6d3364/scripts/init.zsh#LL28-L31
  xdg = {
    # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
    enable = true;
  };

  # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
  home.sessionVariables = {
    my_dot_d = "${env-vars.XDG_CONFIG_HOME}/dfs-rhc";
    my_zsh_d = "${env-vars.my_dot_d}/zsh";
  };

  # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
  # I don't like that these goes after $PATH not before - let me think about a workaround
  # and of course the related issues can be found here! - https://github.com/nix-community/home-manager/issues/3324
  home.sessionPath = [
    "$XDG_CONFIG_HOME/dfs-rhc/bin/path/default"
  ];

  # only the super basic ones that should be shared across shells
  # `cat ~/.bashrc or other shell rc files to debug`
  home.shellAliases = {
    q = "exit";
    printpath = ''echo ''${PATH} | tr ":" "\n"'';
  };

  programs.bash = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.bash.enable
    enable = true;
    # for non-interactive (and interactive)
    bashrcExtra = ''
      # to set env vars and ''$PATH
      # this actually gets done by ~/.profile which hm generates
      # source "''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh"

      # sometimes this gets done by /etc/bashrc (the sourcing ending with `set-environment`)
      # but not all platforms work that way - so this is the fallback
      source "''${my_dot_d}/nix/bin/source/nix.sh"

      # load tea.xyz when available
      if test -d "''${HOME}/.tea"; then
        export TEA_MAGIC=0
        # load tea first
        command -v tea 2>&1 >/dev/null || source <("''${HOME}/.tea/tea.xyz/v*/bin/tea" --magic=bash --silent)

        # and binaries via tea
        command -v tea 2>&1 >/dev/null && export PATH="''${my_dot_d}/bin/path/tea/bin:''${PATH}"
      fi
    '';
  };
}
