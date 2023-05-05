{ config, pkgs, ... }:

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
    ''''${XDG_CONFIG_HOME}/dfs-rhc/bin/path/default''
  ];

  # only the super basic ones that should be shared across shells
  # `cat ~/.bashrc or other shell rc files to debug`
  home.shellAliases = {
    q = "exit";
    printpath = ''echo ''${PATH} | tr ":" "\n"'';
  };

  programs.command-not-found = {
    enable = ! pkgs.stdenv.isDarwin;
  };

  # fallback to nix-index on darwin in place of command-not-found
  # https://github.com/bennofs/nix-index#usage-as-a-command-not-found-replacement
  programs.nix-index = {
    enable = pkgs.stdenv.isDarwin;
  };

  programs.bash = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.bash.enable
    enable = true;

    # for non-interactive (and interactive)
    bashrcExtra = ''
      # to set env vars and ''$PATH
      # making sure to source below (this actually gets done sometimes but not all the time by ~/.profile which hm generates)
      source "''${HOME}/.nix-profile/etc/profile.d/hm-session-vars.sh"

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

      # to prevent zsh to miss this in case bash was the first one to load
      unset __HM_SESS_VARS_SOURCED
    '';
  };

  programs.zsh = {
    # https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.enable
    enable = true;

    # ~/.zshenv
    envExtra = ''
      source "''${my_zsh_d}/env"

      # to prevent bash to miss this in case zsh was the first one to load
      unset __HM_SESS_VARS_SOURCED
    '';

    # ~/.zlogin
    loginExtra = ''
      source "''${my_zsh_d}/zlogin"
    '';

    # ~/.zshrc
    initExtraFirst = ''
      # to prevent loading zsh stuff twice in case tmux is the default shell command for terminal emulators
      # but skip in ssh - https://unix.stackexchange.com/a/9606
      if uname | xargs test "Darwin" =; then
        if [ -n "''${SSH_CLIENT}" ] || [ -n "''${SSH_TTY}" ] || [ -n "''${SSH_CONNECTION}" ] ; then
          true
        else
          export HOST_ALWAYS_USE_TMUX=1
        fi
      fi
    '';

    # ~/.zshrc
    initExtraBeforeCompInit = ''
      source "''${my_zsh_d}/zshrc"
      test -f ~'/.local.zshrc' && source ~'/.local.zshrc'

      return 0 ##### my hack to ignore everything below that is generated by home-manager
    '';

    # enableCompletion = false;
    # completionInit = "";

    # to maintain consistency with followings:
    # - https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/history.zsh
    # - ../../../zsh/modules/history
    history = {
      # setopt extended_history
      extended = true;
      expireDuplicatesFirst = true;
      ignoreDups = true;
      ignoreSpace = true;

      # SAVEHIST=1000000;
      save = 1000000;
      # HISTSIZE=1000000;
      size = 1000000;
    };
  };
}
