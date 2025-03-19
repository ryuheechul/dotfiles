{ config, ... }:

let
  env-vars = config.home.sessionVariables;
  # nix-index-enable = (import ../../pkgs/custom/nix-index-database).default;
  # nix-index-enable = import ../../pkgs/custom/alt-nix-index-database; # favor non-flake glue
  nix-index-enable = import ../../pkgs/custom/alt-nid; # favor a even simpler version
in
{
  imports = [
    # in place of `programs.command-not-found.enable` or `programs.nix-index.enable`;
    # powered by https://github.com/nix-community/nix-index-database
    nix-index-enable
    # https://github.com/bennofs/nix-index#usage-as-a-command-not-found-replacement
  ];

  # compatibility with linux inspired by https://github.com/marlonrichert/zsh-snap/blob/de5f789de661883bc118d96c8fd862935b6d3364/scripts/init.zsh#LL28-L31
  xdg = {
    # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
    enable = true;
  };

  # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
  home.sessionVariables = {
    my_dot_d = "${env-vars.XDG_CONFIG_HOME}/dfs-rhc";
    my_zsh_d = "${env-vars.my_dot_d}/zsh";
    # https://nixos.wiki/wiki/Flatpak
    XDG_DATA_DIRS = ''''${XDG_DATA_DIRS}:/usr/share:/var/lib/flatpak/exports/share:''${HOME}/.local/share/flatpak/exports/share'';
    # https://github.com/sharkdp/bat/issues/2578#issuecomment-1598332705
    LESSUTFCHARDEF = "E000-F8FF:p,F0000-FFFFD:p,100000-10FFFD:p";
    # https://github.com/microsoft/WSL/issues/4446
    LESSCHARSET = "utf-8";
    # mostly to fix the issue describe here, https://www.reddit.com/r/emacs/comments/10lkwgr/emacsclient_in_terminal_doesnt_show_theme/ but also why not?
    COLORTERM = "truecolor";
  };

  # `cat ~/.nix-profile/etc/profile.d/hm-session-vars.sh` to debug
  # I don't like that these goes after $PATH not before - let me think about a workaround
  # and of course the related issues can be found here! - https://github.com/nix-community/home-manager/issues/3324
  home.sessionPath = [
    ''''${XDG_CONFIG_HOME}/dfs-rhc/bin/path/default''
  ];

  # only the super basic ones that should be shared across shells
  # `cat ~/.bashrc or other shell rc files to debug`
  # `cat ~/.zshrc to see why this is not being loaded for zsh`
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

      # overriding due to https://github.com/nix-community/home-manager/issues/3324
      export PATH="''${XDG_CONFIG_HOME}/dfs-rhc/bin/path/default:''${PATH}"

      # to prevent zsh to miss this in case bash was the first one to load
      unset __HM_SESS_VARS_SOURCED
    '';

    # I'm treating this like `.zlogin` and delegating handling ssh login shell case
    # to zsh in case zsh is not the default shell
    profileExtra = ''
      test -n "''${SSH_TTY}" && {
        # e.g. emacs tramp
        if test "''${TERM}" == "dumb"; then
          grep "NAME=NixOS" /etc/os-release &> /dev/null &&
            __ETC_PROFILE_SOURCED= __NIXOS_SET_ENVIRONMENT_DONE= source /etc/profile
        else
          source "''${my_dot_d}/nix/bin/source/nix.sh" && exec zsh -l
        fi
      }
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
        if [ -n "''${SSH_CONNECTION}" ] || [ -n "''${SSH_CLIENT}" ] || [ -n "''${SSH_TTY}" ] ; then
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

  programs.nushell = {
    # simply enable for now since investing in this shell would take a lot of time
    enable = true;
  };
}
