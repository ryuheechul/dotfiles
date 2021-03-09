{ pkgs }:

{
  toInstall = [
    pkgs.htop
    pkgs.cowsay
    pkgs.fortune

    # essential
    pkgs.zsh # my favorite shell interface
    pkgs.git # to replace possible old git comes with OS
    pkgs.bash # use latest bash
    pkgs.tmux # terminal multiplexer
    pkgs.neovim # my favorite editor
    pkgs.starship # cross-shell prompt
    pkgs.fzf # A command-line fuzzy finder
    pkgs.direnv # auto loading env per dir
    pkgs.htop # resource monitoring
    pkgs.gnupg # GnuPG
    pkgs.wget # useful for downloading files

    # viewer
    pkgs.exa # modern ls
    pkgs.bat # cat with wings
    pkgs.glow # markdown render on cli
    pkgs.lf # terminal file manager
    pkgs.fx # json viewer

    # search
    pkgs.ripgrep # modern grep
    pkgs.fasd # Command-line productivity booster, offers quick access to files and directories
    pkgs.fd # mordern find

    # helper
    pkgs.tig # git helper
    pkgs.tldr # Simplified and community-driven man pages
    pkgs.watch # execute a program periodically
    pkgs.entr # Run arbitrary commands when files change
    # pkgs.taskell # CLI kanban
    pkgs.fpp # for tmux-fpp
    pkgs.extract_url # for tmux-urlview
    pkgs.jq # json processor
    pkgs.jsonnet # templating with json
    pkgs.gh # official Github CLI
    pkgs.pueue # long running task manager

    # misc
    pkgs.neofetch
  ];
}
