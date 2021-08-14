{ pkgs ? import <nixpkgs> (import ./fallback/darwin/optional-overlaying.nix) }:

with pkgs;
[
  # just for fun and testing
  hello
  cowsay
  fortune

  # essential
  coreutils # like for readlink
  gnused # sed
  zsh # my favorite shell interface
  git # to replace possible old git comes with OS
  bash # use latest bash
  tmux # terminal multiplexer
  neovim # my favorite editor
  emacs-nox # terminal only emacs
  starship # cross-shell prompt
  fzf # A command-line fuzzy finder
  gotop # terminal based graphical activity monitor
  bottom # bottom instead of top
  gnupg # GnuPG
  wget # useful for downloading files

  # modern tools - i.e. https://github.com/ibraheemdev/modern-unix
  dasel # jq, yq replacement
  hyperfine # Command-line benchmarking tool
  xh # httpie reimplementation - use with `xh` and `xhs`
  curlie # front-end to curl that tries to mimic httpie
  choose # a human-friendly and fast alternative to cut and (sometimes) awk

  # editor - mostly for neovim
  tree-sitter # An incremental parsing system for programming tools
  clang-tools # Standalone command line tools for C++ development
  rust-analyzer # An experimental modular compiler frontend for the Rust language

  # viewer
  vivid # LS_COLORS generator
  exa # modern ls
  bat # cat with wings
  glow # markdown render on cli
  lf # terminal file manager
  fx # json viewer
  delta # syntax-highlighting pager for git
  du-dust # du + rust = dust. Like du but more intuitive - use with `dust`
  duf # Disk Usage/Free Utility
  gping # ping with graph

  # search
  ripgrep # modern grep
  amber # A code search-and-replace tool
  fasd # Command-line productivity booster, offers quick access to files and directories
  fd # mordern find

  # helper
  tig # git helper
  tldr # Simplified and community-driven man pages
  watch # execute a program periodically
  entr # Run arbitrary commands when files change
  # taskell # CLI kanban
  fpp # for tmux-fpp
  extract_url # for tmux-urlview
  jq # json processor
  jsonnet # templating with json
  gh # official Github CLI
  pueue # long running task manager

  # misc
  neofetch
]
++ (import ./extra/default.nix {pkgs=pkgs;})
++
(with bat-extras; [
  batman
  batgrep
  batdiff
  batwatch
  prettybat
])
++
(import ./lang.nix {pkgs=pkgs;})
