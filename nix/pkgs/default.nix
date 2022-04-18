{ pkgs ? import <nixpkgs> (import ./fallback/darwin/optional-overlaying.nix) }:

with pkgs;
# just for fun/testing
[
  hello
  cowsay
  fortune
  neofetch
] ++
# linuxify - enhancing linux compatibility
# not that most these tools don't exist on darwin nor installed on linux by default
# but installing these to "replace" them enables using macOS to feel less gaps with linux
[
  git # Distributed version control system
  gnumake # gnu make
  coreutils # like for readlink
  gnused # sed
  curl # Curl is a command-line tool for transferring data specified with URL syntax
  wget # useful for downloading files
  unzip # The UnZip package contains ZIP extraction utilities
  xsel # clipboard - https://ostechnix.com/access-clipboard-contents-using-xclip-and-xsel-in-linux/
  gnupg # GnuPG
  gcc # the GNU Compiler Collection
  # `ncurses` comes with utilities below - this also enhances experiences with terminal on macOS (darwin) a lot better
  # for example, although CLIs below exists on macOS, `infocmp` fails with `eterm-color` on macOS by default
  # but `ncurses` installed via nix, now `infocmp` works so `lf` now has terminfo to access to render colors that matches with my other terminals
  # - captoinfo, a termcap conversion tool
  # - clear, utility for clearing the screen
  # - infocmp, the terminfo decompiler
  # - tabs, set tabs on a terminal
  # - tic, the terminfo compiler
  # - toe, list (table of) terminfo entries
  # - tput, utility for retrieving terminal capabilities in shell scripts
  # - tset, to initialize the terminal
  ncurses
] ++
# shell
[
  bash # use latest bash
  zsh # my favorite shell interface
  starship # cross-shell prompt
  tmux # terminal multiplexer
  zellij # A terminal workspace with batteries included

] ++
  # editor - mostly for neovim
[
  # emacs editor including GUI, `emacs -nw` to run as TUI
  ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]))
  # above replace `emacs` to enable the use of libvterm
  neovim # my favorite editor
  tree-sitter # An incremental parsing system for programming tools
  # clang-tools # Standalone command line tools for C++ development
  rust-analyzer # An experimental modular compiler frontend for the Rust language
  stylua # An opinionated Lua code formatter

] ++

  # modern tools - i.e. https://github.com/ibraheemdev/modern-unix
[
  fzf # A command-line fuzzy finder
  # gotop # terminal based graphical activity monitor
  bottom # bottom instead of top
  yq-go # yaml processor and it's not a jq wrapper
  dasel # jq, yq replacement
  hyperfine # Command-line benchmarking tool
  httpie # A command line HTTP client whose goal is to make CLI human-friendly
  xh # httpie reimplementation - use with `xh` and `xhs`
  curlie # front-end to curl that tries to mimic httpie
  choose # a human-friendly and fast alternative to cut and (sometimes) awk
  jq # json processor
] ++
  # viewer
[
  vivid # LS_COLORS generator
  exa # modern ls
  bat # cat with wings
  glow # markdown render on cli
  lf # terminal file manager
  fx # json viewer
  du-dust # du + rust = dust. Like du but more intuitive - use with `dust`
  duf # Disk Usage/Free Utility
  gping # ping with graph
  fpp # for tmux-fpp
  extract_url # for tmux-urlview

] ++
  # search
[
  ripgrep # modern grep
  silver-searcher # A code-searching tool similar to ack, but faster
  amber # A code search-and-replace tool - use it with `ambs` and `ambr`
  fasd # Command-line productivity booster, offers quick access to files and directories
  fd # mordern find
] ++
  # enhance git/github experience
[
  tig # Text-mode interface for git
  gfold # help keep track of your Git repositories locally
  git-filter-repo # quickly rewrite git repository history - read https://stackoverflow.com/a/69947947/1570165 for usage
  delta # syntax-highlighting pager for git
  gh # official Github CLI
] ++
  # helper
[
  watch # execute a program periodically
  entr # Run arbitrary commands when files change
  pueue # long running task manager
  # taskell # CLI kanban
  jsonnet # templating with json
  ansifilter # ANSI sequence filter - like [ansi2txt](https://github.com/kilobyte/colorized-logs)
  tldr # Simplified and community-driven man pages

]
++ (import ./lang.nix {pkgs=pkgs;})
++ (import ./extra/default.nix {pkgs=pkgs;})
