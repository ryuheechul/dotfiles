{
  pkgs ? import <nixpkgs> (import ./overlays),
}:

let
  emacs = import ./custom/emacs.nix { pkgs = pkgs; };
  urlview = import ./custom/urlview.nix { pkgs = pkgs; };
in
with pkgs;
# just for fun/testing
[
  hello
  cowsay
  fortune
  neofetch
]
++
  # linuxify - enhancing linux compatibility
  # not that most these tools don't exist on darwin nor installed on linux by default
  # but installing these to "replace" them enables using macOS to feel less gaps with linux
  [
    git # Distributed version control system
    gnumake # gnu make
    time # Tool that runs programs and summarizes the system resources they use
    less # A more advanced file pager than ‘more’
    gnugrep # GNU implementation of the Unix grep command
    coreutils # like for readlink
    gnused # GNU sed, a batch stream editor
    gawk # GNU implementation of the Awk programming language
    ps
    pv # Tool for monitoring the progress of data through a pipeline
    pstree # Show the set of running processes as a tree
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
    fontconfig # A library for font customization and configuration
  ]
++
  # shell
  [
    bashInteractive # use latest bash
    zsh # my favorite shell interface
    starship # cross-shell prompt
    tmux # terminal multiplexer
    zellij # A terminal workspace with batteries included
  ]
++
  # editor - mostly for neovim
  [
    emacs # coming from my customization at ./custom/emacs.nix
    neovim # my favorite editor
    neovim-remote # 👌 Support for --remote and friends.
    tree-sitter # An incremental parsing system for programming tools
    # clang-tools # Standalone command line tools for C++ development
    rust-analyzer # An experimental modular compiler frontend for the Rust language
    stylua # An opinionated Lua code formatter
  ]
++

  # modern tools - i.e. https://github.com/ibraheemdev/modern-unix
  [
    fzf # A command-line fuzzy finder
    skim # Command-line fuzzy finder written in Rust
    # gotop # terminal based graphical activity monitor
    bottom # bottom instead of top
    yq-go # yaml processor and it's not a jq wrapper
    dasel # jq, yq replacement
    hyperfine # Command-line benchmarking tool
    httpie # A command line HTTP client whose goal is to make CLI human-friendly
    hurl # Command line tool that performs HTTP requests defined in a simple plain text format
    xh # httpie reimplementation - use with `xh` and `xhs`
    curlie # front-end to curl that tries to mimic httpie
    choose # a human-friendly and fast alternative to cut and (sometimes) awk
    sq # Swiss army knife for data (jq like experience for SQL database)
    jq # json processor
    jqp # a TUI playground for exploring jq
    # not sure if sqlite is "modern" but keep it here so I can avoid having to build sqlite by an arbitrary npm package which happened quite often
    sqlite # A self-contained, serverless, zero-configuration, transactional SQL database engine
    earthly # Build automation for the container era
    dogdns # Command-line DNS client - as a replacement of dig
  ]
++
  # viewer
  [
    vivid # LS_COLORS generator
    eza # A modern, maintained replacement for ls
    bat # cat with wings
    # comment out due to build error + not really using it at the moment
    # rich-cli # a command line toolbox for fancy output in the terminal
    hexyl # A command-line hex viewer - to supplement `xxd`, `hexdump`
    glow # markdown render on cli
    lf # terminal file manager
    fx # json viewer
    du-dust # du + rust = dust. Like du but more intuitive - use with `dust`
    duf # Disk Usage/Free Utility
    gping # ping with graph
    fpp # for tmux-fpp
    urlview # for tmux-urlview
    extract_url # for tmux-urlview
    viu # A command-line application to view images from the terminal written in Rust
    grc # A generic text colouriser
  ]
++
  # search
  [
    ripgrep # modern grep
    silver-searcher # A code-searching tool similar to ack, but faster
    amber # A code search-and-replace tool - use it with `ambs` and `ambr`
    codespelunker # A command code search tool - use with `cs [term]` which works kinda like `rg [term] | fzf`
    fasd # Command-line productivity booster, offers quick access to files and directories
    sad # search and replace | Space Age seD - ex) `fd . | sad find replace`
    fd # A simple, fast and user-friendly alternative to find
  ]
++
  # enhance git/github experience
  [
    tig # Text-mode interface for git
    gfold # help keep track of your Git repositories locally
    git-filter-repo # quickly rewrite git repository history - read https://stackoverflow.com/a/69947947/1570165 for usage
    delta # syntax-highlighting pager for git
    difftastic # A syntax-aware diff
    gh # official Github CLI
    act # Run your GitHub Actions locally
  ]
++
  # helper
  [
    fswatch # A cross-platform file change monitor with multiple backends
    watch # execute a program periodically
    viddy # A modern watch command
    entr # Run arbitrary commands when files change
    pueue # long running task manager
    # taskell # CLI kanban
    # jsonnet # Purely-functional configuration language that helps you define JSON data
    go-jsonnet # An implementation of Jsonnet in pure Go
    ansifilter # ANSI sequence filter - like [ansi2txt](https://github.com/kilobyte/colorized-logs)
    dos2unix # Convert text files with DOS or Mac line breaks to Unix line breaks and vice versa
    tldr # Simplified and community-driven man pages
    cht-sh # CLI client for cheat.sh, a community driven cheat sheet
    mosh # Mobile shell
    gum # Tasty Bubble Gum for your shell
    vale # A syntax-aware linter for prose built with speed and extensibility in mind
    vhs # A tool for generating terminal GIFs with code
    nix-output-monitor # Parses output of nix-build to show additional information
    iperf # Tool to measure IP bandwidth using UDP or TCP
    lynis # Security auditing tool for Linux, macOS, and UNIX-based systems
  ]
++ (import ./lang { pkgs = pkgs; })
++ (import ./fonts { pkgs = pkgs; })
++ (import ./linux.nix { pkgs = pkgs; })
++ (import ./extra/default.nix { pkgs = pkgs; })
