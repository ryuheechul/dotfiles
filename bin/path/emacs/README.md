# bin/path/emacs

three ways of accessing this from emacs

1. Tramp (SSH) via ~/.profile that is generated via ../../../nix/home/programs/shells.nix
2. non-interactive command search via `(add-to-list 'exec-path (concat (getenv "my_dot_d") "/bin/path/emacs"))`
3. shell (e.g. vterm) within inside via ../../../zsh/path/set-basic

due to how emacs works

this is mainly for emacs-lsp-booster to wrap things with minimal approach

but not limiting its usage just to that


## Helping `emacs-lsp-booster` via this directory became obsolete, thanks to https://github.com/jdtsmith/eglot-booster
