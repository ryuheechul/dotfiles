# `lspx` wrappers

This is where I put wrapper executables that make use https://github.com/thefrontside/lspx to overcome the lack of multi server support from eglot, https://github.com/joaotavora/eglot/discussions/1429

See these files to see how I weave the environment to help the lsp binaries discovered and also shadow them behind `lspx` and `emacs-lsp-booster` to improve the both performance and usability:
- `../../../emacs.d/doom.d/modules/tools/lsp-support`: to wrap with `emacs-lsp-booster`
- `../../../emacs.d/doom.d/shell/source.zsh`: to let neovim's installed lsp's too be discovered
- `../../../nix/home/programs/shells.nix`: to handle tramp access case
- `../../../zsh/path/set-basic`: for local access from Emacs for both
