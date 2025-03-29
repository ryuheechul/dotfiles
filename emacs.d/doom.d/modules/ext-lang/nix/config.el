;;; ext-lang/nix/config.el -*- lexical-binding: t; -*-

(use-package! nix-ts-mode
  :defer
  :mode "\\.nix\\'"
  :init
  (when (and (modulep! :tools lsp)(modulep! +lsp))
    ;; without this lsp doesn't kick off
    (add-hook 'nix-ts-mode-hook #'lsp! 'append)))

(after! lsp-mode
  (setq lsp-nix-nixd-formatting-command [ "nixfmt" ]))
