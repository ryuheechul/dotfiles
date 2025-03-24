;;; ext-lang/lua/config.el -*- lexical-binding: t; -*-

;; it's working but syntax is not highlighted, maybe it will be fixed by emacs one day
(use-package! lua-ts-mode
  :defer
  :init
  (when (modulep! +lsp)
    ;; without this lsp doesn't kick off
    (add-hook 'lua-ts-mode-hook #'lsp! 'append)))

