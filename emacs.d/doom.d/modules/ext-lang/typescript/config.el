;;; ext-lang/typescript/config.el -*- lexical-binding: t; -*-

;; typescript-ts-mode is a builtin mode
(after! typescript-ts-mode
  (when (modulep! :tools lsp)
    (add-hook 'typescript-ts-mode-hook #'lsp! 'append)))
