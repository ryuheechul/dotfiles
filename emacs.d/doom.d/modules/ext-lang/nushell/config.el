;;; ext-lang/nushell/config.el -*- lexical-binding: t; -*-

(use-package! nu-ts-mode
  :defer
  :mode "\\.nu\\'"
  :interpreter "nu"
  :config
  ;; nu-ts-mode handles font-lock, indentation, and comment syntax.
  ;;
  ;; NOTE: The tree-sitter grammar for Nushell is required. Install it with:
  ;;   M-x treesit-install-language-grammar RET nu RET
  ;;
  ;; The grammar source: https://github.com/nushell/tree-sitter-nu
  )
