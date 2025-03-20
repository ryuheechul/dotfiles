;;; tools/treesit-support/config.el -*- lexical-binding: t; -*-

(use-package! treesit-langs)

(after! treesit-langs
  (treesit-langs-major-mode-setup))

;;;; this is a next TODO for this module
;; (use-package! treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))
