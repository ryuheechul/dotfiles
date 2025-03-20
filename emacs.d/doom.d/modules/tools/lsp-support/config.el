;;; tools/lsp-support/config.el -*- lexical-binding: t; -*-

;; define unwanted clients so that lsp-mode avoid using it
;; - https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project
(setq lsp-disabled-clients
      '(nix-nil ;; to favor nixd instead
        ))

;; thanks to https://github.com/emacs-lsp/lsp-mode/issues/3944#issuecomment-2548991516
(define-minor-mode lsp-format-buffer-on-save-mode
  "Run lsp-format-buffer as a before-save-hook."
  :lighter " fmt"
  (if lsp-format-buffer-on-save-mode
      (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (remove-hook 'before-save-hook 'lsp-format-buffer t)))

;; let the minor mode above to be active when lsp is kicked off
(add-hook 'lsp-after-open-hook (lambda () (lsp-format-buffer-on-save-mode t)))
