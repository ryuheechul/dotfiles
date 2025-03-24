;;; tools/lsp-support/config.el -*- lexical-binding: t; -*-

;; LSP working with `tramp' requires additional touch and see ../tramp-support/ for some tweaks
;; LSP executables already installed via neovim is added to the =$PATH= via =../../compat/neovim/=

(define-minor-mode +format-with-eglot-mode
  "Run eglot-format-buffer as a before-save-hook."
  :lighter " fmt"
  (if +format-with-eglot-mode
      (progn
        (add-hook 'before-save-hook 'eglot-format-buffer -100 t)
        ;; turn off apheleia with lsp - see the line for =:editor format +onsave= =../../../init.el= for why I do this manually
        (apheleia-mode -1))
    (progn
      (remove-hook 'before-save-hook 'eglot-format-buffer t)
      ;; turn back on
      (apheleia-mode nil))))

(when (modulep! :tools lsp +eglot)
  ;; look at buffers like =*EGLOT ([dir]/[xyz-mode])) events*= (via stderr) if booster is used or not
  (use-package! eglot-booster
    :after eglot
    :config (eglot-booster-mode))
  ;; (setq eglot-booster-io-only t) ;; is this way would be better with emacs 30+?
  (add-hook 'eglot-managed-mode-hook #'+format-with-eglot-mode))

;; NOTE from here and below, I actually choose to not use neither `lsp-mode' nor `lsp-bridge'
;; since I chose `eglot' for a choice of lsp layer, see ./readme.org for why I made that decision
;;
;; The reason why I keep these unused code are:
;; - I worked hard to come up with the minimal config that allows these tools to work
;;   - I don't want the work to be evaporated
;;   - who know I will choose them again in the future
;; - also these code are guarded with `modulep!', so not gonnba be called

;; when it's intended to be used with `lsp-mode'
(when (modulep! :tools lsp -eglot)
  ;; define unwanted clients so that lsp-mode avoid using it
  ;; - https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project
  (setq lsp-disabled-clients
        '(nix-nil ;; to favor nixd instead
          nix-nil-tramp ;; since the same ls for tramp gets `-tramp' suffix
          )))

;; when I don't want to rely on either of `eglot' nor `lsp-mode', choose `lsp-bridge'
(when (not (modulep! :tools lsp))
  (setq lsp-bridge-log-level 'debug)

  (define-minor-mode lspb-code-format-on-save-mode
    "Run lsp-bridge-code-format as a before-save-hook."
    :lighter " fmt"
    (if lspb-code-format-on-save-mode
        (add-hook 'before-save-hook 'lsp-bridge-code-format t t)
      (remove-hook 'before-save-hook 'lsp-bridge-code-format t)))

  ;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#remote-ssh-server
  (setq lsp-bridge-remote-start-automatically t)

  ;; let the minor mode above to be active when lsp is kicked off
  (add-hook 'lsp-bridge-mode-hook (lambda () (lspb-code-format-on-save-mode t)))

  (use-package! lsp-bridge
    :config
    (global-lsp-bridge-mode)))
