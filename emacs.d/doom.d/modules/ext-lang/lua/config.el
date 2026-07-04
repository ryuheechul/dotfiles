;;; ext-lang/lua/config.el -*- lexical-binding: t; -*-

;; it's working but syntax is not highlighted, maybe it will be fixed by emacs one day
(use-package! lua-ts-mode
  :defer
  :init
  (when (modulep! +lsp)
    ;; without this lsp doesn't kick off
    (add-hook 'lua-ts-mode-hook #'lsp! 'append)))

;; .lua files actually open in lua-mode (doom's :lang lua module, which
;; has no +lsp flag in init.el) - nothing routes them to lua-ts-mode, so
;; the hook above never fired and lookups (gd & co) were silent no-ops in
;; lua buffers. hook lua-mode as well; eglot's stock server entry already
;; covers both (lua-mode lua-ts-mode)
(when (modulep! +lsp)
  (add-hook 'lua-mode-hook #'lsp! 'append))

