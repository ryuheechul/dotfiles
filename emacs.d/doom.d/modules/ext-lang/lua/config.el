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

;; prefer stylua (via apheleia) over lua-language-server's own bundled
;; formatter (what eglot-format-buffer would otherwise call) - matches
;; nvim's efm+stylua setup; see ../../tools/lsp-support/config.el for
;; what this variable does. relies on that module loading first
;; (init.el: tools lsp-support before ext-lang lua) to already have
;; defvar'd it
(add-to-list '+format-with-eglot-excluded-modes 'lua-mode)
(add-to-list '+format-with-eglot-excluded-modes 'lua-ts-mode)

;; apheleia's stock stylua entry is just ("stylua" "-") - with no
;; --search-parent-directories, stylua only checks stylua's own cwd for a
;; stylua.toml, so it'd never find e.g. ../../../../../nvim/stylua.toml
;; from a nested nvim/lua/boot/*.lua buffer and would silently fall back
;; to stylua's built-in defaults instead. nvim's own efm+stylua setup
;; already passes this flag (see that file's header comment) - match it
;; here so both editors format lua identically
(after! apheleia
  (setf (alist-get 'stylua apheleia-formatters) '("stylua" "--search-parent-directories" "-")))

