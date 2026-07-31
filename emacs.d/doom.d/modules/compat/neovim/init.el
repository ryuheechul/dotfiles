;;; compat/neovim/init.el -*- lexical-binding: t; -*-

;; a module's init.el runs at doom's early-init stage, before any package
;; (evil included) loads - which is exactly what the evil-want-* family
;; requires; ./config.el is too late for these (doom's own :editor evil
;; module uses the same split for its evil-want-* settings)

;; nvim yanks to END OF LINE on Y (its default since 0.6, and explicit in
;; ../../../../../nvim/lua/plugins/keymaps.lua); evil defaults to whole line
(setq evil-want-Y-yank-to-eol t)

;; nvim remaps j/k/0/$ to visual-line-aware equivalents only in NORMAL mode,
;; not in operator-pending or visual. `evil-respect-visual-line-mode' is too
;; broad: it creates motion-state bindings that leak into operator-pending and
;; visual via `:enable', causing dd/yy/V to operate on screen lines instead of
;; logical lines.  We replicate nvim's narrow scope in navigate.el instead.
(setq evil-respect-visual-line-mode nil)
