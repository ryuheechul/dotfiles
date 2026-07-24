;;; compat/neovim/sync.el -*- lexical-binding: t; -*-

;; nvim's `autoread` (+ the autoread/shortmess tweak in boot/misc.lua):
;; a file changed outside emacs (git checkout, another editor, a
;; formatter...) reloads automatically instead of asking "file changed on
;; disk, really edit?" later; non-file buffers like dired refresh too
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; same idea, the other direction: visit a file from a magit hunk (`e'),
;; save, `q' back - the status buffer should already show the new diff,
;; not the stale one from before the edit. global-auto-revert-non-file-
;; buffers above is timer/focus-polled, not immediate, and doesn't
;; reliably catch this; magit's own magit-after-save-refresh-status
;; (intended for after-save-hook, not wired by default) refreshes right
;; on save instead
(add-hook 'after-save-hook #'magit-after-save-refresh-status)

;; restore the last cursor position when reopening a file (nvim: the
;; LastPlace autocmd in boot/misc.lua) - measured OFF in this doom
;; checkout, so enable it here. saveplace's default
;; `save-place-ignore-files-regexp' already excludes COMMIT_EDITMSG,
;; matching the gitcommit exclusion the nvim autocmd makes
(save-place-mode 1)
