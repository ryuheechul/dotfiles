;;; compat/neovim/clipboard.el -*- lexical-binding: t; -*-

;; nvim-parity clipboard PASTE for TTY frames: `:os (tty +osc)' already
;; sends kills OUT via OSC 52, but OSC 52 is write-only in practice
;; (terminals refuse clipboard reads - e.g. ghostty defaults
;; clipboard-read to ask), so evil's p in a terminal frame never saw the
;; system clipboard that GUI frames read natively. Same layering rule as
;; nvim's provider pick (../../../../../nvim/lua/boot/misc.lua): prefer
;; the immediate native provider when it's there - GUI frames keep the
;; native path, TTY frames read pbpaste. That's usually the stack-aware
;; wrapper (../../../../../bin/path/default/pbpaste: native mac / osc /
;; xsel), so remote frames work too wherever the wrapper does; no
;; pbpaste at all = copy-only, like nvim under zellij ("One clipboard,
;; every layer" in ../../../../../docs/philosophy.md). Deduping against
;; the kill-ring head is sound because +osc keeps the system clipboard
;; in step with our own kills - a fresh internal kill is already ON the
;; clipboard, so it never gets shadowed by a stale read.
(defvar +neovim/--gui-paste-function interprogram-paste-function
  "The stock (GUI) paste function this module wraps.")

(defun +neovim/frame-aware-paste ()
  "Read the system clipboard appropriately for the selected frame."
  (if (display-graphic-p)
      (and +neovim/--gui-paste-function
           (funcall +neovim/--gui-paste-function))
    (when (executable-find "pbpaste")
      (let ((text (shell-command-to-string "pbpaste")))
        (unless (or (string-empty-p text)
                    (equal text (car kill-ring)))
          text)))))
(setq interprogram-paste-function #'+neovim/frame-aware-paste)
