;;; compat/neovim/smart-quit.el -*- lexical-binding: t; -*-

;; mimic neovim's overall quit semantics
;; (../../../../../nvim/lua/utils/my-smart-quit.lua): quitting at the last
;; window/buffer with nothing unsaved should leave the editor, not silently
;; do nothing - which is what killing a buffer like *scratch* or the
;; dashboard normally looks like, since doom/emacs just regenerates them.
;; wired into `q' from two places: ../../my-custom/morevil/config.el's
;; `close-buffer-or-doom' (via advice, below), and +dashboard-mode-map
;; directly (special-mode's own `q' -> `quit-window' otherwise wins there).
;;
;; behavior matrix (every row verified live):
;;
;; buffer            | environment          | other real buf? | unsaved?     | q does
;; ------------------+----------------------+------------------+--------------+---------------------------------------------
;; file, last one    | emacsclient TUI      | no               | no           | quit client, silently (real shell to return to)
;; file, last one    | GUI / TUI no client  | no               | no           | ask "Quit Emacs?" first
;; file              | any                  | yes              | no           | just close this buffer
;; file, last one    | any                  | no               | yes,elsewhere| just close this buffer - never auto-quits with unsaved work around
;; dashboard, alone  | emacsclient TUI      | no               | no           | quit client, silently
;; dashboard, alone  | GUI                  | no               | no           | ask "Quit Emacs?" first
;; dashboard         | any                  | yes (file open)  | -            | bury dashboard, back to that file (original `quit-window')
;; *scratch*, alone  | emacsclient TUI      | no               | no           | quit client, silently
;; *scratch*, alone  | GUI                  | no               | no           | ask "Quit Emacs?" first
;; *scratch*         | any                  | yes (file open)  | -            | just close/bury scratch, back to that file
;;
;; "other real buf?" = `doom-real-buffer-list' has something left besides
;; the buffer being closed. both *scratch* and *doom* count as "unreal" by
;; doom's own rules, which is what lets them quit without being blocked by
;; each other. rows with multiple windows open (-> `+workspace/close-window
;; -or-workspace') or on a ghostel/vterm terminal buffer (->
;; ../../my-custom/term-enhance/'s own hide/close-pane logic) never reach
;; this file at all.

(defun smart-quit-if-safe ()
  "Quit like neovim's `:q' would, if it's safe to.

Safe means: nothing unsaved anywhere, and no OTHER real buffer
\(`doom-real-buffer-list', excluding the current one) to usefully fall
back to instead. When safe, disconnects the emacsclient TUI frame with no
prompt (mirroring neovim exiting straight back to the shell), or asks
\"Quit Emacs?\" first otherwise (GUI, or no client to disconnect).

Returns non-nil when it handled the quit (or the user declined the
prompt), so callers can use this as `:before-until' advice or an `or'
fallback and only run their own fallback behavior when this returns nil."
  (let* ((other-real-buffers (cl-remove (current-buffer) (doom-real-buffer-list) :test #'eq))
         (unsaved-buffers (seq-some (lambda (b) (and (buffer-file-name b) (buffer-modified-p b)))
                                    (buffer-list)))
         (emacsclient-tui-p (and (not (display-graphic-p)) (frame-parameter nil 'client))))
    (when (and (not other-real-buffers) (not unsaved-buffers))
      (if emacsclient-tui-p
          (save-buffers-kill-terminal)
        (when (y-or-n-p "Quit Emacs? ")
          (save-buffers-kill-terminal)))
      t)))

(advice-add #'close-buffer-or-doom :before-until #'smart-quit-if-safe)

;; the dashboard doesn't go through `close-buffer-or-doom' at all: its
;; +dashboard-mode is derived from emacs' built-in `special-mode', which
;; binds `q' to `quit-window' directly - that wins over
;; ../../my-custom/morevil/config.el's `evil-normal-state-map' binding on
;; the dashboard specifically, so the advice above never gets a chance to
;; run there. give the dashboard the same check, falling back to the
;; original `quit-window' otherwise
(map! :map +dashboard-mode-map :n "q"
      (lambda () (interactive) (or (smart-quit-if-safe) (quit-window))))
