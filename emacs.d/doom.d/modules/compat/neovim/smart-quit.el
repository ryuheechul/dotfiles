;;; compat/neovim/smart-quit.el -*- lexical-binding: t; -*-

;; follow the customized quit semantics of my neovim setup
;; (../../../../../nvim/lua/utils/my-smart-quit.lua): quitting at the last
;; window/buffer with nothing unsaved should leave the editor, not silently
;; do nothing - which is what killing a buffer like *scratch* or the
;; dashboard normally looks like, since doom/emacs just regenerates them.
;;
;; one entrypoint, branches inside - mirroring my-smart-quit.lua's shape
;; (one function, `quit_listed'/`quit_unlisted' as internal branches).
;; Keep it that way: the previous shape - window-count and buffer-safety
;; checks split across two separately-advised functions - let the
;; side-window-terminal case fall through both. Wired into `q' from two
;; places: evil-normal-state-map (below) and +dashboard-mode-map directly
;; (special-mode's own `q' -> `quit-window' otherwise wins there, since
;; a major-mode keymap beats the global evil-normal-state-map one).
;;
;; behavior matrix (side-window rows regression-tested in
;; ../../../tests/smart-quit-tests.el, the rest verified live):
;;
;; buffer            | environment          | other real buf? | unsaved?     | q does
;; ------------------+----------------------+------------------+--------------+---------------------------------------------
;; (in side-window)  | any                  | -                | -            | close that side-window
;; any, last main win| any                  | (side-window terminal present) | just close this buffer - never quits with a live terminal around
;; file, last one    | emacsclient TUI      | no               | no           | quit client, silently (real shell to return to)
;; file, last one    | GUI / TUI no client  | no               | no           | ask "Quit Emacs?" first
;; file              | any                  | yes              | no           | just close this buffer
;; file, last one    | any                  | no               | yes,elsewhere| just close this buffer - never auto-quits with unsaved work around
;; dashboard, alone  | emacsclient TUI      | no               | no           | quit client, silently
;; dashboard, alone  | GUI                  | no               | no           | ask "Quit Emacs?" first
;; dashboard         | any                  | yes (file open)  | -            | bury dashboard, back to that file (`quit-window')
;; *scratch*, alone  | emacsclient TUI      | no               | no           | quit client, silently
;; *scratch*, alone  | GUI                  | no               | no           | ask "Quit Emacs?" first
;; *scratch*         | any                  | yes (file open)  | -            | just close/bury scratch, back to that file
;;
;; "other real buf?" = `doom-real-buffer-list' has something left besides
;; the buffer being closed. both *scratch* and *doom* count as "unreal" by
;; doom's own rules. multiple MAIN windows open route through
;; `+workspace/close-window-or-workspace' instead (its own
;; dedicated/workspace/frame cascade already handles that well - not
;; reinvented here). `q' typed AT a terminal's shell prompt is the shell
;; alias (../../my-custom/term-enhance/'s hide/close-pane logic) and never
;; reaches this function; `q' in evil NORMAL state on a terminal in a
;; side-window does, and hits the close-that-side-window branch.

(defun window-side-p (w)
  "Non-nil if window W is a side-window (e.g. a terminal popup).
Same raw check Emacs's own window.el uses internally (see e.g.
`window--sides-reverse-on-frame-p') - there's no dedicated public
predicate for this, just named here for reuse below."
  (window-parameter w 'window-side))

(defun close-window-or-buffer ()
  "Quit following the customized `:q' semantics of
nvim/lua/utils/my-smart-quit.lua - see this file's header comment for
the full behavior matrix."
  (interactive)
  (cond
   ;; `q' while IN a side-window popup (e.g. the terminal) - close it;
   ;; delete-window is safe here, a side-window is never the last main one
   ((window-side-p (selected-window)) (delete-window))
   ;; another MAIN window (side-windows don't count - deleting the last
   ;; main window is impossible) - let doom's own cascade handle deleting
   ;; this one (dedicated/workspace/frame fallbacks already solid there)
   ((seq-some (lambda (w) (and (not (eq w (selected-window)))
                               (not (window-side-p w))))
              (window-list))
    (+workspace/close-window-or-workspace))
   ;; only main window left
   (t
    (let* ((other-real-buffers (cl-remove (current-buffer) (doom-real-buffer-list) :test #'eq))
           (unsaved-somewhere-p (seq-some (lambda (b) (and (buffer-file-name b) (buffer-modified-p b)))
                                          (buffer-list)))
           (side-window-present (seq-some #'window-side-p (window-list)))
           (emacsclient-tui-p (and (not (display-graphic-p)) (frame-parameter nil 'client))))
      (cond
       ;; something to fall back to, unsaved work somewhere that a quit
       ;; shouldn't sweep away, or a live side-window terminal that a quit
       ;; would kill - just close this one; *doom* is never killed, bury
       ;; it instead (its own regeneration is more involved than
       ;; *scratch*'s).
       ;;
       ;; go through `quit-window' here too, not just for *doom*: plain
       ;; `kill-buffer' skips its bookkeeping, so a buffer displayed via
       ;; `pop-to-buffer-same-window' (e.g. a file visited from a magit
       ;; hunk via `e') leaves the window's `quit-restore' parameter
       ;; stale once emacs falls back to showing the previous buffer -
       ;; breaking THAT buffer's own next quit-window call (see
       ;; ../../../tests/smart-quit-tests.el)
       ((or other-real-buffers unsaved-somewhere-p side-window-present)
        (quit-window (not (string= (buffer-name) "*doom*")))
        ;; killing the last real buffer can make emacs fall back to the
        ;; very buffer the side-window terminal already shows - same
        ;; terminal in two windows. Collapse: drop the side-window so the
        ;; terminal takes the whole frame
        (dolist (w (window-list))
          (when (and (window-side-p w)
                     (not (eq w (selected-window)))
                     (eq (window-buffer w) (window-buffer (selected-window))))
            (delete-window w))))
       ;; truly nothing else, nothing unsaved - quit
       (emacsclient-tui-p (save-buffers-kill-terminal))
       ((y-or-n-p "Quit Emacs? ") (save-buffers-kill-terminal)))))))

(define-key evil-normal-state-map (kbd "q") #'close-window-or-buffer)
;; alternatively the above could be as simple as below
;; (define-key evil-normal-state-map (kbd "q") #'evil-quit)

;; the dashboard doesn't go through evil-normal-state-map at all: its
;; +dashboard-mode is derived from emacs' built-in `special-mode', which
;; binds `q' to `quit-window' directly - that wins over the global
;; binding above on the dashboard specifically. Give it the same
;; function instead (its *doom* branch already buries via `quit-window'
;; when another real buffer exists, matching what the old fallback did)
(map! :map +dashboard-mode-map :n "q" #'close-window-or-buffer)
