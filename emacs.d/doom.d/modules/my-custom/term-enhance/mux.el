;;; $DOOMDIR/modules/my-custom/term-enhance/mux.el -*- lexical-binding: t; -*-

;; Terminal pane operations shared by Ghostel and VTerm.

;; emacs-as-a-multiplexer commands shared by both backends' mw* aliases in
;; ../../../../../zsh/integration/multiplexers - ./ghostel.el and ./vterm.el
;; each only supply the couple of lines that differ (which function actually
;; opens a new terminal buffer), and register these into their own
;; `*-eval-cmds' under the same "mux/*" names so the shell side never has to
;; know which one is live (it already does, implicitly, by which bridge -
;; `ghostel_cmd'/`vterm_cmd' - it called)

(defun term-enhance/mux-new-shell (spawn-fn &optional winop)
  "Open a fresh terminal pane, after optionally splitting WINOP right/below.
SPAWN-FN is a backend-specific thunk that opens the actual terminal buffer
in the (by then correctly split/selected) current window."
  ;; bridge calls run with the calling terminal's buffer current, but not
  ;; necessarily its window selected - reselect so window ops land there
  (let ((win (get-buffer-window (current-buffer))))
    (when win (select-window win)))
  (pcase winop
    ("right" (select-window (split-window-right)))
    ("below" (select-window (split-window-below))))
  (prep-env-for-term)
  (funcall spawn-fn))

(defvar term-enhance/mux--zoom-wconf nil
  "Window configuration to restore on un-zoom (nil = not zoomed).")

(defun term-enhance/mux-zoom ()
  "Toggle maximizing the calling terminal's window, like tmux zoom.
A real toggle: `doom/window-maximize-buffer' turned out to be plain
`delete-other-windows' (winner-undo is its only way back), so save and
restore the window configuration ourselves."
  (let ((win (get-buffer-window (current-buffer))))
    (when win (select-window win)))
  (if (and term-enhance/mux--zoom-wconf (one-window-p))
      (progn
        (set-window-configuration term-enhance/mux--zoom-wconf)
        (setq term-enhance/mux--zoom-wconf nil)
        ;; the restored configuration also restores which window was
        ;; selected back then - return to the pane that asked to un-zoom
        (let ((win (get-buffer-window (current-buffer))))
          (when win (select-window win))))
    ;; (unless ...) keeps it nil when already single-window, so zooming
    ;; there stays a no-op instead of arming a pointless un-zoom
    (setq term-enhance/mux--zoom-wconf (unless (one-window-p)
                                          (current-window-configuration)))
    (delete-other-windows)))

;; make a plain `exit' behave like closing a terminal pane too: both
;; backends' `*-kill-buffer-on-exit' already kill the buffer, but the
;; window stays behind showing whatever buffer came next - remove it as
;; well. `ghostel-exit-functions'/`vterm-exit-functions' both run this
;; before the kill, with the identical (buf event) signature, so the
;; buffer's window is still discoverable here.
;;
;; the `(when buf ...)' guard matters more than it looks: ghostel's sentinel
;; only calls this hook `(when (buffer-live-p buf) ...)', so it never sees a
;; dead buffer here - but vterm's always calls it, passing nil once the
;; buffer's already gone (e.g. after `term-enhance/mux-close-pane' already
;; killed it and its process exits asynchronously afterward). without the
;; guard, `(get-buffer-window nil)' silently falls back to *the currently
;; selected window* and deletes that instead - an unrelated sibling pane -
;; which is exactly the "3 windows -> 1 instead of 2" bug this fixes
(defun term-enhance/mux-close-window-on-exit (buf _event)
  (when buf
    (let ((win (get-buffer-window buf)))
      (when (and win (window-live-p win) (not (frame-root-window-p win)))
        (delete-window win)))))

;; kill the current terminal buffer without any "has a running process;
;; kill it?" prompt: the shell just accepted the `q' that got us here, so
;; nothing of value is running - in fact by OSC 133 bookkeeping that very
;; `q' alias is STILL running (ghostel_cmd fires its elisp before the
;; command-finished D marker), which is exactly what ghostel's kill query
;; (`ghostel-query-before-killing' 'auto) flags. That query lives in the
;; buffer-LOCAL `kill-buffer-query-functions', so a let-bind of the global
;; value can't mask it - clear the dying buffer's local hook instead
;; (also moots vterm's global process query, since a local nil stops the
;; global value from being consulted at all)
(defun term-enhance/kill-terminal-no-questions ()
  (setq-local kill-buffer-query-functions nil)
  (kill-current-buffer))

;; what `q' (`ghostel/hide'/`vterm/hide') should do for a plain split pane
;; (not the popup or full-window singleton, which each backend already
;; knows how to warm-hide instead so the shell stays alive for a fast
;; re-toggle): behave like closing a terminal pane - kill the buffer and
;; remove its window in one action (the manual equivalent was exit + kill
;; file buffer + delete window)
(defun term-enhance/mux-close-pane ()
  (let ((win (get-buffer-window (current-buffer))))
    (term-enhance/kill-terminal-no-questions)
    (when (and win (window-live-p win) (not (frame-root-window-p win)))
      (delete-window win))))

;; what `q' should do when the terminal is the sole window but NOT the
;; backend's full-window singleton - e.g. the popup buffer promoted to
;; full frame by smart-quit's duplicate collapse
;; (../../compat/neovim/smart-quit.el). The backends' full-w/toggle would
;; target the singleton instead of this buffer and look like a refusal;
;; warm-bury when there's somewhere to go back to, kill otherwise (the
;; same choice each full-w/toggle makes for its own buffer)
(defun term-enhance/bury-or-kill-sole-terminal ()
  (if (> (length (doom-real-buffer-list)) 1)
      (bury-buffer)
    (term-enhance/kill-terminal-no-questions)))
