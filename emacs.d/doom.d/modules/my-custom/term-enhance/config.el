;;; $DOOMDIR/modules/my-custom/term-enhance/config.el -*- lexical-binding: t; -*-

;; terminal-agnostic plumbing shared by ./ghostel.el and ./vterm.el - nothing
;; here should ever need to know which backend is active. per-backend
;; specifics live in ./ghostel.el / ./vterm.el; ./integration.el decides
;; which of the two a shared keybinding actually calls.

(setq tui-emacs (eq window-system nil))

;; setenv wrapper that works for tramp remote shell as well
(defun settermenv (key val)
  ;; for a local env, it's simple as that
  (setenv key val)
  ;; for (remote) env for tramp
  (if (boundp 'tramp-remote-process-environment)
      (if (eq val nil)
          ;; setting `nil' is more complicated but can be done via finding the item and remove it
          (setq tramp-remote-process-environment
                (cl-remove-if
                 (lambda (elt) (string-match-p (concat "^" key "=") elt))
                 tramp-remote-process-environment))
        ;; setting an actual value is simpler
        (add-to-list 'tramp-remote-process-environment (concat key "=" val)))))


;; extension point for backend-specific setup that has to run on every
;; `prep-env-for-term' call but doesn't belong in this terminal-agnostic
;; function itself (e.g. vterm's `vterm-shell' tramp workaround, hooked in
;; from ./vterm.el) - stays empty (a no-op) for backends with nothing to add
(defvar term-enhance/prep-env-hook nil)

;; this is a function to let to prepare my (zsh) shell work well within emacs
;; needed to be called before a terminal launches
;; to cover all possible scenario it's currently being called in multiple places
(defun prep-env-for-term ()
  (run-hooks 'term-enhance/prep-env-hook)
  (if tui-emacs (settermenv "TUI_EMACS" "1"))
  ;; desired default
  (settermenv "INSIDE_DOOM_EMACS" "1")
  (settermenv "UNSET_ALL_MY_ZSH_STUFF_LOADED" "1")
  (settermenv "UNSET_MY_BASIC_ZSH_STUFF_LOADED" "1")
  (settermenv "UNSET_HOST_ALWAYS_USE_TMUX" "1"))
;; invoke on startup so the local shell is ready
(prep-env-for-term)
;; invoke again for when tramp is ready
(after! tramp-sh (prep-env-for-term))

;; shared guts of "open the current file in a quick nvim, launched from
;; inside the terminal backend" - ./ghostel.el and ./vterm.el each only
;; supply the terminal-specific part (which `*-with-cmd' to call, and
;; vterm's extra TERM= prefix)
(defun term-enhance/prep-env-for-quick-editor ()
  (settermenv "ALL_MY_ZSH_STUFF_LOADED" "1")
  (settermenv "fast_shell_in_editor" "1")
  (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" "1"))

(defun term-enhance/undo-env-for-quick-editor ()
  (settermenv "ALL_MY_ZSH_STUFF_LOADED" nil)
  (settermenv "fast_shell_in_editor" nil)
  (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" nil))

(defun term-enhance/nvim-open-cmd (file-name line-number)
  ;; `my_nvim_forget_line_number=1' enables opening the same line in neovim as emacs
  (concat "my_nvim_forget_line_number=1 nvim +" line-number " -- " file-name))

(defun term-enhance/current-file-and-line ()
  "Return (FILE-NAME . LINE-NUMBER) for the current buffer, tramp-aware."
  ;; `file-remote-p' is core (no tramp load needed for local files, and for a
  ;; remote buffer tramp is already loaded)
  (cons (if (file-remote-p buffer-file-name)
            (tramp-file-local-name buffer-file-name)
          buffer-file-name)
        (number-to-string (line-number-at-pos))))

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

;; what `q' (`ghostel/hide'/`vterm/hide') should do for a plain split pane
;; (not the popup or full-window singleton, which each backend already
;; knows how to warm-hide instead so the shell stays alive for a fast
;; re-toggle): behave like closing a terminal pane - kill the buffer and
;; remove its window in one action (the manual equivalent was exit + kill
;; file buffer + delete window). `kill-buffer-query-functions' nil skips
;; the running-process prompt
(defun term-enhance/mux-close-pane ()
  (let ((buf (current-buffer))
        (win (get-buffer-window (current-buffer)))
        (kill-buffer-query-functions nil))
    (kill-buffer buf)
    (when (and win (window-live-p win) (not (frame-root-window-p win)))
      (delete-window win))))

;; manage code related theme separately
(load! "theme")      ;; ./theme.el
(load! "ghostel")     ;; ./ghostel.el
(load! "vterm")       ;; ./vterm.el
(load! "integration") ;; ./integration.el
