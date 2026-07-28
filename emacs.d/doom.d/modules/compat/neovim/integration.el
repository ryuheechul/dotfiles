;;; compat/neovim/integration.el -*- lexical-binding: t; -*-

;; Tmux pane navigation - neovim-style C-hjkl pane switching via
;; christoomey/vim-tmux-navigator semantics.  Used by both the intercept
;; map in ../../my-custom/morevil/config.el (normal state) and the
;; terminal-mode C-hjkl in ../term-enhance/{ghostel,vterm}.el (insert
;; state).

;; TTY-to-pane cache.  The daemon's TMUX_PANE is stale (set at daemon
;; startup, wrong for every emacsclient frame).  We look up the real
;; pane from the frame's TTY device instead.  The mapping is stable for
;; the lifetime of a pane (killing a pane kills its process/SIGHUP),
;; so we cache it indefinitely per TTY.  Cached nil results avoid
;; wasteful tmux subprocess calls for `emacs -nw' where the TTY is
;; the generic "/dev/tty" and won't match any real pane.
(defvar +compat/tmux--pane-cache (make-hash-table :test 'equal)
  "Cache mapping TTY path to tmux pane ID (or nil).
Cleared per-TTY when a new frame connects, to handle TTY reuse.")

;; Invalidate on new frame: a TTY freed by pane death may be reused
;; by a new pane with a different pane ID.
(defun +compat/tmux--invalidate-pane-cache (&optional frame)
  "Drop the cached pane ID for FRAME's TTY so the next lookup re-queries tmux."
  (when-let ((tty (frame-parameter (or frame (selected-frame)) 'tty)))
    (remhash tty +compat/tmux--pane-cache)))

(add-hook 'after-make-frame-functions #'+compat/tmux--invalidate-pane-cache)

;; Raw tmux query.  Uses -a to search ALL sessions (not just the
;; daemon's "current" session, which is wrong for emacsclient frames
;; attached to a different session).
(defun +compat/tmux--pane-for-tty (socket tty-name)
  "Return the tmux pane ID whose pane_tty matches TTY-NAME, or nil.
SOCKET is the tmux socket path from the TMUX env var."
  (let ((output (with-output-to-string
                  (call-process "tmux" nil standard-output nil
                                "-S" socket "list-panes" "-a"
                                "-F" "#{pane_id} #{pane_tty}"))))
    ;; Parse "PANE_ID TTY" lines, find the one matching our TTY.
    (cl-loop for line in (split-string output "\n" t)
             for parts = (split-string line " ")
             when (string= (cadr parts) tty-name)
             return (car parts))))

;; Cached wrapper.  Uses :unset sentinel so nil (no matching pane)
;; is also cached -- avoids a useless subprocess call on every
;; keypress for `emacs -nw' where the TTY never matches.
(defun +compat/tmux--cached-pane-for-tty (socket tty-name)
  "Return tmux pane ID for TTY-NAME, cached after first lookup."
  (let ((cached (gethash tty-name +compat/tmux--pane-cache :unset)))
    (if (eq cached :unset)
        ;; First lookup for this TTY -- query tmux and cache the result
        ;; (even if nil, so we don't re-query on every keypress).
        (let ((pane (+compat/tmux--pane-for-tty socket tty-name)))
          (puthash tty-name pane +compat/tmux--pane-cache)
          pane)
      cached)))

;; Pane resolution strategy:
;; 1. emacsclient: frame has a real TTY (e.g. /dev/ttys017) -- look it up.
;; 2. emacs -nw: frame TTY is generic (e.g. /dev/tty, /dev/pts/N on
;;    Linux) -- lookup fails, fall back to TMUX_PANE which is correct
;;    here because the process started fresh in the current pane.
(defun +compat/tmux--current-pane (socket)
  "Return the current tmux pane ID.
For emacsclient frames, derive from the frame's real TTY device.
For `emacs -nw' (where the TTY path doesn't match any tmux pane),
fall back to the TMUX_PANE environment variable."
  (let ((tty (frame-parameter nil 'tty)))
    ;; Try TTY lookup first (fast path for emacsclient).
    ;; Falls back to TMUX_PANE if TTY is nil or doesn't match a pane.
    (or (when tty (+compat/tmux--cached-pane-for-tty socket tty))
        (getenv "TMUX_PANE"))))

(defun +compat/tmux-select-pane (dir)
  "Ask the tmux server hosting this terminal to move to the pane in DIR
\(one of \"L\"/\"D\"/\"U\"/\"R\"), if any.  Return t on success, nil otherwise."
  ;; TMUX env is "SOCKET,PID,SESSION" -- extract socket path.
  ;; The socket path is stable across server restarts (tmux always
  ;; creates it at /tmp/tmux-UID/default), so this is safe even though
  ;; getenv reads the daemon's environment.  We don't use the PID or
  ;; session ID parts.
  (when-let* ((tmux-env (getenv "TMUX"))
              (socket (car (split-string tmux-env ",")))
              (pane (+compat/tmux--current-pane socket)))
    ;; call-process returns 0 on success; zerop converts to t, nil otherwise
    (zerop (call-process "tmux" nil nil nil "-S" socket "select-pane" "-t" pane (concat "-" dir)))))

(defun +compat/window-move-or-tmux (evil-window-fn tmux-dir)
  "Move to an adjacent Emacs window using EVIL-WINDOW-FN, or switch tmux
pane in TMUX-DIR (\"L\"/\"D\"/\"U\"/\"R\").  Emacs windows are tried
first; tmux is the fallback when no adjacent window exists.  Errors from
EVIL-WINDOW-FN are silently suppressed."
  (let ((cur-win (selected-window)))
    (ignore-errors (call-interactively evil-window-fn))
    (when (eq cur-win (selected-window))
      (+compat/tmux-select-pane tmux-dir))))
