;;; compat/neovim/integration.el -*- lexical-binding: t; -*-

;; Tmux pane navigation - neovim-style C-hjkl pane switching via
;; christoomey/vim-tmux-navigator semantics.  Used by both the intercept
;; map in ../../my-custom/morevil/config.el (normal state) and the
;; terminal-mode C-hjkl in ../term-enhance/{ghostel,vterm}.el (insert
;; state).

(defun +compat/tmux-select-pane (dir)
  "Ask the tmux server hosting this terminal to move to the pane in DIR
\(one of \"L\"/\"D\"/\"U\"/\"R\"), if any.  Return t on success, nil otherwise."
  (when-let* ((tmux-env (getenv "TMUX"))
              (socket (car (split-string tmux-env ",")))
              (pane (getenv "TMUX_PANE")))
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
