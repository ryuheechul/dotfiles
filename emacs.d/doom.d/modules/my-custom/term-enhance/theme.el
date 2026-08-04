;;; $DOOMDIR/modules/my-custom/term-enhance/theme.el -*- lexical-binding: t; -*-

;; terminal-agnostic theme integration - the per-backend `switch-theme`
;; eval-cmd registrations (and vterm's TERM override) live in ./ghostel.el /
;; ./vterm.el since `ghostel-eval-cmds'/`vterm-eval-cmds' are backend-specific
;; variables, but everything below applies regardless of which is active.

;; translate between base16-* and doom-*
(defun base16-to-doom (theme)
  (if (eq theme 'base16-solarized-dark)
      'doom-solarized-dark
    'doom-solarized-light))

(defun doom-to-base16 (theme)
  (if (eq theme 'doom-solarized-dark)
      'base16-solarized-dark
    'base16-solarized-light))

;; a handle to inject doom-* instead of base16-* optionally
(setq override-base16-with-doom t)

(defun doom-theme-value ()
  (let ((theme-val (symbol-value 'doom-theme)))
    (if override-base16-with-doom
        (doom-to-base16 theme-val)
      theme-val)))

(defun apply-theme (theme-name)
  (load-theme
   (if override-base16-with-doom (base16-to-doom theme-name) theme-name)
   t))

;; use this instead of using load-theme directly to sync theme between emacs and tinty
(defun switch-doom-theme (theme-name)
  (apply-theme (intern theme-name))
  (setenv
   "DOOM_EMACS_THEME"
   (symbol-name (doom-theme-value))))

;; toggle between light and dark
(defun toggle-doom-theme-tone ()
  ;; decide the target tone once - the instant doom switch and the
  ;; `theme-set` notification below both need it
  (let ((target (if (eq (doom-theme-value) 'base16-solarized-dark)
                    "light" "dark")))
    ;; change the tone instantly in this instance
    (switch-doom-theme (concat "base16-solarized-" target))
    ;; ...and via `theme-set` (bin/path/default/theme-set) so tinty's hooks
    ;; notify every subscriber - all running nvim/emacs instances, tmux, herdr;
    ;; this instance's file-notify watcher (below) re-applies the theme when
    ;; the signal bumps (re-applying the same theme is a no-op, so no loop)
    (call-process "theme-set" nil nil nil target)))

;; to match with <Space> t b from my neovim
(map! :leader
      :prefix "t"
      :desc "Toggle Theme Tone"
      :g
      "b"
      (lambda () (interactive) (toggle-doom-theme-tone)))

(defun follow-theme-tinty ()
  (switch-doom-theme (concat "base16-" (shell-command-to-string "theme-name"))))

;; use base16-theme package to enable base16 theme on emacs
(use-package! base16-theme
  ;; ;; this makes lazy loading possible
  ;; ;; but the theme become not available right away
  ;; ;; so disable it for now
  ;; :after-call ghostel-mode-hook
  :config
  ;; this works the best with me when it runs with `-nw`
  ;; also if there is any issue with truecolor
  ;; `export KONSOLE_DBUS_SESSION=1` might help - https://hoppsjots.org/emacs-24bit.html
  (setq base16-theme-256-color-source 'colors)
  ;; and with GUI version, somehow it looks different depends on
  ;; which terminal that I use to run emacs - this was actually mitigated
  ;; by setting COLORTERM=truecolor
  ;; decide the tone based on tinty's current scheme (by my `theme-name` command)
  (follow-theme-tinty))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Notifications.html
(require 'filenotify)

;; subscriber of "One tone, every layer" - ../../../../../docs/mechanics.md
(file-notify-add-watch
  ;; ;; `file-notify-add-watch' can handle change on a symlink target, so watching
  ;; ;; the stable `~/.active-theme` state link directly would work for emacs - but
  ;; ;; the other watchers (nvim's fwatch) all key off the signal file, and the old
  ;; ;; entr-based watcher couldn't follow a repointed symlink
  ;; ;; (https://github.com/eradman/entr/issues/30) - so we watch the same
  ;; ;; `~/.active-theme.updated-time` file as everyone else to maintain the same
  ;; ;; logic across watchers
  "~/.active-theme.updated-time" '(change) (lambda (event) (follow-theme-tinty)))
