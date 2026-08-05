;;; $DOOMDIR/modules/my-custom/term-enhance/theme.el -*- lexical-binding: t; -*-

;; terminal-agnostic theme integration - the per-backend `switch-theme`
;; eval-cmd registrations (and vterm's TERM override) live in ./ghostel.el /
;; ./vterm.el since `ghostel-eval-cmds'/`vterm-eval-cmds' are backend-specific
;; variables, but everything below applies regardless of which is active.

;; The tone (light/dark) is the single contract of the bus. `theme-tone`
;; (bin/path/default/theme-tone) prints the bare tone, resolved from the
;; pair via `theme-pair` (the only interface to it - the pair files live
;; outside the repo and only theme-pair knows their paths). The pair can
;; hold any schemes - different families per tone, or dark-only names
;; like catppuccin-mocha without a -dark suffix - so nothing here parses or
;; guesses a tone from a scheme name. This Emacs maps the tone to its own
;; theme (doom-solarized-*).

;; the tone this Emacs instance currently has applied - the toggle flips
;; this instead of re-deriving the tone from a theme name
(defvar my-theme-tone "dark"
  "The tone (light/dark) this Emacs instance is currently on.")

(defun tone-to-theme (tone)
  "This Emacs's own theme for TONE."
  (if (string= tone "dark") 'doom-solarized-dark 'doom-solarized-light))

;; use this instead of using load-theme directly to sync theme between emacs and tinty
(defun switch-doom-theme (tone)
  (setq my-theme-tone tone)
  (load-theme (tone-to-theme tone) t)
  (setenv "DOOM_EMACS_THEME" tone))

;; toggle between light and dark
(defun toggle-doom-theme-tone ()
  ;; decide the target tone once - the instant doom switch and the
  ;; `theme-set` notification below both need it
  (let ((target (if (string= my-theme-tone "dark") "light" "dark")))
    ;; change the tone instantly in this instance
    (switch-doom-theme target)
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
  ;; `theme-tone` prints just light/dark - the bus's tone contract (resolved
  ;; via `theme-pair`); this Emacs's own theme for that tone is applied (the
  ;; file-notify watcher below runs this on every signal bump)
  (let ((tone (string-trim (shell-command-to-string "theme-tone"))))
    (when (member tone '("light" "dark"))
      (switch-doom-theme tone))))

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
  ;; decide the tone based on tinty's current scheme (by my `theme-tone` command)
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
