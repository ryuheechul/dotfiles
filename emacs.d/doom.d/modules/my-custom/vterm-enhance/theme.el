;;; $DOOMDIR/modules/my-custom/vterm-enhance/theme.el -*- lexical-binding: t; -*-

;; not sure what above really does or working properly
;; but leave it as is for now

;; configs that makes vterm module even more usable in terms of aesthetics

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

;; use this instead of using load-theme directly to sync theme between emacs and base16-shell
(defun switch-doom-theme (theme-name)
  (apply-theme (intern theme-name))
  (setenv
   "DOOM_EMACS_THEME"
   (symbol-name (doom-theme-value))))

;; toggle between light and dark
(defun toggle-doom-theme-tone ()
  (if (eq (doom-theme-value) 'base16-solarized-dark)
      (switch-doom-theme (symbol-name 'base16-solarized-light))
    (switch-doom-theme (symbol-name 'base16-solarized-dark))))

;; to match with <Space> t b from my neovim
(map! :leader
      :prefix "t"
      :desc "Toggle Theme Tone"
      :g
      "b"
      (lambda () (interactive) (toggle-doom-theme-tone)))

(defun follow-theme-base16-shell ()
  (switch-doom-theme (concat "base16-" (shell-command-to-string "current-base16"))))

;; use base16-theme package to enable base16 theme on emacs
(use-package! base16-theme
  ;; ;; this makes lazy loading possible
  ;; ;; but the theme become not available right away
  ;; ;; so disable it for now
  ;; :after-call vterm-mode-hook
  :config
  ;; this works the best with me when it runs with `-nw`
  ;; also if there is any issue with truecolor
  ;; `export KONSOLE_DBUS_SESSION=1` might help - https://hoppsjots.org/emacs-24bit.html
  (setq base16-theme-256-color-source 'colors)
  ;; and with GUI version, somehow it looks different depends on
  ;; which terminal that I use to run emacs - this was actually mitigated
  ;; by setting COLORTERM=truecolor
  ;; decide the tone based on ~/.base16_theme (by my `current-base16` command)
  (follow-theme-base16-shell))

(setenv "INSIDE_DOOM_EMACS" "1")
(setenv "FORCE_LOAD_MY_ZSH_STUFF" "1")

(after! vterm
  ;; setting TERM=eterm-256color worked best with `lf`
  ;; although some lines might look not properly aligned
  (setq vterm-term-environment-variable "eterm-color")
  ;; let `switch-theme` consumable from shell side
  (add-to-list
   'vterm-eval-cmds
   '("switch-theme" (lambda (theme) (switch-doom-theme theme)))))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Notifications.html
(require 'filenotify)

(file-notify-add-watch
  ;; ;; this `file-notify-add-watch' actually can handle change on the symlink directory like below
  ;; "~/.base16_theme" '(change) (lambda (event) (follow-theme-base16-shell))
  ;; ;; however on my another watcher, ~/.config/dfs-rhc/bin/local/base16-shell-auto-reload-on-tmux can't
  ;; ;; since it relys on entr and it doesn't support that - https://github.com/eradman/entr/issues/30
  ;; ;; therefore we just watch the same file as other watchers to maintain the same logic across watchers
  "~/.base16_theme.updated-time" '(change) (lambda (event) (follow-theme-base16-shell)))
