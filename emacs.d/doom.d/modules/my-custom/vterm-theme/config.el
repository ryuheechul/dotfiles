;;; $DOOMDIR/modules/my-custom/vterm-theme/config.el -*- lexical-binding: t; -*-

;; not sure what above really does or working properly
;; but leave it as is for now

;; configs that makes vterm module even more usable and enables tighter integration with my other tools

;; use this instead of using load-theme directly to sync theme between emacs and base16-shell
(defun switch-doom-theme (theme-name)
  (load-theme (intern theme-name) t)
  (setenv
   "DOOM_EMACS_THEME"
   (symbol-name (symbol-value 'doom-theme))))

;; toggle between light and dark
(defun toggle-doom-theme-tone ()
  (if (eq (symbol-value 'doom-theme) 'base16-solarized-dark)
      (switch-doom-theme (symbol-name 'base16-solarized-light))
    (switch-doom-theme (symbol-name 'base16-solarized-dark))))

;; to match with <Space> t b from my neovim
(map! :leader
      :prefix "t"
      :desc "Toggle Theme Tone"
      :g
      "b"
      (lambda () (interactive) (toggle-doom-theme-tone)))

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
  (switch-doom-theme (concat "base16-" (shell-command-to-string "current-base16"))))

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
