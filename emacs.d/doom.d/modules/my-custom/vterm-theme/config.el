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

;; send ESC to `vterm` instead of `emacs` call `evil-collection-vterm-toggle-send-escape` to change that behavior
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

;; emulate `akinsho/toggleterm.nvim`
(map! :leader
      :g
      "'"
      #'+vterm/toggle)

(after! vterm
  (evil-collection-define-key 'insert 'vterm-mode-map
    ;; to let `christoomey/vim-tmux-navigator` to work properly (at least on GUI Emacs)
    ;; k and l are already taken care of by
    ;; https://github.com/emacs-evil/evil-collection/blob/ca4c6172240321a06498390d7d6fa790033f7fc1/modes/vterm/evil-collection-vterm.el#L227-L228
    (kbd "C-h") #'vterm--self-insert
    (kbd "C-j") #'vterm--self-insert))

;; a hack to let zsh to run command on start up - conjunction with ../../../shell/source.zsh
(defun vterm-with-cmd (cmd)
  (setenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" cmd)
  ;; if to avoid opening in the full window - go with `(+vterm/toggle t)` instead
  (+vterm/here nil)
  (setenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" nil))

;; an option to fallback to neovim
(defun open-in-neovim ()
  (interactive)
  (vterm-with-cmd (concat
                   "my_nvim_forget_line_number=1 "
                   "nvim +"
                   (number-to-string (line-number-at-pos))
                   " -- "
                   buffer-file-name)))

;; map the function above for convenience
(map! :leader
      :prefix "f"
      :n
      "n"
      #'open-in-neovim)
