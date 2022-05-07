;;; $DOOMDIR/modules/my-custom/vterm-enhance/config.el -*- lexical-binding: t; -*-

;; not sure what above really does or working properly
;; but leave it as is for now

;; configs that makes vterm module even more usable and enables tighter integration with my other tools

(setq tui-emacs (eq window-system nil))

(if tui-emacs (setenv "TUI_EMACS" "1"))
(setenv "INSIDE_DOOM_EMACS" "1")
(setenv "UNSET_ALL_MY_ZSH_STUFF_LOADED" "1")
(setenv "UNSET_MY_BASIC_ZSH_STUFF_LOADED" "1")

;; manage code related theme separately
(load! "theme") ;; ./theme.el

;; send ESC to `vterm` instead of `emacs` call `evil-collection-vterm-toggle-send-escape` to change that behavior
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

;; emulate `akinsho/toggleterm.nvim`
(map! :leader
      :g
      "'"
      #'+vterm/toggle)

;; reverse the the default keybinding for `t` and `T`
(map! :leader :prefix "o" :g "t" #'+vterm/here)
(map! :leader :prefix "o" :g "T" #'+vterm/toggle)

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
  (vterm-with-cmd
   (concat
    ;; this is a workaround that fixes the cursor is not changing shape properly between modes for neovim with TERM=eterm-color
    (if tui-emacs "" "TERM=xterm-256color ") ;; skip doing this for TUI becuase somehow it breaks the terminal graphics
    ;; this enables opening the same line in neovim as emacs
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

(after! vterm
  ;; let `vterm/toggle` consumable from shell side
  (add-to-list
   'vterm-eval-cmds
   '("vterm/toggle" (lambda () (+vterm/toggle nil)))))
