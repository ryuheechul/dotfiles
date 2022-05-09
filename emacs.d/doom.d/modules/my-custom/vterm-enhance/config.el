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

;; basically bring toggle to `+vterm/here'
(defun vterm/full-w/toggle ()
  (interactive)
  ;; handle when vterm is not loaded yet too
  (if (boundp 'vterm-buffer-name)
      (let ((target-buffer (get-buffer vterm-buffer-name)))
        (if (eq target-buffer nil)
            (+vterm/here nil)
          (if (doom-buried-buffer-p target-buffer)
              (switch-to-buffer target-buffer)
            ;; since bury-buffer doesn't do much when there is no other real buffer
            ;; better to kill it instead
            (if (> (length (doom-real-buffer-list)) 1)
                ;; somehow `(bury-buffer target-buffer)` doesn't work
                (bury-buffer)
              (kill-this-buffer)))))
    ;; when 'vterm-buffer-name is not bound
    (+vterm/here nil)))

;; reverse the the default keybinding for `t' and `T'
(map! :leader :prefix "o" :g "t" #'vterm/full-w/toggle)
;; TODO: `T' can be better used for running Tmux probably
(map! :leader :prefix "o" :g "T" #'+vterm/toggle)

(after! vterm
  (evil-collection-define-key 'insert 'vterm-mode-map
    ;; to let `christoomey/vim-tmux-navigator` to work properly (at least on GUI Emacs)
    ;; k and l are already taken care of by
    ;; https://github.com/emacs-evil/evil-collection/blob/ca4c6172240321a06498390d7d6fa790033f7fc1/modes/vterm/evil-collection-vterm.el#L227-L228
    (kbd "C-h") #'vterm--self-insert
    (kbd "C-j") #'vterm--self-insert))

;; a hack to let zsh to run command on start up - conjunction with ../../../shell/source.zsh
;; currently the assumption of this function is that run it in full window
;; if to avoid opening in the full window - go with `(+vterm/toggle t)` instead
(defun vterm-with-cmd (cmd)
  ;; this will be read by ../../../shell/source.zsh
  (setenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" cmd)
  ;; to avoid buffer collision with regular ones
  (setq vterm-buffer-name "*vterm-with-cmd*")
  (+vterm/here nil)
  ;; fix the global name back to normal - assuming the original name is `*vterm*`
  (setq vterm-buffer-name "*vterm*")
  ;; clean up this because it should be set only momentarily
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

;; this is more of an illusion of hiding than actual hiding (if there is such a thing)
;; so it's probably not perfect but quite usable to handle both `+vterm/here` and `+vterm/toggle`
(defun vterm/hide ()
  (unless (not (eq major-mode 'vterm-mode))
    (if (= (count-windows) 1)
        ;; assuming it is a full window one
        (vterm/full-w/toggle)
      ;; assuming it is a popup vterm
      (+vterm/toggle nil))))

(after! vterm
  ;; let `vterm/hide` consumable from shell side
  (add-to-list
   'vterm-eval-cmds
   '("vterm/hide" vterm/hide)))

;; these "undo" hiding modeline that is defined from ~/.doom-emacs.d/modules/term/vterm/autoload.el
(after! hide-mode-line
  (add-to-list
   'hide-mode-line-excluded-modes
   'vterm-mode))

(defun vterm/full-w/turn-off-hide-mode-line ()
  (if (string= (buffer-name) vterm-buffer-name)
      (turn-off-hide-mode-line-mode)))

(after! vterm
  ;; let `vterm/hide` consumable from shell side
  (add-to-list
   'vterm-eval-cmds
   '("vterm/unhide-mode-line" vterm/full-w/turn-off-hide-mode-line)))
