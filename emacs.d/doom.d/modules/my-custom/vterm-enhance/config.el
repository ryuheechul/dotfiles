;;; $DOOMDIR/modules/my-custom/vterm-enhance/config.el -*- lexical-binding: t; -*-

;; not sure what above really does or working properly
;; but leave it as is for now

;; configs that makes vterm module even more usable and enables tighter integration with my other tools

(setq tui-emacs (eq window-system nil))

;; allow yank but not paste as libvterm does not support
(setq
 vterm-enable-manipulate-selection-data-by-osc52 t)

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


;; this is a function to let to prepare my (zsh) shell work well within emacs
;; needed to be called before vterm launches
;; to cover all possible scenario it's currently being called in multiple places (in this file)
(defun prep-env-for-term ()
  ;; techincally not envvar but to clean up the mess made with tramp `vterm-shell'
  (setq vterm-shell shell-file-name)
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

;; this might pick up user installed (possibly newer version of zsh)
;; (let ((shell-path (shell-command-to-string "which zsh")))
;;   (setenv "SHELL" shell-path)
;;   (setq vterm-shell shell-path))

;; manage code related theme separately
(load! "theme") ;; ./theme.el

;; send ESC to `vterm` instead of `emacs` call `evil-collection-vterm-toggle-send-escape` to change that behavior
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

;; emulate `akinsho/toggleterm.nvim`
(map! :leader
      :g
      "'"
      #'wrapped/vterm/toggle)

(defun wrapped/vterm/toggle ()
  (interactive)
  (prep-env-for-term)
  (+vterm/toggle nil))

;; basically bring toggle to `+vterm/here'
(defun vterm/full-w/toggle ()
  (interactive)
  (prep-env-for-term)
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
(map! :leader :prefix "o" :g "T" #'wrapped/vterm/toggle)

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
  (settermenv  "INSIDE_EMACS_RUN_CMD_ON_START_UP" cmd)
  ;; to avoid buffer collision with regular ones
  (setq vterm-buffer-name "*vterm-with-cmd*")
  (+vterm/here nil)
  ;; fix the global name back to normal - assuming the original name is `*vterm*`
  (setq vterm-buffer-name "*vterm*")
  ;; clean up this because it should be set only momentarily
  (settermenv  "INSIDE_EMACS_RUN_CMD_ON_START_UP" nil))

;; an option to fallback to neovim
(defun open-in-neovim ()
  (interactive)
  (defun prep-env-to-quickly-open-with-neovim ()
    (settermenv "ALL_MY_ZSH_STUFF_LOADED" "1")
    (settermenv "fast_shell_in_editor" "1")
    (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" "1"))
  (defun undo-env-to-quickly-open-with-neovim ()
    (settermenv "ALL_MY_ZSH_STUFF_LOADED" nil)
    (settermenv "fast_shell_in_editor" nil)
    (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" nil))
  (let ((file-name (if (tramp-tramp-file-p buffer-file-name)
                       (tramp-file-local-name buffer-file-name)
                     buffer-file-name))
        (line-number (number-to-string (line-number-at-pos))))
    (prep-env-to-quickly-open-with-neovim)
    (vterm-with-cmd
     (concat
      ;; this is a workaround that fixes the cursor is not changing shape properly between modes for neovim with TERM=eterm-color
      "TERM=xterm-256color "
      ;; this enables opening the same line in neovim as emacs
      "my_nvim_forget_line_number=1 "
      "nvim +"
      line-number
      " -- "
      file-name))
    ;; canceling potential pollutions
    (undo-env-to-quickly-open-with-neovim)))

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
