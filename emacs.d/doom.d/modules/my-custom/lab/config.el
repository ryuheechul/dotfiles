;;; $DOOMDIR/modules/my-custom/lab/config.el -*- lexical-binding: t; -*-

;; configs that where I can freely experiment without concerning about which specific module to manage the code yet

;; match key bindings similar to my neovim's

;; close buffer and window
(defun close-buffer-or-doom ()
  (if (string= (buffer-name) "*doom*")
      (evil-quit)
    (kill-buffer)))

(defun close-window-or-buffer ()
  (interactive)
  (if (> (count-windows) 1)
      (+workspace/close-window-or-workspace)
    (close-buffer-or-doom)))

(define-key evil-normal-state-map (kbd "q") 'close-window-or-buffer)
;; alternatively the above could be as simple as below
;; (define-key evil-normal-state-map (kbd "q") 'evil-quit)

;; switching buffer and windows
(define-key evil-normal-state-map (kbd "<tab>") 'other-window)

(map! :leader
      :g
      "<tab>"
      'switch-to-next-buffer)

;; split window
(map! :leader
      :prefix "w"
      :g
      "/"
      (lookup-key doom-leader-map (kbd "w v")))

(map! :leader
      :prefix "w"
      :g
      "-"
      (lookup-key doom-leader-map (kbd "w s")))

;; file tree
(setq treemacs-position 'right)

(map! :leader
      :prefix "f"
      :g
      "t"
      'treemacs)
