;;; my-custom/morevil/config.el -*- lexical-binding: t; -*-

;; if it's beyond muscle memory or keybinding related, there is =../../compat/neovim/=

;; equivalent to =:lua vim.wo.wrap = false= in Neovim
(global-visual-line-mode t)

;; enables cil, cal, vil, val, dil, dal, yil, yal, etc
(use-package! evil-textobj-line :after evil)

;; to algin with my neovim keybindings
(map! :textobj "e" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj)

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

(define-key evil-normal-state-map (kbd "q") #'close-window-or-buffer)
;; alternatively the above could be as simple as below
;; (define-key evil-normal-state-map (kbd "q") #'evil-quit)

;; switching buffer and windows
(define-key evil-normal-state-map (kbd "<tab>") #'other-window)

(map! :leader
      :g
      "<tab>"
      'switch-to-next-buffer)

;; emulate `christoomey/vim-tmux-navigator`
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
;; howver these bindings above doesn't seem to work in TUI;
;; not that it's not bound but the keys themselves seem to be not recognized;
;; maybe related to this? https://www.reddit.com/r/emacs/comments/17afhxu/getting_all_keybindings_working_in_the_tui/
;; there is a `C-w' (or `SPC' `w') prefix to workaround

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
      #'treemacs)

(map! :leader
      :prefix "t"
      :g
      "f"
      #'treemacs)

;; clear highlight by search (/)
(map! :leader
      :prefix "s"
      :g
      "c"
      #'evil-ex-nohighlight)

;;;; to match with my muscle memory with ../../../../../nvim/lua/plugins/keymaps.lua
(map! :leader :prefix "f" :g "h" #'doom/help-search)
(map! :n "gx" #'browse-url-xdg-open)
(map! :n "go" #'xref-go-back)
;; to give the illusion of putting the editor in the background (in case as if emacs was terminal)
(map! :n "\\s" #'vterm/full-w/toggle)

(map! (:after info :map Info-mode-map
       :leader :prefix "m" :n "ee"     #'eval-last-sexp)
      (:after info :map Info-mode-map
       ;; even shorter way to eval in info mode
       :n "ge"      #'eval-last-sexp
       ;; swapping the defaults
       :n "gt"      #'Info-toc
       :n "gT"      #'Info-top-node
       ;; don't forget the history!
       :n "gh"      #'Info-history
       ;; easy scrolling
       ;; :n "u"       #'Info-scroll-down
       ;; :n "d"       #'Info-scroll-up
       :n "u"       #'evil-scroll-up
       :n "d"       #'evil-scroll-down
       :n "b"       #'Info-scroll-down
       :n "f"       #'Info-scroll-up))
