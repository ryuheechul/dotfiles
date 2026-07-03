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

;; emulate `dynamotn/Navigator.nvim' (used in
;; ../../../../../nvim/lua/plugins/system.lua in place of the older
;; `christoomey/vim-tmux-navigator', which is commented out there), whose
;; actual algorithm (Navigator.nvim/lua/Navigator/navigate.lua)
;; is: record the current window, try moving within the editor's own
;; splits, and if the window is unchanged afterward (i.e. hit the edge),
;; hand off to the terminal multiplexer instead - here, that means shelling
;; out to `tmux select-pane', mirroring Navigator.nvim's tmux backend
;; (Navigator.nvim/lua/Navigator/mux/tmux.lua) exactly: parse the socket
;; out of $TMUX and target the pane explicitly via $TMUX_PANE, rather than
;; relying on ambient tmux-client context (matters when more than one tmux
;; server is running). a no-op outside tmux ($TMUX unset).
;;
;; `evil-window-left' et al. signal a `user-error' ("No window left from
;; selected window") at the edge instead of silently doing nothing -
;; `ignore-errors' swallows that before checking whether the window moved
(defun my/tmux-select-pane (dir)
  "Ask the tmux server hosting this terminal to move to the pane in DIR
\(one of \"L\"/\"D\"/\"U\"/\"R\"), if any."
  (when-let* ((tmux-env (getenv "TMUX"))
              (socket (car (split-string tmux-env ",")))
              (pane (getenv "TMUX_PANE")))
    (call-process "tmux" nil nil nil "-S" socket "select-pane" "-t" pane (concat "-" dir))))

(defun my/window-move-or-tmux (evil-window-fn tmux-dir)
  (let ((cur-win (selected-window)))
    (ignore-errors (call-interactively evil-window-fn))
    (when (eq cur-win (selected-window))
      (my/tmux-select-pane tmux-dir))))

(define-key evil-normal-state-map (kbd "C-h") (lambda () (interactive) (my/window-move-or-tmux #'evil-window-left "L")))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (my/window-move-or-tmux #'evil-window-down "D")))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (my/window-move-or-tmux #'evil-window-up "U")))
(define-key evil-normal-state-map (kbd "C-l") (lambda () (interactive) (my/window-move-or-tmux #'evil-window-right "R")))
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
(map! :n "\\s" #'term-enhance/full-w-toggle)

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
