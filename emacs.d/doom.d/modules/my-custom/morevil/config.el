;;; my-custom/morevil/config.el -*- lexical-binding: t; -*-

;; if it's beyond muscle memory or keybinding related, there is =../../compat/neovim/=

;; equivalent to =:lua vim.wo.wrap = false= in Neovim
(global-visual-line-mode t)

;; doom's own default (modules/editor/evil/config.el) sets this to nil,
;; C-g is the only escape it binds out of the box
(setq evil-escape-key-sequence "jk")

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

;; switching buffer and windows - `<tab>' (the symbol) only ever fires in a
;; GUI frame; a plain terminal has no way to tell the physical Tab key
;; apart from literal `TAB'/C-i (no extended keyboard protocol in play,
;; same reason C-h collides with Backspace - see
;; ../term-enhance/vterm.el's comment on that), so a TTY always sends
;; "TAB" instead - bind both so this works the same in both contexts
(define-key evil-normal-state-map (kbd "<tab>") #'other-window)
(define-key evil-normal-state-map (kbd "TAB") #'other-window)

;; same GUI-vs-TTY key-symbol split as above; also fully shadows doom's own
;; default "workspace" prefix-map on this key (both `<tab>' and `TAB', see
;; modules/config/default/+evil-bindings.el) - moved to `SPC u' below since
;; workspaces aren't used here, so `SPC TAB' can just mean "next buffer"
(map! :leader
      :g "<tab>" #'switch-to-next-buffer
      :g "TAB"   #'switch-to-next-buffer)

;; which-key ties `:desc' to the key sequence, not the command (same
;; surprise as ../term-enhance/integration.el's comment on this) - without
;; this, `SPC TAB' would still show doom's original "workspace" label here
;; even though it now switches buffers
(which-key-add-key-based-replacements "SPC TAB" "Next buffer")

;; doom's `:prefix-map' names the underlying keymap after its DESCRIPTION,
;; not its key (modules/doom/compat/+keybinds.el: `("doom-leader-%s-map"
;; desc)') - so the workspace keymap doom built for `SPC TAB' already
;; exists as `doom-leader-workspace-map', sub-bindings and all. no need to
;; redeclare any of that; just point a free top-level leader letter at the
;; same keymap. `u' turned out to already be doom's own "Universal
;; argument" (+evil-bindings.el - a plain single-key binding, not a
;; prefix-map, easy to miss when only grepping for `:prefix-map'); `k' is
;; confirmed free across every doom module and this repo's own, by both
;; :prefix-map and plain single-key leader bindings
(map! :leader
      (:when (modulep! :ui workspaces)
       :desc "workspace" "k" doom-leader-workspace-map))

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
;; jumplist navigation like nvim's C-O/C-I (bound go/gn there too, see
;; ../../../../../nvim/lua/plugins/keymaps.lua). better-jumper piggybacks
;; evil's jump list, so plain motions (gg, G, /search) are tracked - doom
;; was already remapping the previous xref-go-back binding here onto
;; better-jumper-jump-backward; bind both directions explicitly instead
(map! :n "go" #'better-jumper-jump-backward)
(map! :n "gn" #'better-jumper-jump-forward)
;; , repeats the last ex command like nvim's @: (same trade as there: it
;; shadows evil's reverse f/t repeat, ; still repeats forward)
(map! :n "," #'evil-ex-repeat)

;; gl/gr complete the lookup muscle memory from
;; ../../../../../nvim/lua/plugins/config/lsp.lua (gd was already doom's
;; +lookup/definition; it only LOOKED broken in lua buffers - see
;; ../../ext-lang/lua/config.el): gl = follow the "link" like nvim's C-]
;; tag jump (first match, no picker - see +neovim/goto-link in
;; ../../compat/neovim/config.el), gr = references. shadows accepted
;; knowingly, mirroring nvim's own trades: gl was evil-lion's left-align
;; operator (gL right-align survives) and gr was doom's +eval:region
;; operator (gR still evals the buffer, and the SPC-level eval bindings
;; remain)
;; :nm - motion state too, so gl also works in doc-ish buffers (help,
;; Info, ...) that evil keeps out of normal state; that is where the
;; original C-] mattered most in vim
(map! :nm "gl" #'+neovim/goto-link)
(map! :n "gr" #'+lookup/references)
;; \s mimics neovim's :suspend (leader s in
;; ../../../../../nvim/lua/plugins/keymaps.lua): in a TTY frame the real
;; thing exists - `suspend-frame' sends SIGTSTP like vim's :suspend, back
;; with fg (also right for emacsclient -nw: it suspends just that client).
;; a GUI frame has no parent shell to drop into (launchd started it;
;; suspend-frame would merely iconify), so keep the illusion there: toggle
;; the full-window terminal instead - whose shell aliases fg to toggle
;; back (../../../shell/source.zsh), completing the
;; suspend/fg round trip. checked per keypress, so daemon frames of both
;; kinds each get the right behavior
(map! :desc "suspend to shell (GUI: toggle terminal)"
      :n "\\s" (cmd! (if (display-graphic-p)
                         (term-enhance/full-w-toggle)
                       (suspend-frame))))

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
