;;; my-custom/morevil/config.el -*- lexical-binding: t; -*-

;; if it's beyond muscle memory or keybinding related, there is =../../compat/neovim/=

;; equivalent to =:lua vim.wo.wrap = false= in Neovim
(global-visual-line-mode t)

;; doom's own default (modules/editor/evil/config.el) sets this to nil,
;; C-g is the only escape it binds out of the box
(setq evil-escape-key-sequence "jk")

;; "jk" is unwanted in terminal buffers: the shell already has its own
;; j/k muscle memory, and it breaks TUI apps like lf where j/k are
;; up/down - see ../term-enhance/integration.el for the same
;; vterm-mode/ghostel-mode exclusion pattern used elsewhere
(setq evil-escape-excluded-major-modes '(vterm-mode ghostel-mode))

;; enables cil, cal, vil, val, dil, dal, yil, yal, etc
(use-package! evil-textobj-line :after evil)

;; % jumps between matching language KEYWORDS too (if/end, function/end,
;; html tags, ...), not only brackets - the vim-matchup of
;; ../../../../../nvim/lua/plugins/editing.lua
(use-package! evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

;; to algin with my neovim keybindings
(map! :textobj "e" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj)
;; evil-markdown's own "ae" (markdown-element-textobj) shadows the global
;; e -> +evil:whole-buffer-txtobj above, but only in markdown buffers -
;; mode-local keymaps win over the global evil-outer-text-objects-map
;; lookup. Basic keybindings should behave the same regardless of mode,
;; so drop the "textobjects" key theme entirely (its only content is
;; that one binding, see evil-markdown.el's
;; evil-markdown--populate-textobjects-bindings) and let ae/ie fall
;; through to the global maps like everywhere else. Calling the setter
;; directly (not `setq') since `evil-markdown-key-theme's `:set'
;; function is what actually rebuilds the keymap - a plain setq wouldn't
;; trigger it
(after! evil-markdown
  (evil-markdown-set-key-theme
   (if (bound-and-true-p evil-disable-insert-state-bindings)
       '(navigation additional)
     '(navigation insert additional))))

;; close buffer and window - see ../../compat/neovim/smart-quit.el for
;; `close-window-or-buffer' itself and its evil-normal-state-map "q"
;; binding (follows my-smart-quit.lua's :q semantics, one entrypoint many
;; branches)

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

;; trim trailing whitespace on the buffer, same as nvim's SPC b t
;; (:%s/\s\+$//e in ../../../../../nvim/lua/plugins/keymaps.lua)
(map! :leader
      :prefix "b"
      :desc "Trim trailing whitespace"
      :g
      "t"
      #'delete-trailing-whitespace)

;; SPC t l toggles listchars (whitespace-mode, set up by
;; ../../compat/neovim/) like nvim's :set list! - doom's line-number
;; toggle moves from t l to the free, and frankly more mnemonic, t n
(map! :leader
      :prefix "t"
      :desc "Hidden chars (listchars)" :g "l" #'whitespace-mode
      :desc "Line numbers"             :g "n" #'doom/toggle-line-numbers)

;;;; to match with my muscle memory with ../../../../../nvim/lua/plugins/keymaps.lua
(map! :leader :prefix "f" :g "h" #'doom/help-search)
;; browse-url-xdg-open shells out to `xdg-open' unconditionally - Linux
;; only, breaks on macOS (no such binary). plain `browse-url' dispatches
;; via browse-url-browser-function, which auto-detects system-type (macOS:
;; `open', Linux: xdg-open, ...) - cross-platform for free
(map! :n "gx" #'browse-url)
;; jumplist navigation like nvim's C-O/C-I (bound go/gn there too, see
;; ../../../../../nvim/lua/plugins/keymaps.lua). better-jumper piggybacks
;; evil's jump list, so plain motions (gg, G, /search) are tracked - doom
;; was already remapping the previous xref-go-back binding here onto
;; better-jumper-jump-backward; bind both directions explicitly instead
(map! :n "go" #'better-jumper-jump-backward)
(map! :n "gn" #'better-jumper-jump-forward)
;; ,/;/:/gh/gk/leader bindings below follow the "anti-shift keybindings"
;; theme in ../../../../../docs/philosophy.md - that's the source of truth
;; for the cross-tool mapping/rationale, comments here are emacs-local
;; detail only
;;
;; repeats the last ex command (shadows evil's reverse f/t repeat, ; still
;; repeats forward)
(map! :n "," #'evil-ex-repeat)

;; enters the ex command-line without needing shift - plain evil-ex
;; already gives completion-at-point suggestions as you type (native
;; command names/file paths), plus M-p/M-n for `evil-ex-history'; no need
;; for a completing-read/evil-ex-execute detour
(map! :n ";" #'evil-ex)
;; on-demand searchable history
(map! :map evil-ex-completion-map "M-r" #'consult-history)
;; the ex prompt is a literal ":" hardcoded in evil-ex.el (not a
;; variable), so advise the primitive it's passed to rather than evil
;; itself - scoped to an exact ":" prompt, which nothing else in this
;; config uses, to avoid relabeling unrelated minibuffers
(advice-add #'read-from-minibuffer :filter-args
            (lambda (args)
              (if (equal (car args) ":")
                  (cons ";" (cdr args))
                args)))

;; swap SPC ; and SPC : (doom's stock ";" = pp-eval-expression, ":" = M-x) -
;; pp-eval-expression moves into the freed SPC : slot rather than being
;; dropped
(map! :leader
      :desc "M-x" ";" #'execute-extended-command
      :desc "Eval expression" ":" #'pp-eval-expression)
;; searchable box over documented symbols (helpful's completing-read
;; prompt, defaults to symbol at point but editable/searchable via
;; vertico) - gh already covers "describe the symbol at point" below, so
;; this is for browsing/searching by name instead
(map! :n ":" #'helpful-symbol)

;; frees K, which duplicated gh's job here (+lookup/documentation IS
;; doom's own multi-source aggregator). Both keys are free globally -
;; dired's own "gh" (dirvish-subtree-up) and magit's git-rebase-mode's
;; "gk" are mode-local overrides that still win in their own buffers
(defun +eldoc-help-at-point ()
  "Pure eldoc/LSP hover.
`eldoc-box-help-at-point' only re-displays whatever eldoc last computed
- force a fresh lookup first, or a first-ever call in the session has
nothing to show yet. Its floating box is a child frame, which needs a
graphical display (../../tools/lsp-support/config.el guards the
equivalent automatic hover-mode the same way) - fall back to the plain
doc buffer in a terminal frame (e.g. ghostel/vterm)."
  (interactive)
  (eldoc-print-current-symbol-info)
  (if (display-graphic-p)
      (eldoc-box-help-at-point)
    (eldoc-doc-buffer t)))
(map! :n "gh" #'+lookup/documentation
      :n "gk" #'+eldoc-help-at-point
      :n "K" nil)

;; magit's base keymap binds h -> magit-dispatch and l -> magit-log,
;; clobbering the h/l = expand/collapse section muscle memory from
;; neogit (../../../../../nvim/lua/plugins/config/git.lua: both mapped to
;; 'Toggle') and tig (../../../vim.tigrc: h = close/back, l = enter/open -
;; same collapse/expand spirit, different verbs). Neither loses real
;; functionality: `?' still opens the dispatch popup (same command, h was
;; a plain duplicate), and Log is still one more keystroke away (`L' for
;; magit-log-refresh, or M-x magit-log)
(map! :map magit-mode-map
      :n "h" #'magit-section-toggle
      :n "l" #'magit-section-toggle)
;; the dispatch popup (bound to `?', see above) has ITS OWN separate key
;; table (magit-dispatch's transient suffixes) that still hardcodes h ->
;; "Help"/magit-info and l -> "Log"/magit-log - stale now that the outer
;; h/l mean something else. h's entry is a pure duplicate already (magit-info
;; is also on "C-x i" in the same popup) - free to drop. l's entry has no
;; duplicate in the popup, so this does cost the one-keystroke path to
;; plain magit-log from inside `?' (still reachable via M-x or `L')
(after! magit
  (transient-remove-suffix 'magit-dispatch "h")
  (transient-remove-suffix 'magit-dispatch "l"))

;; + / - increment/decrement the number at point, as remapped in nvim
;; (keymaps.lua: + = C-A, - = C-X, since the defaults never got used)
(map! :n "+" #'evil-numbers/inc-at-pt
      :n "-" #'evil-numbers/dec-at-pt)

;; help buffers scroll like less, as nvim arranges in boot/misc.lua
;; (d/u/f/b -> C-d/C-u/C-f/C-b there); help is read-only, so the shadowed
;; evil-delete/undo/snipe-f/backward-word lose nothing of value
(map! :map help-mode-map
      :n "d" #'evil-scroll-down
      :n "u" #'evil-scroll-up
      :n "f" #'evil-scroll-page-down
      :n "b" #'evil-scroll-page-up)

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
