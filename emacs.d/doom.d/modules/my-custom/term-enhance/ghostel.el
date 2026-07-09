;;; $DOOMDIR/modules/my-custom/term-enhance/ghostel.el -*- lexical-binding: t; -*-

;; everything specific to the ghostel terminal backend. shared plumbing
;; (`settermenv', `prep-env-for-term', ...) lives in ./config.el; whether
;; any of this file's functions actually gets bound to a key is decided in
;; ./integration.el, not here.

;; declare as special before ghostel is loaded so let-binding it below stays
;; dynamic (otherwise ghostel's own defcustom errors with "Defining as dynamic
;; an already lexical var" when autoloaded inside that let)
(defvar ghostel-buffer-name)

;; let terminal apps set the clipboard via OSC 52
;; (parity with vterm-enable-manipulate-selection-data-by-osc52; note ghostel's
;; bundled terminfo doesn't advertise `Ms', so nvim/tmux won't auto-detect it)
(setq ghostel-enable-osc52 t)

;; macOS remotes have no `getent`, so ghostel-tramp-shells' login-shell
;; auto-detection (getent passwd) always fails there - fall back to a bare
;; "zsh" (no path) rather than vterm's old trick of copying the *local*
;; machine's shell path (see ./known-issues.org item 2). ghostel execs the
;; fallback via `/bin/sh -c "... exec zsh ..."` on the remote, so a bare name
;; is a $PATH lookup on that remote host - unlike a hardcoded path (e.g.
;; ~/.nix-profile/bin/zsh), this isn't tied to one user/host layout. Caveat:
;; only finds zsh if it's on $PATH for that non-interactive shell (governed
;; by `tramp-remote-path', not .zshrc/.profile).
(setq ghostel-tramp-shells '(("ssh" login-shell "zsh")))

;; NOTE: zellij deadlocks the native PTY reader (upstream ghostel bug) - it is
;; masked shell-side via bin/path/emacs/zellij instead of giving up
;; `ghostel-use-native-pty' performance globally

;; ESC routing is handled by `evil-ghostel-escape' (default `auto': ESC goes to
;; the terminal while a TUI runs, to evil otherwise); set it to `terminal' if
;; exact vterm-era always-send-escape behavior is missed

;; emulate `akinsho/toggleterm.nvim`
(defun wrapped/ghostel/toggle ()
  (interactive)
  (prep-env-for-term)
  ;; NOTE: never pass a non-nil arg here; +ghostel/toggle's recreate branch
  ;; references a free variable and errors (bug in doom's term/ghostel module)
  (+ghostel/toggle nil))

;; basically bring toggle to `+ghostel/here'
(defun ghostel/full-w/toggle ()
  (interactive)
  (prep-env-for-term)
  ;; handle when ghostel is not loaded yet too
  ;; (+ghostel--buffer-name has no autoload cookie; +ghostel/here loads it)
  (if (fboundp '+ghostel--buffer-name)
      (let ((target-buffer (get-buffer (+ghostel--buffer-name))))
        (if (eq target-buffer nil)
            (+ghostel/here)
          (if (doom-buried-buffer-p target-buffer)
              (switch-to-buffer target-buffer)
            ;; since bury-buffer doesn't do much when there is no other real buffer
            ;; better to kill it instead
            (if (> (length (doom-real-buffer-list)) 1)
                ;; somehow `(bury-buffer target-buffer)` doesn't work
                (bury-buffer)
              ;; NOT `kill-this-buffer' - its own docstring says it "can be
              ;; reliably invoked only from the menu bar, otherwise it could
              ;; decide to silently do nothing", which is exactly what
              ;; happens when called from here (the ghostel_cmd bridge, not
              ;; a mouse event) - `kill-current-buffer' is what it tells you
              ;; to use instead for keyboard/programmatic invocation
              (kill-current-buffer)))))
    ;; when '+ghostel--buffer-name is not bound
    (+ghostel/here)))

(after! evil-ghostel
  ;; C-hjkl = pane navigation with the same semantics as ../../../tmux.conf:
  ;; while a TUI runs in the terminal (alt-screen, mode 1049 - e.g. nvim)
  ;; send the key into the pty so `christoomey/vim-tmux-navigator' handles
  ;; it; at a plain shell prompt move between emacs windows instead,
  ;; matching morevil's normal-state C-hjkl (../morevil/config.el).
  ;; C-l clear and C-k kill-line lose at the prompt, exactly as under tmux.
  ;;
  ;; ghostel-only: this depends on `ghostel--mode-enabled ... 1049' (is the
  ;; pty in alt-screen mode) to know which behavior to pick. vterm has no
  ;; equivalent - vterm-module.c calls vterm_screen_enable_altscreen but
  ;; never exposes a getter for that state to elisp - so there's no signal
  ;; to key this off of there. vterm's insert-state C-hjkl (./vterm.el) just
  ;; always forwards to the pty instead, which is the best available
  ;; fallback without patching the C module.
  (defun ghostel/mux--nav (key evil-window-fn)
    (if (and ghostel--term (ghostel--mode-enabled ghostel--term 1049))
        (ghostel-send-key key "ctrl")
      (call-interactively evil-window-fn)))
  (evil-define-key* 'insert evil-ghostel-mode-map
    (kbd "C-h") (lambda () (interactive) (ghostel/mux--nav "h" #'evil-window-left))
    (kbd "C-j") (lambda () (interactive) (ghostel/mux--nav "j" #'evil-window-down))
    (kbd "C-k") (lambda () (interactive) (ghostel/mux--nav "k" #'evil-window-up))
    (kbd "C-l") (lambda () (interactive) (ghostel/mux--nav "l" #'evil-window-right))))

;; a hack to let zsh to run command on start up - conjunction with ../../../shell/source.zsh
;; shown as a modal (see ./config.el's term-enhance/open-in-modal, matching
;; nvim's toggleterm float for opening emacs the other direction) rather
;; than a plain in-place window swap
(defun ghostel-with-cmd (cmd)
  ;; this will be read by ../../../shell/source.zsh
  (settermenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" cmd)
  ;; not "*ghostel-with-cmd*" - deliberately doesn't start with "*ghostel"
  ;; so ./config.el's exit hook can pattern-match it uniformly with
  ;; vterm-with-cmd's "*quick-editor-vterm*"; +ghostel/here let-binds
  ;; `ghostel-buffer-name' internally so call `ghostel' directly instead
  (let ((ghostel-buffer-name (generate-new-buffer-name "*quick-editor-ghostel*")))
    (term-enhance/open-in-modal
     ghostel-buffer-name
     (lambda () (pop-to-buffer (save-window-excursion (ghostel))))))
  ;; clean up this because it should be set only momentarily
  (settermenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" nil))

;; an option to fallback to neovim
(defun ghostel/open-in-neovim ()
  (interactive)
  (term-enhance/prep-env-for-quick-editor)
  (pcase-let ((`(,file-name . ,line-number) (term-enhance/current-file-and-line)))
    ;; no TERM override needed here (unlike the eterm-color days):
    ;; xterm-ghostty handles nvim cursor shapes fine
    (ghostel-with-cmd (term-enhance/nvim-open-cmd file-name line-number)))
  ;; canceling potential pollutions
  (term-enhance/undo-env-for-quick-editor))

;; this is more of an illusion of hiding than actual hiding (if there is such a thing)
;; so it's probably not perfect but quite usable to handle `+ghostel/here`,
;; `+ghostel/toggle` AND plain split panes (the mw/ mw- ones)
(defun ghostel/hide ()
  (when (derived-mode-p 'ghostel-mode)
    (cond
     ;; the full window one: hide instead of kill so the shell stays warm
     ;; and the next toggle feels prompt
     ((= (count-windows) 1) (ghostel/full-w/toggle))
     ;; doom's popup terminal: same warm-hide reasoning
     ((and (fboundp '+popup-window-p) (+popup-window-p)) (+ghostel/toggle nil))
     ;; a plain split pane (the mw/ mw- ones) - shared body in ../config.el
     (t (term-enhance/mux-close-pane)))))

;; window cleanup on a plain `exit' - shared body in ../config.el
;; (`term-enhance/mux-close-window-on-exit'); `ghostel-exit-functions' runs
;; this before `ghostel-kill-buffer-on-exit' kills the buffer (see
;; `ghostel--sentinel'), so the buffer's window is still discoverable here
(add-hook 'ghostel-exit-functions #'term-enhance/mux-close-window-on-exit)
;; restores the pre-modal window layout once a ghostel-with-cmd quick
;; editor terminal exits - shared body in ../config.el
(add-hook 'ghostel-exit-functions #'term-enhance/close-quick-editor-wconf-on-exit)

;; the doom module hides the modeline via `mode-line-invisible-mode';
;; despite its name this should only kick in for the "full screen one"
(defun ghostel/full-w/turn-off-hide-mode-line ()
  (if (and (fboundp '+ghostel--buffer-name)
           (string= (buffer-name) (+ghostel--buffer-name)))
      (mode-line-invisible-mode -1)))

;; ghostel's half of the shared emacs-as-a-multiplexer commands
;; (../config.el's `term-enhance/mux-new-shell'/`term-enhance/mux-zoom') for
;; the mw* aliases of ../../../../../zsh/integration/multiplexers: exposed
;; through ghostel_cmd (the tty-bound bridge) rather than emacsclient, which
;; would reach whichever instance owns the default server socket (e.g. a
;; bg-daemon that has no ghostel at all), not necessarily THIS emacs
(defun ghostel/mux-new-shell (&optional winop)
  (term-enhance/mux-new-shell
   (lambda ()
     ;; unique buffer per pane, same pattern as `ghostel-with-cmd' above
     (let ((ghostel-buffer-name (generate-new-buffer-name "*ghostel*")))
       (ghostel)))
   winop))

(after! ghostel
  ;; let these consumable from shell side (via ghostel_cmd)
  (add-to-list 'ghostel-eval-cmds '("ghostel/hide" ghostel/hide))
  (add-to-list 'ghostel-eval-cmds '("ghostel/unhide-mode-line" ghostel/full-w/turn-off-hide-mode-line))
  (add-to-list 'ghostel-eval-cmds '("mux/new-shell" ghostel/mux-new-shell))
  (add-to-list 'ghostel-eval-cmds '("mux/zoom" term-enhance/mux-zoom))
  ;; let `switch-theme` consumable from shell side
  (add-to-list 'ghostel-eval-cmds '("switch-theme" (lambda (theme) (switch-doom-theme theme)))))
