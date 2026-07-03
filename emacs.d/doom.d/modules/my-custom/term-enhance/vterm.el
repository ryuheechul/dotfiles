;;; $DOOMDIR/modules/my-custom/term-enhance/vterm.el -*- lexical-binding: t; -*-

;; everything specific to the vterm terminal backend. shared plumbing lives
;; in ./config.el; ./integration.el decides whether any of this actually
;; gets bound to a key (ghostel wins by default - see
;; `term-enhance/backend-priority').

;; allow yank but not paste as libvterm does not support
(setq vterm-enable-manipulate-selection-data-by-osc52 t)

;; techincally not envvar but to clean up the mess made with tramp
;; `vterm-shell' - hooked into ../config.el's `prep-env-for-term' since
;; ghostel has no equivalent variable to reset
(add-hook 'term-enhance/prep-env-hook
          (defun vterm/reset-shell-for-tramp ()
            (setq vterm-shell shell-file-name)))

;; setting TERM=eterm-256color worked best with `lf`
;; although some lines might look not properly aligned
(setq vterm-term-environment-variable "eterm-color")

;; send ESC to `vterm` instead of `emacs` - ghostel's `evil-ghostel-escape`
;; handles this itself, vterm needs the explicit toggle
(add-hook 'vterm-mode-hook #'evil-collection-vterm-toggle-send-escape)

;; C-hjkl = pane navigation with the same semantics as ../../../tmux.conf and
;; ../ghostel.el's `ghostel/mux--nav': while a foreground command/TUI holds
;; the prompt, send the key into the pty so `christoomey/vim-tmux-navigator'
;; handles it (e.g. nested nvim); at an idle shell prompt move between emacs
;; windows instead, matching morevil's normal-state C-hjkl
;; (../morevil/config.el).
;;
;; vterm-module.c has no alt-screen getter to key off of (see ../ghostel.el's
;; comment), but `vterm--at-prompt-p' gets us the same distinction a
;; different way: it tracks the OSC "51;A" prompt-end marker sent by
;; ../shell/shim/emacs-vterm-zsh.zsh's `PROMPT=$PROMPT'%{$(vterm_prompt_end)%}''
;; hook (appended onto starship's own PROMPT template, set earlier in
;; ../../../../zsh/my_addons/znap - order matters, or the marker never
;; survives). "cursor sits right where the last prompt ended" turns out to
;; be exactly "idle at the prompt" - confirmed live, this part works.
(defun vterm/mux--nav (evil-window-fn)
  (if (vterm--at-prompt-p)
      (call-interactively evil-window-fn)
    ;; `vterm--self-insert' reads `last-command-event' (not
    ;; `this-command-keys'), so calling it from inside this wrapper still
    ;; forwards the actual originating key
    (vterm--self-insert)))

;; bound from `vterm-mode-hook', NOT a one-shot `after! vterm' block: Doom
;; hooks `evil-collection-init' to `doom-after-modules-config', which fires
;; after every module's config.el - including this one - has already
;; registered its own `with-eval-after-load 'vterm'. So
;; `evil-collection-vterm-setup' is queued AFTER ours and wins the race when
;; `vterm' actually loads later, silently re-binding whatever it touches.
;; It explicitly re-binds C-k/C-l to plain `vterm--self-insert'
;; (https://github.com/emacs-evil/evil-collection/blob/ca4c6172240321a06498390d7d6fa790033f7fc1/modes/vterm/evil-collection-vterm.el#L227-L228)
;; but never touches C-h/C-j - which is exactly why only k/l were getting
;; silently clobbered back to old behavior while h/j worked fine.
;; `vterm-mode-hook' fires per-buffer, strictly after both of those one-time
;; setups have already run, so re-asserting here every time is cheap and
;; guarantees we always have the final say regardless of load order.
(add-hook 'vterm-mode-hook
          (defun vterm/mux--rebind-nav-keys ()
            (evil-collection-define-key 'insert 'vterm-mode-map
              (kbd "C-h") (lambda () (interactive) (vterm/mux--nav #'evil-window-left))
              (kbd "C-j") (lambda () (interactive) (vterm/mux--nav #'evil-window-down))
              (kbd "C-k") (lambda () (interactive) (vterm/mux--nav #'evil-window-up))
              (kbd "C-l") (lambda () (interactive) (vterm/mux--nav #'evil-window-right)))))

;; emulate `akinsho/toggleterm.nvim`
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
              ;; NOT `kill-this-buffer' - its own docstring says it "can be
              ;; reliably invoked only from the menu bar, otherwise it could
              ;; decide to silently do nothing", which is exactly what
              ;; happens when called from here (the vterm_cmd bridge, not a
              ;; mouse event) - this was the "q keeps coming back" bug.
              ;; `kill-current-buffer' is what it tells you to use instead
              ;; for keyboard/programmatic invocation
              (kill-current-buffer)))))
    ;; when 'vterm-buffer-name is not bound
    (+vterm/here nil)))

;; a hack to let zsh run a command on start up - conjunction with ../../../shell/source.zsh
;; currently the assumption of this function is that run it in full window
(defun vterm-with-cmd (cmd)
  ;; this will be read by ../../../shell/source.zsh
  (settermenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" cmd)
  ;; to avoid buffer collision with regular ones
  (setq vterm-buffer-name "*vterm-with-cmd*")
  (+vterm/here nil)
  ;; fix the global name back to normal - assuming the original name is `*vterm*`
  (setq vterm-buffer-name "*vterm*")
  ;; clean up this because it should be set only momentarily
  (settermenv "INSIDE_EMACS_RUN_CMD_ON_START_UP" nil))

;; an option to fallback to neovim
(defun vterm/open-in-neovim ()
  (interactive)
  (term-enhance/prep-env-for-quick-editor)
  (pcase-let ((`(,file-name . ,line-number) (term-enhance/current-file-and-line)))
    ;; this is a workaround that fixes the cursor not changing shape properly
    ;; between modes for neovim with TERM=eterm-color
    (vterm-with-cmd (concat "TERM=xterm-256color " (term-enhance/nvim-open-cmd file-name line-number))))
  ;; canceling potential pollutions
  (term-enhance/undo-env-for-quick-editor))

;; this is more of an illusion of hiding than actual hiding (if there is such a thing)
;; so it's probably not perfect but quite usable to handle `+vterm/here`,
;; `+vterm/toggle` AND plain split panes (the mw/ mw- ones)
(defun vterm/hide ()
  (when (eq major-mode 'vterm-mode)
    (cond
     ;; the full window one: hide instead of kill so the shell stays warm
     ;; and the next toggle feels prompt
     ((= (count-windows) 1) (vterm/full-w/toggle))
     ;; doom's popup terminal: same warm-hide reasoning
     ((and (fboundp '+popup-window-p) (+popup-window-p)) (+vterm/toggle nil))
     ;; a plain split pane (the mw/ mw- ones) - shared body in ../config.el
     (t (term-enhance/mux-close-pane)))))

;; Doom's own term/vterm/config.el hides the modeline via
;; `mode-line-invisible-mode' (the `hide-mode-line' package/
;; `hide-mode-line-excluded-modes' this used to reference is stale - that
;; was the pre-ghostel-era mechanism, and `turn-off-hide-mode-line-mode'
;; being void-function at runtime is exactly how that showed up); despite
;; its name this should only kick in for the "full screen one", mirroring
;; `ghostel/full-w/turn-off-hide-mode-line'
(defun vterm/turn-off-hide-mode-line ()
  (if (string= (buffer-name) vterm-buffer-name)
      (mode-line-invisible-mode -1)))

;; window cleanup on a plain `exit' - shared body in ../config.el
;; (`term-enhance/mux-close-window-on-exit'); `vterm-exit-functions' runs
;; this before `vterm-kill-buffer-on-exit' kills the buffer (see
;; `vterm--sentinel'), so the buffer's window is still discoverable here.
;; same (buf event) signature as ghostel's exit hook, so the shared body
;; works unmodified
(add-hook 'vterm-exit-functions #'term-enhance/mux-close-window-on-exit)

;; vterm's half of the shared emacs-as-a-multiplexer commands
;; (../config.el's `term-enhance/mux-new-shell'/`term-enhance/mux-zoom') for
;; the mw* aliases of ../../../../../zsh/integration/multiplexers: exposed
;; through vterm_cmd (the tty-bound bridge), mirroring ../ghostel.el
(defun vterm/mux-new-shell (&optional winop)
  (term-enhance/mux-new-shell
   (lambda ()
     ;; unique buffer per pane, same pattern as `vterm-with-cmd' above
     (let ((vterm-buffer-name (generate-new-buffer-name "*vterm*")))
       (+vterm/here nil)))
   winop))

(after! vterm
  ;; let these consumable from shell side (via vterm_cmd)
  (add-to-list 'vterm-eval-cmds '("vterm/hide" vterm/hide))
  (add-to-list 'vterm-eval-cmds '("vterm/unhide-mode-line" vterm/turn-off-hide-mode-line))
  (add-to-list 'vterm-eval-cmds '("mux/new-shell" vterm/mux-new-shell))
  (add-to-list 'vterm-eval-cmds '("mux/zoom" term-enhance/mux-zoom))
  ;; let `switch-theme` consumable from shell side
  (add-to-list 'vterm-eval-cmds '("switch-theme" (lambda (theme) (switch-doom-theme theme)))))
