;;; $DOOMDIR/modules/my-custom/term-enhance/integration.el -*- lexical-binding: t; -*-

;; single place for how the two terminal backends (ghostel/vterm) integrate
;; with the rest of emacs: which one the shared, user-facing keybindings
;; below actually act through (so nothing else - this module's own files,
;; ../morevil, ... - has to hardcode "ghostel wins"), plus cross-cutting
;; integration concerns that apply regardless of which backend is picked -
;; tramp is one of those (see both the tramp override in
;; `term-enhance/backend' and the envrc fix below). edit
;; `term-enhance/backend-priority' to change the LOCAL preference - nothing
;; else needs to change, as long as ./ghostel.el and ./vterm.el keep
;; providing the functions dispatched to below.

(defvar term-enhance/backend-priority '(ghostel vterm)
  "Preferred terminal backends, most-preferred first.
Only backends actually enabled as doom modules (`:term ghostel'/`:term
vterm' in init.el) are considered; the first available one wins.")

(defun term-enhance/backend ()
  "Return the terminal backend to dispatch to: `ghostel' or `vterm'.
Uses `doom-module-active-p' (a plain function, unlike the `modulep!' macro)
since the module to check is picked at runtime, not known at compile time."
  (seq-find (lambda (b) (doom-module-active-p :term b)) term-enhance/backend-priority))

;; direnv/envrc integration (doom's :tools direnv, enabled over tramp too
;; by ../../tools/tramp-support/config.el's `envrc-remote t') makes no
;; sense in a terminal buffer - the shell running there already manages
;; its own env (its own direnv hook, if it has one) - and leaving it on
;; is actively harmful over tramp: `envrc--update' walks UP the directory
;; tree looking for a `.envrc', and each parent directory checked is a
;; SEPARATE, SYNCHRONOUS SSH ROUND-TRIP. caught live via SIGUSR2 + a
;; backtrace during a real hang (2026-07-04): this - not anything in
;; vterm/ghostel/tramp's own terminal-spawning code - is what caused the
;; intermittent "SPC f n hangs on a tramp buffer" symptom (it only hangs
;; when that walk happens to be slow, which is why it didn't reproduce
;; every time). skip the expensive part entirely for both terminal
;; backends, local or remote - `envrc-mode' itself may still nominally
;; turn on (harmless: a lighter with nothing behind it), it just never
;; does its slow work here
(after! envrc
  (defadvice! term-enhance/skip-envrc-update-in-terminals-a (fn &rest args)
    :around #'envrc--update
    (unless (derived-mode-p 'vterm-mode 'ghostel-mode)
      (apply fn args))))

;; flycheck makes as little sense in a terminal buffer as envrc above, and
;; leaving it on is what broke ghostel over tramp: tramp uses the terminal
;; buffer as its connection buffer, so the ssh handshake is inserted there
;; while tramp holds the connection lock - flycheck's after-change hook then
;; probes the remote `default-directory', and that nested tramp op dies with
;; "Forbidden reentrant call of Tramp" on every remote spawn (see
;; ./known-issues.org item 1). must be this predicate, not a mode hook:
;; global minor modes enable AFTER mode hooks run
(after! flycheck
  (setq flycheck-global-modes '(not ghostel-mode vterm-mode)))

(defun term-enhance/dispatch (fn-alist &rest args)
  "Call the function in FN-ALIST keyed by the active backend, with ARGS."
  (apply (alist-get (term-enhance/backend) fn-alist) args))

(defun term-enhance/toggle ()
  (interactive)
  (term-enhance/dispatch '((ghostel . wrapped/ghostel/toggle)
                            (vterm . wrapped/vterm/toggle))))

(defun term-enhance/full-w-toggle ()
  (interactive)
  (term-enhance/dispatch '((ghostel . ghostel/full-w/toggle)
                            (vterm . vterm/full-w/toggle))))

(defun term-enhance/open-in-neovim ()
  (interactive)
  (term-enhance/dispatch '((ghostel . ghostel/open-in-neovim)
                            (vterm . vterm/open-in-neovim))))

;; emulate `akinsho/toggleterm.nvim`
(map! :leader :g "'" #'term-enhance/toggle)
;; reverse the default keybinding for `t' and `T'
(map! :leader :prefix "o" :g "t" #'term-enhance/full-w-toggle)
;; TODO: `T' can be better used for running Tmux probably
(map! :leader :prefix "o" :g "T" #'term-enhance/toggle)
(map! :leader :prefix "f" :n "n" #'term-enhance/open-in-neovim)

;; which-key labels: Doom's own default `+evil-bindings.el' sets "Toggle
;; vterm popup"/etc. on these same key sequences (guarded on `:term vterm'
;; being enabled, which it is - vterm stays the fallback), and since `map!'
;; ties its `:desc' to the KEY SEQUENCE (not the command), overriding the
;; command above doesn't clear that stale text; it would always say
;; "vterm" regardless of which backend `term-enhance/backend-priority'
;; actually picked. `which-key-add-key-based-replacements' overrides it
;; explicitly, computed from the real dispatch result rather than assuming
;; ghostel like the rest of this repo's comments do.
(let ((backend (symbol-name (term-enhance/backend))))
  (which-key-add-key-based-replacements
    "SPC '"   (format "Toggle %s popup" backend)
    "SPC o t" (format "Open %s here" backend)
    "SPC o T" (format "Toggle %s popup" backend)
    "SPC f n" (format "Open in neovim (via %s)" backend)))
