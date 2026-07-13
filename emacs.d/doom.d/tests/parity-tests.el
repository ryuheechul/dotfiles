;;; tests/parity-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the executable half of the neovim-parity plan
;; (../modules/compat/neovim/plan.org; see ./README.org for the
;; one-test-file-per-contract model) - invariants that used to be
;; verified by hand after every change. run via ../bin/run-tests, which
;; respawns the daemon first so these see the config exactly as a fresh
;; emacs loads it

(require 'ert)

(ert-deftest parity/normal-state-bindings ()
  "The ported muscle-memory keys resolve to the intended commands."
  (dolist (spec '((","   evil-ex-repeat)
                  ("gl"  +neovim/goto-link)
                  ("gn"  better-jumper-jump-forward)
                  ("go"  better-jumper-jump-backward)
                  ("gr"  +lookup/references)
                  ("+"   evil-numbers/inc-at-pt)
                  ("-"   evil-numbers/dec-at-pt)))
    (should (eq (lookup-key evil-normal-state-map (kbd (car spec)))
                (cadr spec)))))

(ert-deftest parity/suspend-binding-is-a-command ()
  "\\s is bound (frame-type dispatch lambda, so just command-ness)."
  (should (commandp (lookup-key evil-normal-state-map "\\s"))))

(ert-deftest parity/leader-bindings ()
  (dolist (spec '(("t l" whitespace-mode)
                  ("t n" doom/toggle-line-numbers)
                  ("b t" delete-trailing-whitespace)))
    (should (eq (lookup-key doom-leader-map (kbd (car spec)))
                (cadr spec)))))

(ert-deftest parity/Y-yanks-to-eol ()
  "Y mid-line yanks to end of line (nvim), not the whole line (evil default)."
  (with-temp-buffer
    (insert "abc def")
    (goto-char 5)
    (evil-local-mode 1)
    (evil-normal-state)
    (call-interactively (key-binding "Y"))
    ;; nil paste function: this asserts kill-ring semantics - the live
    ;; +neovim/frame-aware-paste would hand current-kill the machine's
    ;; real clipboard here (batch daemon frames are non-graphic)
    (let ((interprogram-paste-function nil))
      (should (equal (substring-no-properties (current-kill 0)) "def")))))

(ert-deftest parity/percent-jumps-language-keywords ()
  "evil-matchit: % on a lua `if' lands on its `end' (vim-matchup parity)."
  (with-temp-buffer
    (lua-mode)
    (insert "if x then\ny = 1\nend")
    (goto-char 1)
    (evil-local-mode 1)
    (evil-normal-state)
    (call-interactively (key-binding "%"))
    (should (= (line-number-at-pos) 3))))

(ert-deftest parity/qol-modes-enabled ()
  (should save-place-mode)
  (should global-auto-revert-mode)
  (should global-evil-matchit-mode)
  (should (memq 'flyspell-prog-mode prog-mode-hook))
  ;; trim-on-save is ws-butler's job now (:editor whitespace +trim), not a
  ;; blunt before-save delete-trailing-whitespace; doom defers the global
  ;; mode to the first real buffer, so accept either state: still queued
  ;; on the hook, or already enabled because an earlier test's find-file
  ;; fired doom-first-buffer-hook (test files load alphabetically)
  (should (or (memq 'ws-butler-global-mode doom-first-buffer-hook)
              (bound-and-true-p ws-butler-global-mode))))

(ert-deftest parity/smartcase-search ()
  (should (eq evil-ex-search-case 'smart)))

(ert-deftest parity/tty-paste-reads-system-clipboard ()
  "Regression (2026-07-11, pre-existing): evil's p in a TTY frame never
saw the system clipboard - OSC 52 only carries kills OUT.
+neovim/frame-aware-paste reads pbpaste on non-graphic frames (batch
daemon frames are non-graphic, so that branch is what runs here) and
dedups against the kill-ring head so yank rotation still works."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
            ((symbol-function 'shell-command-to-string)
             (lambda (_) "from-system")))
    (let ((kill-ring nil))
      (should (equal (+neovim/frame-aware-paste) "from-system")))
    (let ((kill-ring '("from-system")))
      (should-not (+neovim/frame-aware-paste)))))

(ert-deftest parity/spell-checks-comments-not-strings ()
  "Functional spell check: comment typos flagged, string typos ignored.
Would have caught both the aspell-vs-ispell scare and the face scoping."
  (with-temp-buffer
    (lua-mode)
    (insert "-- helllo wrold\nlocal x = \"strngtypoo\"\n")
    (font-lock-ensure)
    (flyspell-prog-mode)
    (flyspell-buffer)
    (let ((flagged
           (mapcar (lambda (o)
                     (buffer-substring-no-properties
                      (overlay-start o) (overlay-end o)))
                   (cl-remove-if-not
                    (lambda (o) (overlay-get o 'flyspell-overlay))
                    (overlays-in (point-min) (point-max))))))
      (should (member "helllo" flagged))
      (should-not (member "strngtypoo" flagged)))))

(ert-deftest parity/dashboard-new-buffer ()
  "The dashboard menu entry, its i binding, and the insert-state refusal."
  (should (assoc "New unnamed buffer" +dashboard-menu-sections))
  (should (eq (lookup-key (evil-get-auxiliary-keymap
                           +dashboard-mode-map 'normal)
                          "i")
              '+default/new-buffer))
  (with-current-buffer (get-buffer-create "*parity-dashboard-test*")
    (+dashboard-mode)
    (evil-local-mode 1)
    (evil-normal-state)
    (evil-insert-state)
    (unwind-protect
        (should (eq evil-state 'normal))
      (kill-buffer))))

(ert-deftest parity/whitespace-listchars-style ()
  "listchars port: whitespace-mode set up with the intended style.
Requires the package first - doom defers it to the first real buffer;
the no-spaces assertion distinguishes our custom style from the default
(which also carries tab-mark/trailing but marks every space too)."
  (require 'whitespace)
  (should (memq 'tab-mark whitespace-style))
  (should (memq 'trailing whitespace-style))
  (should-not (memq 'spaces whitespace-style)))

(ert-deftest parity/path-completion-capf-everywhere ()
  "cape-file (nvim cmp-path parity) reaches text/conf modes too, not
just doom's prog-mode default - ./ and ../ complete in org/markdown/
plain text like they do in nvim."
  (dolist (mode '(emacs-lisp-mode text-mode conf-unix-mode))
    (with-temp-buffer
      (funcall mode)
      (should (memq 'cape-file completion-at-point-functions)))))

(ert-deftest parity/corfu-tab-bound ()
  "TAB in the corfu popup runs our wrapper, in both key symbols (GUI/TTY)."
  (require 'corfu)
  (should (eq (lookup-key corfu-map (kbd "TAB")) '+neovim/corfu-tab))
  (should (eq (lookup-key corfu-map [tab]) '+neovim/corfu-tab)))

(ert-deftest parity/corfu-tab-continues-completion ()
  "Regression (2026-07-12): the wrapper must be a `corfu-continue-command'.
corfu quits after any command not listed there (nor matching its
`\\`corfu-' regex); +neovim/corfu-tab matches neither, so without the
explicit registration the popup died after a single press - cycled once,
then `corfu--post-command' quit it."
  (require 'corfu)
  (should (memq '+neovim/corfu-tab corfu-continue-commands)))

(ert-deftest parity/corfu-tab-dispatch ()
  "Lone candidate completes, several candidates cycle (the TAB parity)."
  (require 'corfu)
  (let (called)
    (cl-letf (((symbol-function 'corfu-complete) (lambda () (setq called 'complete)))
              ((symbol-function 'corfu-next) (lambda (&optional _) (setq called 'next))))
      (let ((corfu--total 1)) (+neovim/corfu-tab))
      (should (eq called 'complete))
      (setq called nil)
      (let ((corfu--total 3)) (+neovim/corfu-tab))
      (should (eq called 'next)))))

(ert-deftest parity/titlecase-operator ()
  "nvim's vim-titlecase gz: `gzil' titlecases the current line, composing the
`gz' operator with the `il' line text object (vim-textobj-line parity).
A real windowed buffer (not `with-temp-buffer') because `execute-kbd-macro'
feeds keys to the selected window's buffer."
  (should (eq (lookup-key evil-normal-state-map "gz") '+neovim/evil-titlecase))
  (let ((buf (get-buffer-create "*parity-titlecase-test*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (erase-buffer)
            (insert "  hello WORLD from EMACS\n")
            (goto-char (point-min))
            (evil-local-mode 1)
            (evil-normal-state)
            (execute-kbd-macro (kbd "gzil"))
            ;; `il' is inner line (skips the leading indent), so the content
            ;; is titlecased and the indentation left untouched
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           "  Hello World From Emacs\n"))))
      (kill-buffer buf))))

;;; parity-tests.el ends here
