;;; tests/parity-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the neovim-parity invariants (see ../modules/compat/neovim/plan.org)
;; that used to be verified by hand after every change - run via
;; ../run-tests, which respawns the daemon first so these see the config
;; exactly as a fresh emacs loads it

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
    (should (equal (substring-no-properties (current-kill 0)) "def"))))

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
  ;; mode to the first real buffer, so assert the wiring not the mode
  (should (memq 'ws-butler-global-mode doom-first-buffer-hook)))

(ert-deftest parity/smartcase-search ()
  (should (eq evil-ex-search-case 'smart)))

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

(defmacro parity--with-main-and-side-window (main-var side-var &rest body)
  "Set up MAIN-VAR (a plain window/buffer) and SIDE-VAR (a dedicated
bottom side-window, mimicking a ghostel/vterm popup terminal) for
../modules/compat/neovim/smart-quit.el's close-window-or-buffer, then
restore the original single-window layout and kill both test buffers."
  (declare (indent 2))
  `(let* ((,main-var (generate-new-buffer "parity-test-main"))
          (,side-var (generate-new-buffer "parity-test-side")))
     (unwind-protect
         (progn
           (delete-other-windows)
           (let ((main-win (selected-window)))
             (let ((side-win (split-window main-win -15 'below)))
               (set-window-buffer main-win ,main-var)
               (set-window-buffer side-win ,side-var)
               (set-window-parameter side-win 'window-side 'bottom)
               (set-window-dedicated-p side-win 'popup)))
           ,@body)
       ;; the test may leave the side-window selected, and
       ;; delete-other-windows refuses to make a side window the only
       ;; window - drop side windows first, then collapse the rest
       (dolist (w (window-list))
         (when (window-parameter w 'window-side) (delete-window w)))
       (delete-other-windows)
       (when (buffer-live-p ,main-var) (kill-buffer ,main-var))
       (when (buffer-live-p ,side-var) (kill-buffer ,side-var)))))

(ert-deftest smart-quit/last-main-window-with-terminal-closes-buffer ()
  "q in the last main window with a side-window terminal below closes
the buffer, stays out of the terminal, and never asks \"Quit Emacs?\"
\(both regressions happened: a quit prompt with a live terminal present,
and a fix that focused the terminal instead of closing the buffer)."
  (parity--with-main-and-side-window main side
    (select-window (get-buffer-window main))
    (with-current-buffer (window-buffer (selected-window))
      (call-interactively #'close-window-or-buffer))
    (should-not (buffer-live-p main))
    (should-not (window-side-p (selected-window)))
    (should (window-live-p (get-buffer-window side)))))

(ert-deftest smart-quit/duplicate-terminal-collapses-to-full-frame ()
  "When killing the last real buffer makes emacs fall back to the very
buffer the side-window terminal already shows (same terminal in two
windows), the side-window is dropped so the terminal takes the whole
frame. Forces that fallback deterministically by making the side buffer
the main window's only prev-buffer."
  (parity--with-main-and-side-window main side
    (let ((main-win (get-buffer-window main)))
      (select-window main-win)
      (set-window-prev-buffers
       main-win
       (with-current-buffer side
         (list (list side (point-min-marker) (point-min-marker)))))
      (set-window-next-buffers main-win nil))
    (with-current-buffer (window-buffer (selected-window))
      (call-interactively #'close-window-or-buffer))
    (should (= (length (window-list)) 1))
    (should (eq (window-buffer (selected-window)) side))))

(ert-deftest smart-quit/q-in-side-window-closes-it ()
  "q while focused IN the side-window terminal deletes that window."
  (parity--with-main-and-side-window main side
    (select-window (get-buffer-window side))
    (with-current-buffer (window-buffer (selected-window))
      (call-interactively #'close-window-or-buffer))
    (should (= (length (window-list)) 1))
    (should (eq (window-buffer (selected-window)) main))))

;;; parity-tests.el ends here
