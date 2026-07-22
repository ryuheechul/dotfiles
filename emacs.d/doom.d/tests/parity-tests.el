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

(ert-deftest parity/J-joins-lines ()
  "The baseline J binding joins ordinary adjacent lines, like Neovim."
  (with-temp-buffer
    (insert "first\nsecond\n")
    (goto-char (point-min))
    (evil-local-mode 1)
    (evil-normal-state)
    (call-interactively (key-binding "J"))
    (should (equal (buffer-string) "first second\n"))))

(ert-deftest parity/J-joins-toml-comments ()
  "J joins adjacent TOML comments without retaining the second # prefix."
  (with-temp-buffer
    (toml-ts-mode)
    (insert "# one\n# two\n")
    (goto-char (point-min))
    (evil-local-mode 1)
    (evil-normal-state)
    (call-interactively (key-binding "J"))
    (should (equal (buffer-string) "# one two\n"))))

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
  "nvim ignorecase+smartcase parity: a lowercase / pattern folds case, an
uppercase letter forces a sensitive match. `evil-ex-find-next' lands point
just past the match, so the end offsets below distinguish the two."
  (should (eq evil-ex-search-case 'smart))
  (with-temp-buffer
    (insert "aaa BANANA banana Banana zzz")
    ;; lowercase pattern folds case - first hit is BANANA (ends at 11)
    (goto-char (point-min))
    (evil-ex-find-next (evil-ex-make-search-pattern "banana") 'forward t)
    (should (= (point) 11))
    ;; an uppercase letter forces sensitivity - skips to capitalized Banana
    (goto-char (point-min))
    (evil-ex-find-next (evil-ex-make-search-pattern "Banana") 'forward t)
    (should (= (point) 25))))

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
  "Three branches: a lone candidate completes; a fresh popup (point on the
preselect) lands on the first candidate; an in-progress selection cycles."
  (require 'corfu)
  (let (called)
    (cl-letf (((symbol-function 'corfu-insert) (lambda () (setq called 'insert)))
              ((symbol-function 'corfu-next) (lambda (&optional _) (setq called 'next)))
              ((symbol-function 'corfu--goto) (lambda (i) (setq called (cons 'goto i)))))
      ;; lone candidate -> insert (directory descent handled by cape-file's
      ;; injected exit-function, so a plain insert suffices)
      (let ((corfu--total 1)) (+neovim/corfu-tab))
      (should (eq called 'insert))
      ;; fresh popup (index == preselect) -> land on the first, detach preselect
      (setq called nil)
      (let ((corfu--total 3) (corfu--index 0) (corfu--preselect 0))
        (+neovim/corfu-tab)
        (should (equal called '(goto . 0)))
        (should (= corfu--preselect -1)))
      ;; already navigating (index != preselect) -> cycle
      (setq called nil)
      (let ((corfu--total 3) (corfu--index 1) (corfu--preselect -1))
        (+neovim/corfu-tab))
      (should (eq called 'next)))))

(ert-deftest parity/corfu-tab-selects-first-not-second ()
  "Regression (2026-07-13): on a fresh popup TAB lands on the FIRST
candidate, not the second. `corfu-preselect' first silently selects
candidate 0 but corfu can't preview the preselect, so it reads as unselected
(seen on ./ path completion) and a plain corfu-next would skip to candidate
1. The first press must stay on 0 (and now preview it); the next cycles."
  (require 'corfu)
  (let ((buf (parity--corfu-undo-buffer '("aaa1" "aaa2" "aaa3"))))
    (unwind-protect
        (with-current-buffer buf
          (evil-insert-state)
          (execute-kbd-macro (kbd "aaa"))
          (completion-at-point)
          (should (= corfu--index 0))               ; first is preselected
          (should-not (corfu--preview-current-p))   ; but not previewed - the trap
          (+neovim/corfu-tab)
          (should (= corfu--index 0))               ; first TAB stays on the FIRST
          (should (corfu--preview-current-p))       ; and now previews it
          (+neovim/corfu-tab)
          (should (= corfu--index 1)))              ; second TAB -> second
      (kill-buffer buf))))

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

(defun parity--corfu-undo-buffer (candidates)
  "Set up a windowed buffer with corfu + evil and a capf offering CANDIDATES.
`execute-kbd-macro' feeds the selected window's buffer, and corfu needs a
live window, so this is a real buffer rather than `with-temp-buffer'."
  (require 'corfu)
  (let ((buf (get-buffer-create "*parity-corfu-undo*")))
    (set-window-buffer (selected-window) buf)
    (with-current-buffer buf
      (fundamental-mode)
      (erase-buffer)
      (setq-local completion-at-point-functions
                  (list (lambda ()
                          (list (line-beginning-position) (point)
                                candidates :exclusive 'no))))
      (corfu-mode 1)
      (evil-local-mode 1))
    buf))

(ert-deftest parity/corfu-undo-through-completion ()
  "Accepting a corfu candidate does not split evil's insert-state undo:
after one insert session (typed prefix + accepted completion), a single
`u' returns to the pre-insert state and `C-r' restores it. Guards the
`evil-want-fine-undo' nil grouping against a completion inserting its own
undo boundary (which would leave stray text after one undo) - the kind of
undo/redo strangeness that is easy to hit with completion."
  (let ((buf (parity--corfu-undo-buffer '("parallelism"))))
    (unwind-protect
        (with-current-buffer buf
          (evil-insert-state)
          (execute-kbd-macro (kbd "para"))
          (completion-at-point)
          (corfu-insert)
          (evil-force-normal-state)
          (should (equal (buffer-string) "parallelism"))
          (evil-undo 1)
          (should (equal (buffer-string) ""))
          (evil-redo 1)
          (should (equal (buffer-string) "parallelism")))
      (kill-buffer buf))))

(ert-deftest parity/corfu-escape-no-preview-leak ()
  "Escaping insert state while a candidate is previewed
(`corfu-preview-current' is `insert') must not commit the preview: doom
quits corfu on `evil-insert-state-exit-hook', so only the typed prefix
survives and undo stays clean (no leaked candidate text to undo twice)."
  (let ((buf (parity--corfu-undo-buffer '("parallelism" "paragraph"))))
    (unwind-protect
        (with-current-buffer buf
          (evil-insert-state)
          (execute-kbd-macro (kbd "par"))
          (completion-at-point)
          (corfu-next)                  ; preview a candidate
          (evil-force-normal-state)
          ;; the previewed candidate's distinctive tail never lands in the buffer
          (should-not (string-match-p "graph\\|llelism" (buffer-string)))
          (evil-undo 1)
          (should (equal (buffer-string) "")))
      (kill-buffer buf))))

(ert-deftest parity/corfu-file-descends-into-directory ()
  "nvim cmp-path parity: completing a directory keeps offering its contents.
cape-file ships no exit-function, so corfu stopped at the inserted `dir/';
the advice injects one that re-triggers completion when a directory is
inserted. Exercised with a real temp dir via the lone-candidate path
(`corfu-insert', which is what TAB runs for one match)."
  (require 'corfu)
  (require 'cape)
  (let* ((dir (make-temp-file "parity-cape-" t))
         (sub (expand-file-name "onlysub" dir))
         (buf (get-buffer-create "*parity-cape-dir*")))
    (make-directory sub)
    (write-region "" nil (expand-file-name "f1.txt" sub))
    (write-region "" nil (expand-file-name "f2.txt" sub))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (fundamental-mode)
            (erase-buffer)
            (setq default-directory (file-name-as-directory dir))
            (setq-local completion-at-point-functions (list #'cape-file))
            (corfu-mode 1)
            (evil-local-mode 1)
            (evil-insert-state)
            (execute-kbd-macro (kbd "./on"))
            (completion-at-point)          ; lone candidate: onlysub/
            (corfu-insert)                 ; insert dir -> exit-function descends
            (should (string-prefix-p "./onlysub/" (buffer-string)))
            (should (equal (sort (mapcar #'substring-no-properties corfu--candidates)
                                 #'string<)
                           '("f1.txt" "f2.txt")))))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest parity/buffer-word-completion ()
  "nvim `buffer' cmp source parity: words already in the buffer complete via
cape-dabbrev (type app -> apple), wired as a low-priority capf in the
editing-mode families."
  (require 'cape)
  (dolist (mode '(emacs-lisp-mode text-mode conf-unix-mode))
    (with-temp-buffer
      (funcall mode)
      (should (memq 'cape-dabbrev completion-at-point-functions))))
  (with-temp-buffer
    (prog-mode)
    (insert "apple banana\napp")
    (should (member "apple" (all-completions "app" (nth 2 (cape-dabbrev)))))))

(ert-deftest parity/prose-completion-merges-spell-and-buffer ()
  "nvim shows all sources at once; emacs' capf chain lets the prose spell
source shadow cape-dabbrev, so a buffer word only appears after the word is
finished. In text-mode the two are merged (cape-capf-super) so a buffer word
(current-base16) shows alongside dictionary words while still partial (curr).
Ordering is asserted separately (corfu-sort-override-function)."
  (require 'cape)
  (with-temp-buffer
    (text-mode)
    (run-hooks 'text-mode-hook)
    (insert "current-base16\ncurr")
    (let ((cands
           (catch 'hit
             (dolist (f completion-at-point-functions)
               (when (functionp f)
                 (let ((r (ignore-errors (funcall f))))
                   (when (and r (listp r))
                     (let ((cc (all-completions "curr" (nth 2 r))))
                       (when cc (throw 'hit cc))))))))))
      ;; the first matching capf yields BOTH the buffer word and dict words
      (should (member "current-base16" cands))
      (should (> (length cands) 1)))))

(ert-deftest parity/corfu-buffer-words-first-sort ()
  "The sort partitions buffer words (dabbrev source) ahead of the rest,
stably, and treats a hyphenated token as one word (current-base16, not
current)."
  (with-temp-buffer
    (insert "current-base16 alpha\n")
    (should (equal (+neovim/corfu-buffer-words-first
                    '("current" "current-base16" "alpha" "currency"))
                   '("current-base16" "alpha" "current" "currency")))))

(ert-deftest parity/prose-buffer-words-rank-first ()
  "Regression (2026-07-13): in the live corfu popup the buffer word ranks
above the spell source (corfu-sort-override-function), so the dictionary's
many words don't bury it - the earlier version left current-base16 last."
  (require 'corfu)
  (require 'cape)
  (let ((buf (get-buffer-create "*parity-prose-order*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (text-mode)
            (run-hooks 'text-mode-hook)
            (erase-buffer)
            (insert "current-base16\n")
            (corfu-mode 1)
            (evil-local-mode 1)
            (evil-insert-state)
            (execute-kbd-macro (kbd "curren"))
            (completion-at-point)
            (let ((cands (mapcar #'substring-no-properties corfu--candidates)))
              (should (member "current-base16" cands))
              (when (member "currency" cands)
                (should (< (cl-position "current-base16" cands :test #'equal)
                           (cl-position "currency" cands :test #'equal)))))))
      (kill-buffer buf))))

(ert-deftest parity/operator-jk-full-linewise ()
  "dj/dk/yj span two FULL lines including the cursor line, like nvim.
`evil-respect-visual-line-mode' makes bare j/k visual-line motions; without
the operator-state override that left dj a 1-line exclusive delete that
dropped the line below - the muscle-memory hazard this guards against."
  (let ((buf (get-buffer-create "*parity-opjk*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (dolist (spec '(("dj" "line1\nline4\n")   ; cursor line + below
                          ("dk" "line3\nline4\n")))  ; cursor line + above
            (with-current-buffer buf
              (visual-line-mode 1)
              (erase-buffer)
              (insert "line1\nline2\nline3\nline4\n")
              (goto-char (point-min))
              (forward-line 1)               ; on line2
              (evil-local-mode 1)
              (evil-normal-state)
              (execute-kbd-macro (kbd (car spec)))
              (should (equal (buffer-string) (cadr spec)))))
          ;; yj yanks the two full lines (cursor + below), not one
          (with-current-buffer buf
            (erase-buffer)
            (insert "line1\nline2\nline3\nline4\n")
            (goto-char (point-min))
            (forward-line 1)
            (evil-normal-state)
            (execute-kbd-macro (kbd "yj"))
            (let ((interprogram-paste-function nil))
              (should (equal (substring-no-properties (current-kill 0))
                             "line2\nline3\n")))))
      (kill-buffer buf))))

(ert-deftest parity/long-line-hint-is-subtle-rule-not-aggressive-colour ()
  "Long lines get a dim vertical rule at `fill-column'
(display-fill-column-indicator), not whitespace-mode's `lines' style
painting the whole tail in the loud `whitespace-line' face."
  (require 'whitespace)
  (should-not (memq 'lines whitespace-style))
  ;; column is not hardcoded - the indicator tracks `fill-column' (its `t'
  ;; default), and the `editorconfig' module sets `fill-column' from
  ;; `max_line_length' (100) in the repo .editorconfig: one source of truth
  (should (eq (default-value 'display-fill-column-indicator-column) t))
  ;; doom turns editorconfig-mode on at the first real buffer, which a cold
  ;; test daemon has not seen yet - force it, then a real file must pick up 100
  (require 'editorconfig)
  (editorconfig-mode 1)
  (let ((buf (find-file-noselect (expand-file-name "init.el" doom-user-dir))))
    (unwind-protect
        (with-current-buffer buf (should (= fill-column 100)))
      (kill-buffer buf)))
  (dolist (mode '(emacs-lisp-mode text-mode conf-unix-mode))
    (with-temp-buffer
      (funcall mode)
      (should (bound-and-true-p display-fill-column-indicator-mode)))))

(ert-deftest parity/treemacs-roots-at-current-project ()
  "SPC f t / SPC t f open the sidebar rooted at the CURRENT project
(+treemacs/toggle), not raw `treemacs' - which reopens the last persisted
session (the wrong dir on a shared daemon, since treemacs state is global
apart from its per-persp scope)."
  (dolist (keys '("f t" "t f"))
    (should (eq (lookup-key doom-leader-map (kbd keys)) '+treemacs/toggle))))

;;; parity-tests.el ends here
