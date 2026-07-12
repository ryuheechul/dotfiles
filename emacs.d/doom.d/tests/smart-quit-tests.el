;;; tests/smart-quit-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the executable half of the behavior matrix in
;; ../modules/compat/neovim/smart-quit.el's header comment (see
;; ./README.org for the one-test-file-per-contract model) - run via
;; ../bin/run-tests

(require 'ert)

(defmacro smart-quit--with-main-and-side-window (main-var side-var &rest body)
  "Set up MAIN-VAR (a plain window/buffer) and SIDE-VAR (a dedicated
bottom side-window, mimicking a ghostel/vterm popup terminal) for
close-window-or-buffer, then restore the original single-window layout
and kill both test buffers."
  (declare (indent 2))
  `(let* ((,main-var (generate-new-buffer "smart-quit-test-main"))
          (,side-var (generate-new-buffer "smart-quit-test-side")))
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
  (smart-quit--with-main-and-side-window main side
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
  (smart-quit--with-main-and-side-window main side
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
  (smart-quit--with-main-and-side-window main side
    (select-window (get-buffer-window side))
    (with-current-buffer (window-buffer (selected-window))
      (call-interactively #'close-window-or-buffer))
    (should (= (length (window-list)) 1))
    (should (eq (window-buffer (selected-window)) main))))

(ert-deftest smart-quit/closing-buffer-with-quit-restore-does-not-strand-window ()
  "Regression (2026-07-10): a buffer displayed via
`pop-to-buffer-same-window' (as magit-diff-visit-file does when `e' on a
hunk visits the file in place) sets a `quit-restore' window parameter.
Closing that buffer via plain `kill-buffer' left it stale once emacs
fell back to redisplaying the previous buffer (e.g. magit-status) - that
buffer's own next q silently could not make any further progress. Real
repro: magit status -> e on a hunk -> close the visited file -> q on
magit status again did nothing."
  (smart-quit--with-main-and-side-window main side
    (let ((visited (generate-new-buffer "smart-quit-test-visited")))
      (unwind-protect
          (progn
            (select-window (get-buffer-window main))
            (pop-to-buffer-same-window visited)
            (should (window-parameter (selected-window) 'quit-restore))
            (with-current-buffer (window-buffer (selected-window))
              (call-interactively #'close-window-or-buffer))
            (should-not (buffer-live-p visited))
            (should (eq (window-buffer (selected-window)) main))
            ;; closing `main' now must still make progress, not silently
            ;; no-op like the bug did
            (with-current-buffer (window-buffer (selected-window))
              (call-interactively #'close-window-or-buffer))
            (should-not (buffer-live-p main)))
        (when (buffer-live-p visited) (kill-buffer visited))))))

(ert-deftest smart-quit/magit-quit-falls-back-when-bury-is-inert ()
  "Regression (2026-07-10): magit's own q (doom's +magit/quit,
../modules/my-custom/morevil/config.el) doesn't go through
`close-window-or-buffer' at all normally - it buries via
`magit-bury-buffer-function', then checks whether a magit window is
still visible to decide whether to escalate. Once that bury is a no-op
(same bug as the test above, but hitting magit's OWN quit path instead
of this file's), the check is fooled: the window IS still showing a
magit buffer, just the same stuck one. The +magit-quit-actually-quit-a
advice there detects the no-op and falls back to
`close-window-or-buffer'. Simulates the no-op bury directly rather than
reproducing the full e-then-q setup (fragile to drive headlessly)."
  (require 'magit)
  (let ((buf (generate-new-buffer "smart-quit-test-magit-quit"))
        (fallback-ran nil))
    (unwind-protect
        (with-current-buffer buf
          (magit-mode)
          (setq-local magit--default-directory "/tmp/")
          (delete-other-windows)
          (switch-to-buffer buf)
          (let ((magit-bury-buffer-function #'ignore))
            (cl-letf (((symbol-function 'close-window-or-buffer)
                       (lambda () (interactive) (setq fallback-ran t)))
                      ((symbol-function 'magit-toplevel)
                       (lambda (&rest _) "/tmp/")))
              (+magit/quit)))
          (should fallback-ran))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest smart-quit/server-client-buffer-q-finishes-client ()
  "q on a buffer a server client is waiting on ($EDITOR - git commit
etc.) calls `server-edit' (finish the client) before any
window/workspace/quit handling."
  (let ((buf (generate-new-buffer "smart-quit-test-client"))
        (edited nil))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (setq-local server-buffer-clients '(fake-proc))
          (cl-letf (((symbol-function 'server-edit)
                     (lambda (&rest _) (setq edited t))))
            (call-interactively #'close-window-or-buffer))
          (should edited))
      (with-current-buffer buf
        ;; or server's kill-buffer query would prompt in batch
        (setq-local server-buffer-clients nil))
      (kill-buffer buf))))

(ert-deftest smart-quit/last-window-of-extra-workspace-returns-to-previous ()
  "Regression (2026-07-11): q on the last window of a summoned
workspace (term-enhance/server-window-workspace) killed the whole
emacsclient - the last-main-window branch never considered other
workspaces - and the first fix returned via doom's +workspace/kill on
the CURRENT workspace, whose frame-predicate check swaps an \"unreal\"
buffer (the terminal being returned to) for the dashboard. q must land
back on the previous workspace with its buffer showing, never quit."
  (skip-unless (bound-and-true-p persp-mode))
  (let* ((origin-ws (+workspace-current-name))
         ;; star name = doom-unreal, like a vterm/ghostel terminal
         (term-buf (generate-new-buffer "*smart-quit-test-term*"))
         (file-buf (generate-new-buffer "smart-quit-test-file"))
         summon-ws)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer term-buf)
          (+workspace/new)
          (setq summon-ws (+workspace-current-name))
          (switch-to-buffer file-buf)
          (cl-letf (((symbol-function 'doom-real-buffer-list)
                     (lambda (&rest _) (list file-buf)))
                    ((symbol-function 'save-buffers-kill-terminal)
                     (lambda (&rest _) (error "q must never quit emacs here"))))
            (call-interactively #'close-window-or-buffer))
          (should (equal (+workspace-current-name) origin-ws))
          ;; the daemon's initial frame is unmanaged by persp, so the
          ;; origin window conf (term-buf showing) can't be asserted
          ;; headlessly - but the dashboard-swap regression can: the
          ;; kill-CURRENT-workspace path would have swapped the window
          ;; to the fallback buffer and the terminal must survive
          (should-not (eq (window-buffer (selected-window))
                          (doom-fallback-buffer)))
          (should (buffer-live-p term-buf))
          (should-not (+workspace-exists-p summon-ws)))
      (when (and summon-ws (+workspace-exists-p summon-ws))
        (+workspace-kill summon-ws))
      (unless (equal (+workspace-current-name) origin-ws)
        (+workspace-switch origin-ws))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf)))))

;;; smart-quit-tests.el ends here
