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

;;; smart-quit-tests.el ends here
