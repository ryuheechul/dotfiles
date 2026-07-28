;;; tests/morevil-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the executable half of the behavior contract in
;; ../modules/my-custom/morevil/config.el (see ./README.org for the
;; one-test-file-per-contract model) - run via ../bin/run-tests

(require 'ert)

;; --- C-hjkl intercept keymap ---

(ert-deftest morevil/navigation-override-mode-active ()
  "The global minor mode that keeps the intercept map in
`current-active-maps' must be active after config load."
  (should (bound-and-true-p my/navigation-override-mode)))

(ert-deftest morevil/intercept-map-has-intercept-state-marker ()
  "The keymap must be stamped with [intercept-state] so
`evil-state-intercept-keymaps' recognizes it as a tier-1 map."
  (should (evil-intercept-keymap-p my/navigation-override-map 'normal)))

(ert-deftest morevil/c-hjkl-bind-present ()
  "C-h, C-j, C-k, C-l must all be bound in the intercept keymap."
  (dolist (key '(?h ?j ?k ?l))
    (should (lookup-key my/navigation-override-map (kbd (format "C-%c" key))))))

(ert-deftest morevil/intercept-map-in-evil-mode-map-alist ()
  "With evil-local-mode active, the intercept keymap must appear in
`evil-mode-map-alist' so evil's key dispatch can find it."
  (evil-local-mode 1)
  (evil-normalize-keymaps)
  (should (assq 'my/navigation-override-mode evil-mode-map-alist)))

(ert-deftest morevil/intercept-map-at-front-of-alist ()
  "With evil-local-mode active, the intercept keymap must be among the
first entries in `evil-mode-map-alist' (intercept tier) so its bindings
win over auxiliary, overriding, and state keymaps."
  (evil-local-mode 1)
  (evil-normalize-keymaps)
  (let ((entry (assq 'my/navigation-override-mode evil-mode-map-alist)))
    (should entry)
    ;; intercept maps come before state/auxiliary/overriding maps;
    ;; verify our entry precedes any non-intercept entry
    (let ((pos (cl-position entry evil-mode-map-alist))
          (first-non-intercept
           (cl-position-if
            (lambda (e)
              (not (evil-intercept-keymap-p (cdr e))))
            evil-mode-map-alist)))
      (should pos)
      (should (< pos first-non-intercept)))))
