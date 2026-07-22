;;; tests/term-enhance-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; Terminal-specific editor bridge behavior from
;; ../modules/my-custom/term-enhance/editor-bridge.el. Run via ../bin/run-tests.

(require 'ert)

(ert-deftest term-enhance/bridge-paths-resolve-against-callers-tramp-prefix ()
  "A Tramp terminal's remote-side path retains its caller's remote prefix."
  (let ((default-directory "/ssh:user@somehost:/home/user/"))
    (should (equal (term-enhance/from-caller-fs "/home/user/x.txt")
                   "/ssh:user@somehost:/home/user/x.txt")))
  (let ((default-directory "/tmp/"))
    (should (equal (term-enhance/from-caller-fs "/tmp/x.txt") "/tmp/x.txt"))))
