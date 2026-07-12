;;; tests/format-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the executable half of `+format-with-eglot-excluded-modes' (see
;; ../modules/tools/lsp-support/config.el) and lua's use of it
;; (../modules/ext-lang/lua/config.el) - run via ../bin/run-tests

(require 'ert)

(ert-deftest format/excluded-mode-skips-eglot-format-mode ()
  "`+maybe-format-with-eglot-h' does not enable `+format-with-eglot-mode'
for a major-mode listed in `+format-with-eglot-excluded-modes'."
  (with-temp-buffer
    (let ((+format-with-eglot-excluded-modes '(fundamental-mode)))
      (+maybe-format-with-eglot-h)
      (should-not +format-with-eglot-mode))))

(ert-deftest format/non-excluded-mode-enables-eglot-format-mode ()
  "`+maybe-format-with-eglot-h' enables `+format-with-eglot-mode' for a
major-mode NOT listed in `+format-with-eglot-excluded-modes' - the
default eglot-owns-formatting behavior must survive the opt-out
mechanism for everyone who didn't ask for it."
  (with-temp-buffer
    (let ((+format-with-eglot-excluded-modes '(text-mode)))
      (unwind-protect
          (progn
            (+maybe-format-with-eglot-h)
            (should +format-with-eglot-mode))
        (+format-with-eglot-mode -1)))))

(ert-deftest format/lua-modes-opt-out-in-favor-of-stylua ()
  "lua-mode and lua-ts-mode register themselves in
`+format-with-eglot-excluded-modes' (ext-lang/lua/config.el) - lsp-support
itself knows nothing about lua, only the empty mechanism (loose
coupling: see the commit that split this out)."
  (should (memq 'lua-mode +format-with-eglot-excluded-modes))
  (should (memq 'lua-ts-mode +format-with-eglot-excluded-modes)))

(ert-deftest format/stylua-apheleia-command-searches-parent-directories ()
  "apheleia's stylua invocation includes --search-parent-directories so
it can find e.g. nvim/stylua.toml from a nested lua file - apheleia's
stock entry is just (\"stylua\" \"-\"), which only checks stylua's own
cwd and would silently fall back to stylua's built-in defaults instead
of this repo's actual settings. Matches nvim's own efm+stylua setup.
apheleia itself is lazily loaded (only via `doom-first-file-hook'), so
the override in ext-lang/lua/config.el's `after! apheleia' block won't
have run yet unless something already forced the load - hence the
explicit `require' here rather than relying on test order."
  (require 'apheleia)
  (should (member "--search-parent-directories" (alist-get 'stylua apheleia-formatters))))

;;; format-tests.el ends here
