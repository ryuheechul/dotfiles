;;; compat/neovim/whitespace.el -*- lexical-binding: t; -*-

;; show hidden/whitespace characters, same as
;; ../../../../../nvim/lua/boot/filetype.lua's `vim.opt.list' + `listchars'

;; both the top-level call and the after! block are needed:
;; - top-level: defvar preserves our value when whitespace.el loads later
;; - after!: overrides Doom's :config if it already ran, and removes the
;;   indent-mismatch hook (which needs the function to exist)

(defun +compat/whitespace-configure ()
  "Set whitespace-mode display to match nvim's listchars."
  (setq whitespace-style '(face tabs tab-mark trailing newline newline-mark))
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?> ?\t] [?\\ ?\t])
          (newline-mark ?\n [?↵ ?\n] [?$ ?\n]))))
;; enable on first real buffer
(add-hook 'doom-first-buffer-hook #'global-whitespace-mode)
;; set before whitespace.el loads — `defvar' inside whitespace.el won't
;; overwrite an existing value, so our settings survive
(+compat/whitespace-configure)
;; re-apply after whitespace loads to override Doom's :config which sets
;; `lines-tail' in whitespace-style (same red-text symptom as `lines')
(after! whitespace
  (+compat/whitespace-configure)
  ;; :editor whitespace's own per-buffer indent-mismatch highlighter already
  ;; no-ops itself whenever `global-whitespace-mode' is on (see its own guard
  ;; clause), which it always is here - remove it explicitly so that bypass
  ;; is documented instead of incidental
  (remove-hook 'after-change-major-mode-hook
               #'+whitespace-highlight-incorrect-indentation-h))

;; two things filetype.lua's listchars does that have no port here:
;; - `trail:·' marks trailing whitespace with a literal character; whitespace-mode has
;;   no per-trailing glyph substitution (only `space-mark', which would mark ALL spaces,
;;   not just trailing ones) - `trailing' above highlights it via face instead, same
;;   information conveyed, different visual treatment (background face, not a glyph)
;; - `extends'/`precedes' (line-wrap continuation arrows, only visible when nowrap) need
;;   nothing here - emacs already shows the same thing natively via fringe arrows
;;   whenever `truncate-lines' is set on a buffer

;; a subtle long-line hint, like nvim's colorcolumn but NON-aggressive: a thin
;; vertical rule in the `fill-column-indicator' face (a muted grey already, no
;; override needed), drawn by the display engine. the column is NOT hardcoded -
;; `display-fill-column-indicator-column' stays at its `t' default, which tracks
;; `fill-column', and the `editorconfig' module sets that from `max_line_length'
;; in ../../../../../.editorconfig (100) - the one source of truth, shared with
;; nvim's `colorcolumn'. the long-line TEXT is left undecorated on purpose: the
;; "special painting" past the column is whitespace-mode's `lines' style, and
;; the off-switch is simply leaving `lines' out of the whitespace-style above,
;; which this module does. prog/text/conf only, so dashboards/popups stay clean.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-fill-column-indicator-mode)
