;;; $DOOMDIR/modules/ext-lang/toml/config.el -*- lexical-binding: t; -*-

;; Emacs 30's `toml-ts-mode' recognizes # comments through tree-sitter but
;; leaves the standard comment variables unset. Doom's comment-aware
;; `evil-join' needs them when joining adjacent comment lines.
(defun +toml/comment-delimiters-h ()
  "Provide TOML line-comment metadata missing from `toml-ts-mode'."
  (setq-local comment-start "#"
              comment-start-skip "#+\\s-*"))

(add-hook 'toml-ts-mode-hook #'+toml/comment-delimiters-h)
