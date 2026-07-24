;;; compat/neovim/spell.el -*- lexical-binding: t; -*-

;; nvim has global `spell`; doom's :checkers spell already covers prose
;; modes (org, markdown, ...) - extend to code via flyspell-prog-mode,
;; whose narrow scope keeps the overhead negligible
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; ... and match nvim's scope while at it: its treesitter @spell captures
;; check comments, NOT plain strings (identifiers/paths/format specifiers
;; there are all false positives) - flyspell-prog-mode does strings by
;; default, so drop that; docstrings stay, they are prose
(after! flyspell
  (setq flyspell-prog-text-faces
        (remq 'font-lock-string-face flyspell-prog-text-faces))
  ;; eglot semantic tokens turn the `face' text property into a LIST
  ;; (e.g. (eglot-semantic-comment font-lock-comment-face)) and the stock
  ;; predicate's bare memq cannot see into it - flyspell then silently
  ;; skips every semantically-fontified comment word (typos only got
  ;; flagged when the check won the race against the async tokens)
  (defadvice! +neovim/flyspell-prog-verify-face-lists-a ()
    :override #'flyspell-generic-progmode-verify
    (unless (eql (point) (point-min))
      (let ((f (get-text-property (1- (point)) 'face)))
        (cl-intersection (ensure-list f) flyspell-prog-text-faces))))
  ;; the stock faces hide behind a `(supports :underline (:style wave))'
  ;; display guard, so on terminals that don't advertise styled underlines
  ;; they render as NOTHING at all (invisible flags). replace with an
  ;; unconditional wave that leaves the text color alone - a hint, not an
  ;; alarm - degrading to a plain underline (never to nothing) where the
  ;; terminal can't style underlines. yellow3/cyan3 are the exact RGB the
  ;; terminal palette renders "yellow"/"cyan" as, so GUI and TUI frames
  ;; come out identical; cyan for duplicates over the stock DarkOrange
  ;; because orange blurs into the yellow of misspellings.
  ;; face-OVERRIDE-spec, not defface spec: doom-themes define these faces
  ;; too and a theme beats a defface, but nothing beats the override spec
  (face-spec-set 'flyspell-incorrect
                 '((t :underline (:style wave :color "yellow3")))
                 'face-override-spec)
  (face-spec-set 'flyspell-duplicate
                 '((t :underline (:style wave :color "cyan3")))
                 'face-override-spec))
