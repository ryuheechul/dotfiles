;;; compat/neovim/completion.el -*- lexical-binding: t; -*-

;; fuzzy match completions!!!
(after! orderless ;; orderless is cool and all but I'm used to fuzzy matching more ...
  (defun hotfuzz-setup ()
    "Set up `hotfuzz'."
    (setq completion-ignore-case t)
    (unless (memq 'hotfuzz completion-styles)
      (push 'hotfuzz completion-styles)))

  ;; to overtake orderless set by :completion vertico
  (use-package! hotfuzz
    :config
    (hotfuzz-setup)))

;; nvim's TAB completes a lone match (nvim-cmp, ../../../../../nvim/lua/plugins/config/completion.lua);
;; doom binds corfu's TAB to `corfu-next' (config/default's `cmds-tab'), which
;; with `corfu-preselect 'first' + `corfu-cycle t' just re-selects the only
;; candidate - looks like a no-op. cycle when there are several, insert when
;; there is one. a lone matched DIRECTORY still keeps offering its contents
;; like nvim's path completion, but that continuation is now driven by the
;; cape-file exit-function below (which also handles RET), so a plain
;; `corfu-insert' is enough here - no need for `corfu-complete'.
;;
;; the fresh-popup branch fixes a "TAB skips the first candidate" bug (most
;; visible on ./ path completion): `corfu-preselect 'first' silently selects
;; candidate 0, but corfu previews a candidate only when point differs from
;; the preselect (corfu--preview-current-p), so candidate 0 shows as
;; unselected/unfilled - and a plain `corfu-next' then jumps straight to
;; candidate 1, skipping 0. nvim's completion.lua uses `menuone,noselect', so
;; the first TAB lands ON the first candidate; match that by detaching the
;; preselect on that first press so candidate 0 becomes previewable. RET keeps
;; inserting candidate 0 (still index >= 0, so no "embarrassing newline"), and
;; later presses cycle normally.
(defun +neovim/corfu-tab ()
  "TAB in the corfu popup: cycle when several candidates, complete a lone one.
On a fresh popup, land on and preview the first candidate instead of skipping
it (see the block comment above)."
  (interactive)
  (cond
   ((= corfu--total 1) (corfu-insert))
   ;; reaches into corfu internals (corfu--index/--preselect/--goto): the
   ;; public API has no "preview the preselected candidate" lever.
   ((and (>= corfu--index 0) (= corfu--index corfu--preselect))
    (setq corfu--preselect -1)
    (corfu--goto 0))
   (t (corfu-next))))

;; to make this to resemble the completion experience from Neovim
(after! corfu
  ;; let's not make the enter key embarassing when there is no candidate
  (setq corfu-preselect 'first)
  ;; bind BOTH key symbols - "TAB" (TTY) and [tab] (GUI), as doom does - this
  ;; module has been bitten by GUI/TTY key-symbol splits before. runs on corfu
  ;; load, after doom's config/default `cmds-tab' binding, so it wins.
  (map! :map corfu-map
        :gi "TAB" #'+neovim/corfu-tab
        :gi [tab] #'+neovim/corfu-tab)
  ;; corfu quits after any command not in `corfu-continue-commands' (nor
  ;; matching its built-in `\\`corfu-' regex) - our wrapper name matches
  ;; neither, so without this the popup dies after a single press: it cycles
  ;; once, then `corfu--post-command' calls `corfu-quit'. doom registers its
  ;; own +corfu/* wrappers here for exactly this reason.
  (add-to-list 'corfu-continue-commands #'+neovim/corfu-tab))

;; ./ and ../ path completion everywhere, like nvim's cmp-path source
;; (../../../../../nvim/lua/plugins/config/completion.lua lists it
;; globally): doom's corfu module wires `cape-file' into prog-mode only -
;; extend it to the text/conf families (org/markdown derive from
;; text-mode, so they're covered)
(add-hook! '(text-mode-hook conf-mode-hook) #'+corfu-add-cape-file-h)

;; keep offering choices as you descend into a directory, like nvim's
;; cmp-path: cape-file ships no `:exit-function', so after you complete a
;; `dir/' corfu just stops and you have to re-invoke completion by hand.
;; inject an exit-function that re-triggers `completion-at-point' whenever the
;; inserted candidate ends in `/' (a directory) - fires for RET (a dir picked
;; from several) and for TAB (a lone dir, via `corfu-insert' above) alike, and
;; recurses naturally as you go deeper. guarded so a future cape-file that
;; grows its own exit-function wins.
(defun +neovim/cape-file-continue-in-dir-a (result)
  "Advice: make cape-file re-open completion after inserting a directory."
  (when (and (consp result) (number-or-marker-p (car-safe result)))
    (let ((tail (nthcdr 3 result)))
      (unless (plist-get tail :exit-function)
        (setf (nthcdr 3 result)
              (plist-put tail :exit-function
                         (lambda (str status)
                           (when (and (eq status 'finished)
                                      (string-suffix-p "/" str))
                             (completion-at-point))))))))
  result)
(advice-add #'cape-file :filter-return #'+neovim/cape-file-continue-in-dir-a)

;; nvim's `buffer' cmp source (completion.lua): complete words already in the
;; buffer (type "app" -> "apple"). doom gates this behind the corfu module's
;; +dabbrev flag, which isn't set here, so wire `cape-dabbrev' in directly.
;; depth 20 keeps it a low-priority fallback so real capfs (lsp, file) are
;; offered first, matching nvim listing `buffer' after lsp/path among its
;; sources. corfu-auto's 2-char prefix triggers it earlier than nvim's
;; keyword_length 4, which is the more responsive feel the buffer source is
;; wanted for anyway.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  (defun +neovim/add-cape-dabbrev-h ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))

;; nvim-cmp shows every source at once; emacs' capf list is a CHAIN where the
;; first capf that returns matches wins, so in prose buffers the spell source
;; (`ispell-completion-at-point', text-mode only) shadows `cape-dabbrev' - type
;; "curr" next to a "current-base16" and you see only dictionary words until
;; the word is finished, then the buffer word finally appears. merge the two
;; with `cape-capf-super' so spelling AND buffer words show together like nvim.
;; added ahead (depth -1) of the standalone spell/dabbrev entries so it wins
;; and they never shadow it; `cape-file' (depth -10) still wins first for real
;; paths. text-mode only: that is where the spell capf runs (prog-mode's
;; flyspell only flags, conf-mode has no spell capf to shadow dabbrev).
;;
;; ranking buffer words ABOVE the dictionary needs a real sort override, not
;; just source order: the completion style (hotfuzz) sorts candidates by fuzzy
;; score, and cape-super's own `:display-sort-function' is `identity' (keeps
;; that score order), so a long buffer word like "current-base16" sinks below
;; the dozens of short dictionary words that "bury others". `corfu--sort-
;; function' consults `corfu-sort-override-function' FIRST (before any
;; display-sort-function), so set that: partition buffer words to the front.
(defun +neovim/corfu-buffer-words-first (cands)
  "Sort CANDS so buffer words (the dabbrev source) rank above the rest.
A candidate counts as a buffer word if it appears verbatim as a token in the
buffer; `[[:alnum:]_-]+' spans the hyphenated identifiers dabbrev completes
\(e.g. current-base16). Stable within each group, so the style's own ordering
survives; a single buffer scan keeps it cheap even for large candidate sets."
  (let ((words (make-hash-table :test #'equal))
        in out)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:alnum:]_-]+" nil t)
        (puthash (match-string-no-properties 0) t words)))
    (dolist (c cands)
      (if (gethash c words) (push c in) (push c out)))
    (nconc (nreverse in) (nreverse out))))

(add-hook! 'text-mode-hook :append
  (defun +neovim/merge-spell-and-buffer-completion-h ()
    (add-hook 'completion-at-point-functions
              (cape-capf-super #'cape-dabbrev #'ispell-completion-at-point)
              -1 t)
    (setq-local corfu-sort-override-function #'+neovim/corfu-buffer-words-first)))
