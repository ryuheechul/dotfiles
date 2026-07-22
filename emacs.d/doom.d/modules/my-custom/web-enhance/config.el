;;; $DOOMDIR/modules/my-custom/web-enhance/config.el -*- lexical-binding: t; -*-

;; Browser integration, backend-agnostic in spirit the way ../term-enhance/ is
;; for terminals: the browser of the day is emacs-os/embr.el (headless Chromium
;; rendered into a buffer), but anything web-facing lives here and a different
;; backend (eww, xwidget-webkit, ...) would slot in the same seams.

;; embr: browse the web inside emacs. `embr-browser-engine' defaults to
;; 'cloakbrowser (closed-source anti-fingerprint patches); 'chromium is the
;; fully-open-source Playwright engine, and the README's config table has the
;; rest of the knobs. first run: M-x embr-install-or-update-cloakbrowser
;; (creates a python venv + downloads the browser), then M-x embr-browse.

(defun web-enhance/embr-doom-leader ()
  "Open the doom leader map (with which-key) from inside an embr buffer.
SPC is the vimium leader there, so this is hung off SPC SPC."
  (interactive)
  ;; which-key--show-keymap / which-key--hide-popup are private (double-dash),
  ;; but there's no public API for "show this arbitrary keymap and dismiss on
  ;; next command" — which-key-show-keymap is interactive-only and returns nil.
  ;; this pattern is common in doom configs.
  (which-key--show-keymap "SPC" doom-leader-map nil nil t)
  (set-transient-map doom-leader-map t #'which-key--hide-popup))

(use-package! embr
  :defer t
  ;; vimium (vim-style modal browsing) on in every browser buffer
  :hook (embr-mode . embr-vimium-mode)
  :init
  ;; embr forwards every key to the browser and vimium's leader is SPC, so evil
  ;; has to get out of the way - otherwise evil normal state grabs SPC/hjkl/f
  ;; before vimium can. emacs state hands every key straight through.
  (after! evil
    (evil-set-initial-state 'embr-mode 'emacs))
  :config
  ;; SPC is the vimium leader now, so the doom SPC leader is unreachable in the
  ;; browser. hang it off SPC SPC: the first SPC opens the vimium dispatch
  ;; (magit-style transient), a second SPC drops into the doom leader with
  ;; which-key, like bare SPC elsewhere. (M-SPC, doom's leader-alt-key, reaches
  ;; the leader too, in emacs state.)
  ;; guard: embr's transient isn't defined until first load (`:defer t'), so
  ;; re-evaluating this file without reloading embr would silently fail.
  (when (fboundp 'embr-vimium-dispatch)
    (transient-append-suffix 'embr-vimium-dispatch "?"
      '("SPC" "Doom leader" web-enhance/embr-doom-leader))))

;; make embr the emacs-wide browser so URLs everywhere (gx, org/help/magit
;; links, ...) open in it - `browse-url-browser-function' is the backend-neutral
;; seam this module is built around. left off by default: it also redirects
;; morevil's gx away from the system browser, so flip it on once you're sure
;; embr is where every link should land.
;; (setq browse-url-browser-function #'embr-browse)
;; (global-goto-address-mode 1)
