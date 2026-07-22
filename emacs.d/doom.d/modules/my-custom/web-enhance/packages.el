;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/my-custom/web-enhance/packages.el

;; emacs-os/embr.el - a web browser INSIDE emacs (headless Chromium rendered
;; into a buffer via CDP screencast). it ships .py/.sh and native C next to the
;; elisp, so the recipe has to pull those files too.
(package! embr
  :recipe (:host github :repo "emacs-os/embr.el"
           :files ("*.el" "*.py" "*.sh" "native/*.c" "native/Makefile")))
