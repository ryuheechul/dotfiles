;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/my-custom/lab/packages.el

(when IS-LINUX
  (package! eaf :recipe (:host github
                         :repo "emacs-eaf/emacs-application-framework"
                         :files ("*")
                         :build (:not compile))))
