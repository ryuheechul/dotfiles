;; -*- no-byte-compile: t; -*-
;;; tools/lsp-support/packages.el

;; `eglot' is builtin
;; installing `lsp-mode' is handled via dooemacs modeul =:tools lsp=

(when (modulep! :tools lsp +eglot)
  (package! eglot-booster
    :recipe (:host github
             :repo "jdtsmith/eglot-booster")))

(when (not (modulep! :tools lsp))
  (when (package! lsp-bridge
          :recipe (:host github
                   :repo "manateelazycat/lsp-bridge"
                   :branch "master"
                   :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                   ;; do not perform byte compilation or native compilation for lsp-bridge
                   :build (:not compile)))
    (package! markdown-mode)
    (package! yasnippet)))
