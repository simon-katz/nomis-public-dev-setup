;;;; ---- Create an Emacs "installation" -- download packages ----

(defun load-file-relative-to-this-file (file-name)
  (let* ((directory (file-name-directory
                     (file-truename
                      (or load-file-name
                          (buffer-file-name))))))
    (load (concat directory file-name))))

(load-file-relative-to-this-file "emacs-installation/ensure-expected-emacs-version")
(load-file-relative-to-this-file "emacs-installation/set-up-package-stuff")

(package-refresh-contents)

(defvar my-packages
  ;; c.f. `package-selected-packages` in "emacs-installation/.emacs".
  ;; The strange space below is so that we have the same indentation as we
  ;; have for `package-selected-packages`.
  ' (aggressive-indent
     auto-complete
     auto-dim-other-buffers
     beacon
     cider
     cider-eval-sexp-fu
     cl-format
     cl-lib
     clj-refactor
     clojure-mode
     company
     counsel
     dash-functional
     diff-hl
     discover
     dumb-jump
     edn
     elisp-slime-nav
     exec-path-from-shell
     fill-column-indicator
     flycheck
     flycheck-clj-kondo
     flycheck-joker
     flycheck-kotlin
     fuzzy
     gnu-elpa-keyring-update
     helm
     highlight-indentation
     ht
     hydra
     ido-completing-read+
     ido-vertical-mode
     iedit
     inflections
     js2-mode
     json-mode
     kaocha-runner
     key-chord
     keycast
     kotlin-mode
     loccur
     logview
     lsp-mode
     lsp-treemacs
     lsp-ui
     lua-mode
     magit
     markdown-mode
     multiple-cursors
     nav-flash
     org
     org-bullets
     paredit
     parseedn
     paxedit
     pos-tip
     projectile
     queue
     rainbow-delimiters
     rainbow-blocks
     saveplace
     sidecar-locals
     smartrep
     smex
     spinner
     sqlup-mode
     terraform-mode
     tldr
     treepy
     undo-tree
     vterm
     which-key
     yafolding
     yaml-mode
     yasnippet
     zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
