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
  '(aggressive-indent
    auto-complete
    auto-dim-other-buffers
    cider
    cider-eval-sexp-fu
    cl-format
    cl-lib
    clojure-mode
    company
    dash-functional
    discover
    dumb-jump
    edn ; needed by clj-refactor, I think
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
    inflections ; needed by clj-refactor, I think
    js2-mode
    json-mode
    key-chord
    keycast
    kotlin-mode
    loccur
    logview
    lsp-mode
    lsp-treemacs
    lsp-ui
    magit
    markdown-mode
    multiple-cursors
    org-bullets
    paredit
    parseedn
    paxedit
    pos-tip
    projectile
    rainbow-delimiters
    saveplace
    smex
    terraform-mode
    treepy
    undo-tree
    vterm
    which-key
    yafolding
    yaml-mode
    yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
