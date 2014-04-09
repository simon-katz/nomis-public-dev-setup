;;;; ---- Create an Emacs "installation" -- download packages ----

(load (concat (file-name-directory load-file-name)
              "emacs-installation/common-install-and-tailor-stuff.el"))

(package-refresh-contents)

(defvar my-packages '(elisp-slime-nav
                      cl-lib
                      paredit
                      rainbow-delimiters
                      auto-complete
                      saveplace
                      workgroups
                      fuzzy
                      htmlize
                      pos-tip
                      magit
                      ido-ubiquitous
                      smex
                      idle-highlight-mode
                      scala-mode2
                      undo-tree
                      cider
                      ac-nrepl
                      clojure-mode
                      clojure-test-mode
                      clj-refactor
                      align-cljlet
                      js2-mode
                      ac-js2))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
