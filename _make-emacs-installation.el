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

(defvar my-packages '(elisp-slime-nav
                      cl-lib
                      paredit
                      rainbow-delimiters
                      auto-complete
                      saveplace
                      fuzzy
                      ;; htmlize
                      pos-tip
                      magit
                      ido-ubiquitous
                      smex
                      ;; idle-highlight-mode -- see "emacs-installation/emacs-init-files/manually-installed/nomis-idle-highlight-mode.el"
                      ;; scala-mode2
                      undo-tree
                      ;; ac-nrepl
                      clojure-mode
                      cider
                      cider-eval-sexp-fu ; causes failure to download `highlight` -- I installed my old version manually
                      clj-refactor
                      cljr-helm
                      ;; align-cljlet -- you have cloned this repo yourself
                      exec-path-from-shell
                      js2-mode
                      ;; ac-js2
                      projectile
                      company
                      ;; rcirc-notify
                      markdown-mode
                      hydra
                      key-chord
                      yasnippet
                      multiple-cursors))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
