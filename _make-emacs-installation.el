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
                      workgroups ; Emacs 24.4 has something like this built in
                      fuzzy
                      ;; htmlize
                      pos-tip
                      magit
                      ido-ubiquitous
                      smex
                      idle-highlight-mode
                      scala-mode2
                      undo-tree
                      ;; ac-nrepl
                      clojure-mode
                      align-cljlet
                      exec-path-from-shell
                      js2-mode
                      ;; ac-js2
                      projectile
                      company
                      rcirc-notify
                      markdown-mode
                      hydra
                      cider-eval-sexp-fu))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
