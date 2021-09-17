;;;; Init stuff -- Searching -- directory stuff

;;;; ___________________________________________________________________________

(require 'nomis-searching-impl)

;;;; ___________________________________________________________________________
;;;; ---- Tailoring of built-in grep ignored things ----

(eval-after-load 'grep
  '(setq grep-find-ignored-directories ; Note that this is idempotent.
         (let ((v (append '(;; Instead of adding stuff here, consider defining
                            ;; `nomis/grep/local-ignored-directories` in a .dir-locals file.
                            ".cache"
                            ".cpcache"
                            ".emacs-backups"
                            ".emacs.d"
                            ".idea"
                            ".repl"
                            ".shadow-cljs"
                            ".worksheet"
                            "bootstrap-icons-1.1.0"
                            "bundle"
                            "cljs-runtime"
                            "clojure-for-the-brave-and-true/emacs-for-clojure-book1"
                            "emacs-configuration-pre-2018-06-upgrade-packages"
                            ;; "emacs-configuration/nomis-addons/cygwin-mount.el"
                            ;; "labrepl*/public/javascripts/jquery.js"
                            "log"
                            "logs"
                            "musimathics-errata_files"
                            "node_modules"
                            "out"
                            "out2"
                            "target"
                            )
                          grep-find-ignored-directories)))
           (cl-remove-duplicates v :from-end t :test #'equal))))

;;;; ___________________________________________________________________________

(defun nomis/grep/remove-ignored-directory (name)
  (interactive (-nomis/grep/remove-ignored/args :directories))
  (-nomis/grep/toggle-ignored :directories name))

(defun nomis/grep/re-add-ignored-directory (name)
  (interactive (-nomis/grep/re-add-ignored/args :directories))
  (-nomis/grep/toggle-ignored :directories name))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/directories-vars ()
  (list (list -nomis/grep/directories/with-overridden-ignore/builtin
              -nomis/grep/directories/with-overridden-ignore/local)
        (list grep-find-ignored-directories
              nomis/grep/local-ignored-directories)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-directory-stuff)
