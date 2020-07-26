;;;; Init stuff -- Searching -- directory stuff

;;;; ___________________________________________________________________________

(require 'nomis-searching-impl)

;;;; ___________________________________________________________________________
;;;; ---- Tailoring of built-in grep ignored things ----

(eval-after-load 'grep
  '(setq grep-find-ignored-directories ; Note that this is idempotent.
         (let ((v (append '(".emacs-backups"
                            ".worksheet"
                            "out"
                            "target"
                            ".repl"
                            "bundle"
                            ".idea"
                            ;; "labrepl*/public/javascripts/jquery.js"
                            ;; "emacs-configuration/nomis-addons/cygwin-mount.el"
                            "node_modules"
                            ".shadow-cljs"
                            ".emacs.d"
                            "emacs-configuration-pre-2018-06-upgrade-packages"
                            "clojure-for-the-brave-and-true/emacs-for-clojure-book1"
                            "cljs-runtime"
                            ".clj-kondo"
                            "log"
                            "logs"
                            ;; Instead of adding stuff here, consider defining
                            ;; `nomis/grep/local-ignored-directories` in a .dir-locals file.
                            )
                          grep-find-ignored-directories)))
           (cl-remove-duplicates v :from-end t :test #'equal))))

;;;; ___________________________________________________________________________

(defun nomis/grep/remove-ignored-directory (name)
  (interactive (-nomis/grep/remove-ignored/args :directories))
  (-nomis/grep/toggle-ignored/impl :directories name))

(defun nomis/grep/re-add-ignored-directory (name)
  (interactive (-nomis/grep/re-add-ignored/args :directories))
  (-nomis/grep/toggle-ignored/impl :directories name))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/directories-vars ()
  (list (list -nomis/grep/ignore-overridden/builtin/directories
              -nomis/grep/ignore-overridden/local/directories)
        (list grep-find-ignored-directories
              nomis/grep/local-ignored-directories)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-directory-stuff)
