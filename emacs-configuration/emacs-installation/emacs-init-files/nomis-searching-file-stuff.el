;;;; Init stuff -- Searching -- file stuff

;;;; ___________________________________________________________________________

(require 'nomis-searching-impl)

;;;; ___________________________________________________________________________
;;;; ---- Tailoring of built-in grep ignored things ----

(eval-after-load 'grep
  '(setq grep-find-ignored-files ; Note that this is idempotent.
         (let ((v (append '(".ido.last"
                            ".smex-items"
                            ;; ".jar"
                            ;; ".exe"
                            ".cider-repl-history"
                            ".lein-repl-history"
                            "*.iml"
                            "*.zip"
                            "figwheel_server.log"
                            "archive-contents"
                            ;; Instead of adding stuff here, consider defining
                            ;; `nomis/grep/local-ignored-files` in a .dir-locals file.
                            )
                          grep-find-ignored-files)))
           (cl-remove-duplicates v :from-end t :test #'equal))))

;;;; ___________________________________________________________________________

(defun nomis/grep/remove-ignored-file (name)
  (interactive (-nomis/grep/remove-ignored/args :files))
  (-nomis/grep/toggle-ignored :files name))

(defun nomis/grep/re-add-ignored-file (name)
  (interactive (-nomis/grep/re-add-ignored/args :files))
  (-nomis/grep/toggle-ignored :files name))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/files-vars ()
  (list (list -nomis/grep/files/with-overridden-ignore/builtin
              -nomis/grep/files/with-overridden-ignore/local)
        (list grep-find-ignored-files
              nomis/grep/local-ignored-files)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-file-stuff)
