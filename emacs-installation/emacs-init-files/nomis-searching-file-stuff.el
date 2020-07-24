;;;; Init stuff -- Searching -- file stuff

;;;; ___________________________________________________________________________

(require 'dash)
(require 'cl)

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
           (cl-remove-duplicates v :from-end t))))

(defvar nomis/grep/local-ignored-files '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep/ignore-overridden/builtin/files '())
(defvar nomis/grep/ignore-overridden/local/files '())

(defvar -nomis/grep/ignored/all/files-vars
  '(nomis/grep/local-ignored-files
    grep-find-ignored-files))

(defvar -nomis/grep/ignore-overridden/all/files-vars
  '(nomis/grep/ignore-overridden/builtin/files
    nomis/grep/ignore-overridden/local/files))

(defun -nomis/grep/ignored/all/files ()
  (-mapcat #'symbol-value
           -nomis/grep/ignored/all/files-vars))

(defun -nomis/grep/ignore-overridden/all/files ()
  (-mapcat #'symbol-value
           -nomis/grep/ignore-overridden/all/files-vars))

(defun nomis/grep/ignored-files ()
  (let ((ignored
         (-nomis/grep/ignored/all/files))
        (ignore-overridden
         (-nomis/grep/ignore-overridden/all/files)))
    (cl-set-difference ignored ignore-overridden :test #'equal)))

(defun -nomis/grep/toggle-ignored-files (file-names)
  (-nomis/grep/toggle-ignored-things nil file-names))

;;;; ___________________________________________________________________________

(defvar -nomis/grep/toggle/last-names/files '())

(defun -nomis/grep/annotate-name/files (name)
  (-nomis/grep/annotate-name name
                             (-nomis/grep/ignore-overridden/all/files)))

(defun nomis/toggle-grep-ignored-files (file-name)
  (interactive (let* ((options
                       (append -nomis/grep/toggle/last-names/files
                               (cl-set-difference
                                (-nomis/grep/ignored/all/files)
                                -nomis/grep/toggle/last-names/files
                                :test #'equal)))
                      (annotated-options
                       (-map #'-nomis/grep/annotate-name/files
                             options))
                      (annotated-s (ido-completing-read
                                    "File name: "
                                    annotated-options
                                    nil
                                    t
                                    nil
                                    'nomis/grep/toggle-files/history))
                      (s (-nomis/grep/de-annotate-name annotated-s)))
                 (set-text-properties 0 (length s) nil s)
                 (list s)))
  (setq -nomis/grep/toggle/last-names/files
        (-take 50 (cons file-name
                        (remove file-name
                                -nomis/grep/toggle/last-names/files))))
  (-nomis/grep/toggle-ignored-files (list file-name)))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/files-vars ()
  (list (list nomis/grep/ignore-overridden/builtin/files
              nomis/grep/ignore-overridden/local/files)
        (list grep-find-ignored-files
              nomis/grep/local-ignored-files)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-file-stuff)
