;;;; Init stuff -- Searching -- file stuff

;;;; ___________________________________________________________________________

(defvar nomis/grep/global-ignored-files
  '(".ido.last"
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
    ))

(defvar nomis/grep/local-ignored-files '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep/ignore-overridden/builtin/files '())
(defvar nomis/grep/ignore-overridden/global/files '())
(defvar nomis/grep/ignore-overridden/local/files '())

(defvar -nomis/grep/ignored/all/files-vars
  '(grep-find-ignored-files
    nomis/grep/global-ignored-files
    nomis/grep/local-ignored-files))

(defvar -nomis/grep/ignore-overridden/all/files-vars
  '(nomis/grep/ignore-overridden/builtin/files
    nomis/grep/ignore-overridden/global/files
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

(defvar -nomis/grep/toggle-files/last-name "")

(defun nomis/toggle-grep-ignored-files (file-names)
  (interactive (list (list
                      (read-string "File name: "
                                   -nomis/grep/toggle-files/last-name
                                   'nomis/grep/toggle-files/history))))
  (setq -nomis/grep/toggle-files/last-name
        (first file-names))
  (-nomis/grep/toggle-ignored-files file-names))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/files-vars ()
  (list (list nomis/grep/ignore-overridden/builtin/files
              nomis/grep/ignore-overridden/global/files
              nomis/grep/ignore-overridden/local/files)
        (list grep-find-ignored-files
              nomis/grep/global-ignored-files
              nomis/grep/local-ignored-files)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-file-stuff)
