;;;; Init stuff -- Searching -- file stuff

;;;; ___________________________________________________________________________

(defvar nomis/global-grep-find-ignored-files
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
    ;; `nomis/local-grep-find-ignored-files` in a .dir-locals file.
    ))

(defvar nomis/local-grep-find-ignored-files '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep-find-ignored-files/ignore-overridden '())
(defvar nomis/global-grep-find-ignored-files/ignore-overridden '())
(defvar nomis/local-grep-find-ignored-files/ignore-overridden '())

(defvar -nomis/all-grep-find-ignored-files-vars/ignored
  '(grep-find-ignored-files
    nomis/global-grep-find-ignored-files
    nomis/local-grep-find-ignored-files))

(defvar -nomis/all-grep-find-ignored-files-vars/ignore-overridden
  '(nomis/grep-find-ignored-files/ignore-overridden
    nomis/global-grep-find-ignored-files/ignore-overridden
    nomis/local-grep-find-ignored-files/ignore-overridden))

(defun -nomis/all-grep-find-ignored-files/ignored ()
  (-mapcat #'symbol-value
           -nomis/all-grep-find-ignored-files-vars/ignored))

(defun -nomis/all-grep-find-ignored-files/ignore-overridden ()
  (-mapcat #'symbol-value
           -nomis/all-grep-find-ignored-files-vars/ignore-overridden))

(defun nomis/all-grep-find-ignored-files ()
  (let ((ignored
         (-nomis/all-grep-find-ignored-files/ignored))
        (ignore-overridden
         (-nomis/all-grep-find-ignored-files/ignore-overridden)))
    (cl-set-difference ignored ignore-overridden :test #'equal)))

(defun -nomis/toggle-grep-find-ignored-files (file-names)
  (-nomis/toggle-grep-find-ignored-things nil file-names))

;;;; ___________________________________________________________________________

(defvar -nomis/toggle-grep-find-files-last-file-name "")

(defun nomis/toggle-grep-ignored-files (file-names)
  (interactive (list (list
                      (read-string "File name: "
                                   -nomis/toggle-grep-find-files-last-file-name
                                   'nomis/toggle-grep-find-files-file-history))))
  (setq -nomis/toggle-grep-find-files-last-file-name
        (first file-names))
  (-nomis/toggle-grep-find-ignored-files file-names))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored-files-vars/ignored/for-debugging ()
  (list (list nomis/grep-find-ignored-files/ignore-overridden
              nomis/global-grep-find-ignored-files/ignore-overridden
              nomis/local-grep-find-ignored-files/ignore-overridden)
        (list grep-find-ignored-files
              nomis/global-grep-find-ignored-files
              nomis/local-grep-find-ignored-files)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-file-stuff)
