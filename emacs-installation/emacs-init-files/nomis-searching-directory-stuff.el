;;;; Init stuff -- Searching -- directory stuff

;;;; ___________________________________________________________________________

(defvar log-dir-names '("log"
                        "logs"))

(defvar nomis/grep/global-ignored-directories
  (append log-dir-names
          '(".emacs-backups"
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
            ;; Instead of adding stuff here, consider defining
            ;; `nomis/grep/local-ignored-directories` in a .dir-locals file.
            )))

(defvar nomis/grep/local-ignored-directories '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep/ignore-overridden/builtin/directories '())
(defvar nomis/grep/ignore-overridden/global/directories '())
(defvar nomis/grep/ignore-overridden/local/directories '())

(defvar -nomis/grep/ignored/all/directories-vars
  '(grep-find-ignored-directories
    nomis/grep/global-ignored-directories
    nomis/grep/local-ignored-directories))

(defvar -nomis/grep/ignore-overridden/all/directories-vars
  '(nomis/grep/ignore-overridden/builtin/directories
    nomis/grep/ignore-overridden/global/directories
    nomis/grep/ignore-overridden/local/directories))

(defun -nomis/grep/ignored/all/directories ()
  (-mapcat #'symbol-value
           -nomis/grep/ignored/all/directories-vars))

(defun -nomis/grep/ignore-overridden/all/directories ()
  (-mapcat #'symbol-value
           -nomis/grep/ignore-overridden/all/directories-vars))

(defun nomis/grep/ignored-directories ()
  (let ((ignored
         (-nomis/grep/ignored/all/directories))
        (ignore-overridden
         (-nomis/grep/ignore-overridden/all/directories)))
    (cl-set-difference ignored ignore-overridden :test #'equal)))

(defun -nomis/grep/toggle-ignored-dirs (dir-names)
  (-nomis/grep/toggle-ignored-things t dir-names))

;;;; ___________________________________________________________________________

(defun nomis/toggle-grep-find-emacs.d ()
  (interactive)
  (-nomis/grep/toggle-ignored-dirs '(".emacs.d")))

(defun nomis/toggle-grep-find-log-files ()
  (interactive)
  (-nomis/grep/toggle-ignored-dirs log-dir-names))

(defvar -nomis/grep/toggle-dirs/last-name "")

(defun nomis/toggle-grep-ignored-dirs (dir-names)
  (interactive (list (list
                      (read-string "Dir name: "
                                   -nomis/grep/toggle-dirs/last-name
                                   'nomis/grep/toggle-dirs/history))))
  (setq -nomis/grep/toggle-dirs/last-name
        (first dir-names))
  (-nomis/grep/toggle-ignored-dirs dir-names))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/directories-vars ()
  (list (list nomis/grep/ignore-overridden/builtin/directories
              nomis/grep/ignore-overridden/global/directories
              nomis/grep/ignore-overridden/local/directories)
        (list grep-find-ignored-directories
              nomis/grep/global-ignored-directories
              nomis/grep/local-ignored-directories)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-directory-stuff)
