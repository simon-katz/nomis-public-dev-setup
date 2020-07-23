;;;; Init stuff -- Searching -- directory stuff

;;;; ___________________________________________________________________________

(defvar log-dir-names '("log"
                        "logs"))

(defvar nomis/global-grep-find-ignored-directories
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
            ;; `nomis/local-grep-find-ignored-directories` in a .dir-locals file.
            )))

(defvar nomis/local-grep-find-ignored-directories '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep-find-ignored-directories/ignore-overridden '())
(defvar nomis/global-grep-find-ignored-directories/ignore-overridden '())
(defvar nomis/local-grep-find-ignored-directories/ignore-overridden '())

(defvar -nomis/all-grep-find-ignored-directories-vars/ignored
  '(grep-find-ignored-directories
    nomis/global-grep-find-ignored-directories
    nomis/local-grep-find-ignored-directories))

(defvar -nomis/all-grep-find-ignored-directories-vars/ignore-overridden
  '(nomis/grep-find-ignored-directories/ignore-overridden
    nomis/global-grep-find-ignored-directories/ignore-overridden
    nomis/local-grep-find-ignored-directories/ignore-overridden))

(defun -nomis/all-grep-find-ignored-directories/ignored ()
  (-mapcat #'symbol-value
           -nomis/all-grep-find-ignored-directories-vars/ignored))

(defun -nomis/all-grep-find-ignored-directories/ignore-overridden ()
  (-mapcat #'symbol-value
           -nomis/all-grep-find-ignored-directories-vars/ignore-overridden))

(defun nomis/all-grep-find-ignored-directories ()
  (let ((ignored
         (-nomis/all-grep-find-ignored-directories/ignored))
        (ignore-overridden
         (-nomis/all-grep-find-ignored-directories/ignore-overridden)))
    (cl-set-difference ignored ignore-overridden :test #'equal)))

(defun -nomis/toggle-grep-find-ignored-dirs (dir-names)
  (-nomis/toggle-grep-find-ignored-things t dir-names))

;;;; ___________________________________________________________________________

(defun nomis/toggle-grep-find-emacs.d ()
  (interactive)
  (-nomis/toggle-grep-find-ignored-dirs '(".emacs.d")))

(defun nomis/toggle-grep-find-log-files ()
  (interactive)
  (-nomis/toggle-grep-find-ignored-dirs log-dir-names))

(defvar -nomis/toggle-grep-find-files-last-dir-name "")

(defun nomis/toggle-grep-ignored-dirs (dir-names)
  (interactive (list (list
                      (read-string "Dir name: "
                                   -nomis/toggle-grep-find-files-last-dir-name
                                   'nomis/toggle-grep-find-files-dir-history))))
  (setq -nomis/toggle-grep-find-files-last-dir-name
        (first dir-names))
  (-nomis/toggle-grep-find-ignored-dirs dir-names))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored-directories-vars/ignored/for-debugging ()
  (list (list nomis/grep-find-ignored-directories/ignore-overridden
              nomis/global-grep-find-ignored-directories/ignore-overridden
              nomis/local-grep-find-ignored-directories/ignore-overridden)
        (list grep-find-ignored-directories
              nomis/global-grep-find-ignored-directories
              nomis/local-grep-find-ignored-directories)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-directory-stuff)
