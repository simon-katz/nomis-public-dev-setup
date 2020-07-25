;;;; Init stuff -- Searching -- directory stuff

;;;; ___________________________________________________________________________

(require 'dash)
(require 'cl)

(defvar log-dir-names '("log"
                        "logs"))

(eval-after-load 'grep
  '(setq grep-find-ignored-directories ; Note that this is idempotent.
         (let ((v (append log-dir-names
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
                            )
                          grep-find-ignored-directories)))
           (cl-remove-duplicates v :from-end t))))

(defvar nomis/grep/local-ignored-directories '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar nomis/grep/ignore-overridden/builtin/directories '())
(defvar nomis/grep/ignore-overridden/local/directories '())

(defvar -nomis/grep/ignored/all/directories-vars
  '(nomis/grep/local-ignored-directories
    grep-find-ignored-directories))

(defvar -nomis/grep/ignore-overridden/all/directories-vars
  '(nomis/grep/ignore-overridden/builtin/directories
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

(defvar -nomis/grep/toggle/last-names/dirs '())

(defun nomis/remove-grep-ignored-dir (dir-name)
  (interactive (let* ((options
                       (append (cl-set-difference
                                -nomis/grep/toggle/last-names/dirs
                                (-nomis/grep/ignore-overridden/all/directories)
                                :test #'equal)
                               (cl-set-difference
                                (nomis/grep/ignored-directories)
                                -nomis/grep/toggle/last-names/dirs
                                :test #'equal)))
                      (_
                       (when (null options)
                         (error "Nothing to remove")))
                      (s (ido-completing-read
                          "Dir name to remove from ignored: "
                          options ; annotated-options
                          nil
                          t
                          nil
                          'nomis/grep/toggle-dirs/history)))
                 (set-text-properties 0 (length s) nil s)
                 (list s)))
  (setq -nomis/grep/toggle/last-names/dirs
        (-take 50 (cons dir-name
                        (remove dir-name
                                -nomis/grep/toggle/last-names/dirs))))
  (-nomis/grep/toggle-ignored-dirs (list dir-name)))

(defun nomis/re-add-grep-ignored-dir (dir-name)
  (interactive (let* ((options
                       (append (cl-set-difference
                                -nomis/grep/toggle/last-names/dirs
                                (nomis/grep/ignored-directories)
                                :test #'equal)
                               (cl-set-difference
                                (-nomis/grep/ignore-overridden/all/directories)
                                -nomis/grep/toggle/last-names/dirs
                                :test #'equal)))
                      (_
                       (when (null options)
                         (error "Nothing to re-add")))
                      (s (ido-completing-read
                          "Dir name to re-add to ignored: "
                          options ; annotated-options
                          nil
                          t
                          nil
                          'nomis/grep/toggle-dirs/history)))
                 (set-text-properties 0 (length s) nil s)
                 (list s)))
  (setq -nomis/grep/toggle/last-names/dirs
        (-take 50 (cons dir-name
                        (remove dir-name
                                -nomis/grep/toggle/last-names/dirs))))
  (-nomis/grep/toggle-ignored-dirs (list dir-name)))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for grep -- debugging ----

(defun -nomis/all-grep-find-ignored/ignored/for-debugging/directories-vars ()
  (list (list nomis/grep/ignore-overridden/builtin/directories
              nomis/grep/ignore-overridden/local/directories)
        (list grep-find-ignored-directories
              nomis/grep/local-ignored-directories)))

;;;; ___________________________________________________________________________

(provide 'nomis-searching-directory-stuff)
