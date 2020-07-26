;;;; Init stuff -- Searching impl.

;;;; ___________________________________________________________________________

(require 'dash)
(require 'cl)

;;;; ___________________________________________________________________________
;;;; ---- grep vars ----

(defvar nomis/grep/local-ignored-directories '()) ; set this in .dir-locals.el
(defvar nomis/grep/local-ignored-files '()) ; set this in .dir-locals.el

;;;; We don't want to set directory-local variables, because the behaviour isn't
;;;; easy to understand. So instead have global xxxx/ignore-overridden variables.

(defvar -nomis/grep/ignore-overridden/builtin/directories '())
(defvar -nomis/grep/ignore-overridden/local/directories '())
(defvar -nomis/grep/ignore-overridden/builtin/files '())
(defvar -nomis/grep/ignore-overridden/local/files '())

(defvar -nomis/grep/recent-names/directories '())
(defvar -nomis/grep/recent-names/files '())

;;;; ___________________________________________________________________________
;;;; ---- Strings ----

(defun -nomis/grep/directory-or-file-string (kind)
  (case kind
    (:directories "directory")
    (:files       "file")))

(defun -nomis/grep/directories-or-files-string (kind)
  (case kind
    (:directories "directories")
    (:files       "files")))

;;;; ___________________________________________________________________________
;;;; ---- grep var access ----

(defun -nomis/grep/ignored/all/vars (kind)
  (case kind
    (:directories '(nomis/grep/local-ignored-directories
                    grep-find-ignored-directories))
    (:files       '(nomis/grep/local-ignored-files
                    grep-find-ignored-files))))

(defun -nomis/grep/ignore-overridden/all/vars (kind)
  (case kind
    (:directories '(-nomis/grep/ignore-overridden/builtin/directories
                    -nomis/grep/ignore-overridden/local/directories))
    (:files       '(-nomis/grep/ignore-overridden/builtin/files
                    -nomis/grep/ignore-overridden/local/files))))

(defun -nomis/grep/recent-names (kind)
  (case kind
    (:directories -nomis/grep/recent-names/directories)
    (:files       -nomis/grep/recent-names/files)))

(defun -nomis/grep/set-recent-names (kind v)
  (case kind
    (:directories (setq -nomis/grep/recent-names/directories v))
    (:files       (setq -nomis/grep/recent-names/files v))))

;;;; ___________________________________________________________________________
;;;; ---- grep logic ----

(defun -nomis/grep/history-list-name (kind)
  (case kind
    (:directories 'nomis/grep/history-list-name/directories)
    (:files       'nomis/grep/history-list-name/files)))

(defun -nomis/grep/ignored/all (kind)
  (-mapcat #'symbol-value
           (-nomis/grep/ignored/all/vars kind)))

(defun -nomis/grep/ignore-overridden/all (kind)
  (-mapcat #'symbol-value
           (-nomis/grep/ignore-overridden/all/vars kind)))

(defun nomis/grep/ignored-things (kind)
  (let ((ignored           (-nomis/grep/ignored/all kind))
        (ignore-overridden (-nomis/grep/ignore-overridden/all kind)))
    (cl-set-difference ignored
                       ignore-overridden
                       :test #'equal)))

(defun -nomis/grep/remove-ignored/args (kind)
  (let* ((options
          (append (cl-set-difference
                   (-nomis/grep/recent-names kind)
                   (-nomis/grep/ignore-overridden/all kind)
                   :test #'equal)
                  (cl-set-difference
                   (nomis/grep/ignored-things kind)
                   (-nomis/grep/recent-names kind)
                   :test #'equal)))
         (_
          (when (null options)
            (error (format "No %s to remove"
                           (-nomis/grep/directories-or-files-string kind)))))
         (s (ido-completing-read
             (format "%s name to remove from ignored: "
                     (s-capitalize (-nomis/grep/directory-or-file-string kind)))
             options ; annotated-options
             nil
             t
             nil
             (-nomis/grep/history-list-name kind))))
    (set-text-properties 0 (length s) nil s)
    (list s)))

(defun -nomis/grep/re-add-ignored/args (kind)
  (let* ((options
          (append (cl-set-difference
                   (-nomis/grep/recent-names kind)
                   (nomis/grep/ignored-things kind)
                   :test #'equal)
                  (cl-set-difference
                   (-nomis/grep/ignore-overridden/all kind)
                   (-nomis/grep/recent-names kind)
                   :test #'equal)))
         (_
          (when (null options)
            (error (format "No %s to re-add"
                           (-nomis/grep/directories-or-files-string kind)))))
         (s (ido-completing-read
             (format "%s name to re-add to ignored: "
                     (s-capitalize (-nomis/grep/directory-or-file-string kind)))
             options ; annotated-options
             nil
             t
             nil
             (-nomis/grep/history-list-name kind))))
    (set-text-properties 0 (length s) nil s)
    (list s)))

(defun -nomis/grep/toggle-ignored-things (kind names) ; TODO This is over-complicated for what you now need.
  (cl-flet* ((currently-ignored?
              ()
              (member (first names)
                      (nomis/grep/ignored-things kind)))
             (change-state
              (ignore?)
              (cl-loop
               for v1 in (-nomis/grep/ignore-overridden/all/vars kind)
               as  v2 in (-nomis/grep/ignored/all/vars kind)
               do (cl-loop for name in names
                           when (member name (symbol-value v2))
                           do   (set v1
                                     (let ((v (symbol-value v1)))
                                       (if ignore?
                                           (remove name v)
                                         (cons name v))))))
              (message "%s %S -- NOTE: THIS WILL APPLY ONLY TO NEW GREP BUFFERS"
                       (let* ((directories-or-files-string
                               (-nomis/grep/directories-or-files-string kind)))
                         (if ignore?
                             (format "Ignoring the following %s:"
                                     directories-or-files-string)
                           (format "Not ignoring the following %s:"
                                   directories-or-files-string)))
                       names)))
    (if (currently-ignored?)
        (change-state nil)
      (progn
        (cl-loop
         for name in names
         unless (member name
                        (-nomis/grep/ignored/all kind))
         do (error "%s '%s' is not ignored, so cannot override the ignore"
                   (s-capitalize (-nomis/grep/directory-or-file-string kind))
                   name))
        (change-state t)))))

(defun -nomis/grep/toggle-ignored/impl (kind name)
  (-nomis/grep/set-recent-names kind
                                (-take 50 (cons name
                                                (remove name
                                                        (-nomis/grep/recent-names kind)))))
  (-nomis/grep/toggle-ignored-things kind (list name)))

;;;; ___________________________________________________________________________
;;;; Hack grep commands with advice

(defun with-augmented-grep-find-ignored-things* (f)
  (let* ((grep-find-ignored-directories (nomis/grep/ignored-things :directories))
         (grep-find-ignored-files       (nomis/grep/ignored-things :files)))
    (funcall f)))

(defmacro with-augmented-grep-find-ignored-things (options &rest body)
  (declare (indent 1))
  `(with-augmented-grep-find-ignored-things* (lambda () ,@body)))

(advice-add 'rgrep-default-command
            :around
            (lambda (orig-fun &rest args)
              (with-augmented-grep-find-ignored-things ()
                (apply orig-fun args)))
            '((name . nomis/augment-grep-find-ignored-things)))

(advice-add 'projectile-rgrep-default-command
            :around
            (lambda (orig-fun &rest args)
              (with-augmented-grep-find-ignored-things ()
                (apply orig-fun args)))
            '((name . nomis/augment-grep-find-ignored-things)))

;;;;___________________________________________________________________________

(provide 'nomis-searching-impl)
