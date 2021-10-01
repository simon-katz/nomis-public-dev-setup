;;;; Init stuff -- nomis-revert --  -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'nomis-msg)
(require 'ido)

;;;; ___________________________________________________________________________
;;;; ---- Various utilities ----

(defun nomis/prompt-using-value-string-pairs (prompt
                                              value-string-pairs
                                              &optional
                                              history-list-name)
  (let* ((s (ido-completing-read prompt
                                 (-map #'second value-string-pairs)
                                 nil
                                 t
                                 nil
                                 history-list-name)))
    (first (-find (lambda (pair) (equal (second pair) s))
                  value-string-pairs))))

(defun nomis/vc-buffer-in-current-repo? (b)
  (case 2
    (1 (magit-auto-revert-repository-buffer-p b))
    (2 (let* ((vc-root-dir (nomis/dirtree/vc-root-dir)))
         (when (null vc-root-dir)
           (beep)
           (error "Not in a repository"))
         (s-starts-with? vc-root-dir
                         (buffer-file-name b))))))

(defun nomis/-vc-make/buffer-in-current-repo?-fun ()
  (let* ((this-buffer (current-buffer)))
    (lambda (b)
      (with-current-buffer this-buffer
        (nomis/vc-buffer-in-current-repo? b)))))

(defun nomis/find-buffers (&rest buffer-predicates)
  (-filter (lambda (b)
             (with-current-buffer b
               (-every? (lambda (pred) (funcall pred b))
                        buffer-predicates)))
           (buffer-list)))

(defun nomis/with-cleanup-on-non-local-exit/fun (f cleanup-f)
  (let* ((non-local-exit? t))
    (unwind-protect
        (prog1 (funcall f)
          (setq non-local-exit? nil))
      (when non-local-exit?
        (funcall cleanup-f)))))

(defmacro nomis/with-cleanup-on-non-local-exit (bodyform &rest cleanup-forms)
  (declare (indent 1))
  `(nomis/with-cleanup-on-non-local-exit/fun (lambda () ,bodyform)
                                             (lambda () ,@cleanup-forms)))

(defun nomis/y-or-n-p-reporting-non-local-exit (prompt)
  (nomis/with-cleanup-on-non-local-exit
      (y-or-n-p prompt)
    (nomis/message-no-disp "==== Quitting")))

(defun nomis/buffers->string-of-names (buffers)
  (cl-flet ((buffer-desc
             (b)
             (format "%s%s %s"
                     (if (verify-visited-file-modtime b) " " "!")
                     (if (not (buffer-modified-p b))     " " "*")
                     (buffer-file-name b))))
    (->> buffers
         (-sort (lambda (x y)
                  (s-less? (buffer-file-name x)
                           (buffer-file-name y))))
         (-map #'buffer-desc)
         (s-join "\n"))))

;;;; ___________________________________________________________________________
;;;; ---- Reverting buffers ----

(defun nomis/-revert-buffers* (buffers-to-revert preserve-modes?)
  (let* ((reverted-buffers '())
         (failures '()))
    (dolist (b buffers-to-revert)
      (with-current-buffer b
        (let* ((file-name (buffer-file-name b)))
          (condition-case e
              (progn (revert-buffer t t preserve-modes?)
                     (nomis/message-no-disp "Reverted %s" file-name)
                     (push b reverted-buffers))
            (error
             (push b failures)
             (nomis/message-no-disp "**** Failed to revert %s because: %s"
                                    file-name
                                    e))))))
    (message
     "%s"
     (let* ((lines
             (-concat
              (list (format "Reverted %s buffer(s)"
                            (length reverted-buffers)))
              (when failures
                (list "--------------------------------------------------------------------------------"
                      (format "Failed to revert the following %s buffer(s):"
                              (length failures))
                      (nomis/buffers->string-of-names failures)
                      (format "Failed to revert the above %s buffer(s):"
                              (length failures))))
              (list "See *Messages* buffer for details"))))
       (s-join "\n" lines)))
    (when failures (beep))))

(defun nomis/revert/-prompt-for-mode ()
  (nomis/prompt-using-value-string-pairs
   "Which buffers do you want to revert? "
   '((:out-of-sync-unmodified-only     "Out-of-sync unmodified buffers only")
     (:unmodified-buffers-only         "Unmodified buffers only")
     (:unmodified-only-if-no-modified  "Unmodified buffers only, only if there are no modified buffers")
     (:modified-buffers-only           "Modified buffers only")
     (:modified-and-unmodified-buffers "Modified and unmodified buffers"))
   'nomis/revert/-prompt-for-mode))

(defun nomis/revert/-prompt-for-restrict-to-current-repo ()
  (nomis/prompt-using-value-string-pairs
   "Restrict to current repository? "
   '((t   "Yes, restrict to buffers in the current repository")
     (nil "No, don't restrict"))
   'nomis/revert/-prompt-for-restrict-to-current-repo))

(defun nomis/revert/-prompt-for-preserve-modes ()
  (nomis/prompt-using-value-string-pairs
   "Preserve buffer modes? "
   '((t   "Yes, leave modes as they are")
     (nil "No, revert modes"))
   'nomis/revert/-prompt-for-preserve-modes))

(defun nomis/revert-buffers (no-confirm?
                             mode
                             only-current-repo?
                             preserve-modes?)
  "Revert buffers."
  (interactive (list current-prefix-arg
                     (nomis/revert/-prompt-for-mode)
                     (nomis/revert/-prompt-for-restrict-to-current-repo)
                     (nomis/revert/-prompt-for-preserve-modes)))
  (cl-multiple-value-bind (revert-modified-buffers?
                           revert-unmodified-buffers?
                           out-of-sync-buffers-only?
                           bail-out-when-modified-buffers?)
      (ecase mode
        (:out-of-sync-unmodified-only     '(nil t   t   nil))
        (:unmodified-buffers-only         '(nil t   nil nil))
        (:unmodified-only-if-no-modified  '(nil t   nil t))
        (:modified-buffers-only           '(t   nil nil nil))
        (:modified-and-unmodified-buffers '(t   t   nil nil)))
    (let* ((buffer-predicates
            (-concat (when only-current-repo?
                       (list (nomis/-vc-make/buffer-in-current-repo?-fun)))
                     (when out-of-sync-buffers-only?
                       (list (-compose #'not #'verify-visited-file-modtime)))))
           (buffers-to-consider (apply #'nomis/find-buffers
                                       #'buffer-file-name
                                       buffer-predicates))
           (modified-buffers (-filter #'buffer-modified-p
                                      buffers-to-consider)))
      (if (and bail-out-when-modified-buffers?
               modified-buffers)
          (progn
            (beep)
            (message "The following %s buffers are unsaved:\n%s\nThe above %s buffer(s) are unsaved.\nSave them or revert them first."
                     (length modified-buffers)
                     (nomis/buffers->string-of-names modified-buffers)
                     (length modified-buffers)))
        (let* ((unmodified-buffers (-filter (-compose #'not #'buffer-modified-p)
                                            buffers-to-consider))
               (buffers-to-revert (-concat (when revert-modified-buffers?
                                             modified-buffers)
                                           (when revert-unmodified-buffers?
                                             unmodified-buffers))))
          (if (null buffers-to-revert)
              (message "There are no buffers to revert")
            (when (or no-confirm?
                      (nomis/y-or-n-p-reporting-non-local-exit
                       (format
                        "%s\nAre you sure you want to revert the above %s buffer(s)?"
                        (nomis/buffers->string-of-names buffers-to-revert)
                        (length buffers-to-revert))))
              (nomis/-revert-buffers* buffers-to-revert
                                      preserve-modes?))))))))

;;;; ___________________________________________________________________________

(provide 'nomis-revert)
