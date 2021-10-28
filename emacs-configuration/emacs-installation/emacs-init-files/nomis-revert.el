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
  (let* ((value-string-pairs-2 (cl-loop
                                for i = 1 then (1+ i)
                                for (v s) in value-string-pairs
                                collect (list v (format "%s %s" i s))))
         (s (ido-completing-read prompt
                                 (-map #'second value-string-pairs-2)
                                 nil
                                 t
                                 nil
                                 history-list-name)))
    (first (-find (lambda (pair) (equal (second pair) s))
                  value-string-pairs-2))))

(defun nomis/vc-buffer-in-current-repo? (b)
  (let* ((vc-root-dir (nomis/dirtree/vc-root-dir)))
    (when (null vc-root-dir)
      (beep)
      (error "Not in a repository"))
    (s-starts-with? vc-root-dir
                    (buffer-file-name b))))

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
       (s-join "\n" lines)))))

(defconst nomis/revert/-mode-pairs
  '((:unmodified-only-if-out-of-sync  "Unmodified buffers only, and only out-of-sync ones")
    (:unmodified-buffers-only         "Unmodified buffers only")
    (:modified-buffers-only           "Modified buffers only")
    (:modified-and-unmodified-buffers "Modified and unmodified buffers")))

(defun nomis/revert/-mode->text (mode)
  (cl-loop for (mode-2 text) in nomis/revert/-mode-pairs
           when (eql mode mode-2)
           return text))

(defun nomis/revert/-prompt-for-mode ()
  (nomis/prompt-using-value-string-pairs
   "Which buffers do you want to revert? "
   nomis/revert/-mode-pairs
   'nomis/revert/-prompt-for-mode))

(defun nomis/revert/-prompt-for-restrict-to-current-repo ()
  (nomis/prompt-using-value-string-pairs
   "Restrict to current repository? "
   (if (nomis/dirtree/vc-root-dir)
       '((t   "Yes, restrict to buffers in the current repository")
         (nil "No, don't restrict"))
     '((nil "No, don't restrict (only option because not in a repository)")))
   'nomis/revert/-prompt-for-restrict-to-current-repo))

(defun nomis/revert/-prompt-for-preserve-modes ()
  (nomis/prompt-using-value-string-pairs
   "Preserve buffer modes? "
   '((t   "Yes, leave modes as they are")
     (nil "No, revert modes"))
   'nomis/revert/-prompt-for-preserve-modes))

(defun nomis/revert-buffers* (desc
                              only-current-repo?
                              revert-unmodified-buffers?
                              revert-modified-buffers?
                              out-of-sync-buffers-only?
                              tell-user-about-modified-non-reverting-buffers?)
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
    (let* ((unmodified-buffers (-filter (-compose #'not #'buffer-modified-p)
                                        buffers-to-consider))
           (buffers-to-revert (-concat (when revert-modified-buffers?
                                         modified-buffers)
                                       (when revert-unmodified-buffers?
                                         unmodified-buffers))))
      (if (null buffers-to-revert)
          (message "There are no buffers to revert (%s)"
                   desc)
        (when (or (null modified-buffers)
                  (not tell-user-about-modified-non-reverting-buffers?)
                  (nomis/y-or-n-p-reporting-non-local-exit
                   (format "The following %s buffers are unsaved:\n%s\nThe above %s buffer(s) are unsaved.\nAre you sure you want to revert other buffers?"
                           (length modified-buffers)
                           (nomis/buffers->string-of-names modified-buffers)
                           (length modified-buffers)"Some buffers are modified.")))
          (let* ((preserve-modes? (nomis/revert/-prompt-for-preserve-modes)))
            (when (nomis/y-or-n-p-reporting-non-local-exit
                   (format
                    "%s\nAre you sure you want to revert the above %s buffer(s), %s modes?"
                    (nomis/buffers->string-of-names buffers-to-revert)
                    (length buffers-to-revert)
                    (if preserve-modes? "preserving" "reverting")))
              (nomis/-revert-buffers* buffers-to-revert
                                      preserve-modes?))))))))

(defun nomis/revert-buffers ()
  "Revert buffers."
  (interactive)
  (let* ((mode (nomis/revert/-prompt-for-mode))
         (only-current-repo? (nomis/revert/-prompt-for-restrict-to-current-repo)))
    (cl-multiple-value-bind (revert-unmodified-buffers?
                             revert-modified-buffers?
                             out-of-sync-buffers-only?
                             tell-user-about-modified-non-reverting-buffers?)
        (ecase mode
          (:unmodified-only-if-out-of-sync  '(t   nil t   nil))
          (:unmodified-buffers-only         '(t   nil nil t))
          (:modified-buffers-only           '(nil t   nil nil))
          (:modified-and-unmodified-buffers '(t   t   nil nil)))
      (nomis/revert-buffers* (format "Options: %s / %s"
                                     (nomis/revert/-mode->text mode)
                                     (if only-current-repo?
                                         "Only current repo"
                                       "All repos"))
                             only-current-repo?
                             revert-unmodified-buffers?
                             revert-modified-buffers?
                             out-of-sync-buffers-only?
                             tell-user-about-modified-non-reverting-buffers?))))

;;;; ___________________________________________________________________________

(provide 'nomis-revert)
