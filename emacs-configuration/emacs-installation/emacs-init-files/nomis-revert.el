;;;; Init stuff -- nomis-revert --  -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'nomis-msg)

;;;; ___________________________________________________________________________
;;;; ---- Various utilities ----

(defun nomis/vc-buffer-in-current-repo? (b)
  (case 2
    (1 (magit-auto-revert-repository-buffer-p b))
    (2 (s-starts-with? (nomis/dirtree/vc-root-dir)
                       (buffer-file-name b)))))

(defun nomis/-vc-make/buffer-in-current-repo?-fun ()
  (let* ((this-buffer (current-buffer)))
    (lambda (b)
      (with-current-buffer this-buffer
        (nomis/vc-buffer-in-current-repo? b)))))

(defun nomis/-vc-buffer-unmodified? (in-current-repo?-fun b)
  (and (buffer-file-name b)
       (not (buffer-modified-p b))
       (funcall in-current-repo?-fun b)))

(defun nomis/-vc-buffer-unmodified-and-out-of-date? (in-current-repo?-fun b)
  (and (nomis/-vc-buffer-unmodified? in-current-repo?-fun b)
       (not (verify-visited-file-modtime b))))

(defun nomis/find-buffers (&rest buffer-predicates)
  (let* ((buffers '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (-every? (lambda (pred) (funcall pred buf))
                       buffer-predicates)
          (push buf buffers))))
    buffers))

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
  (s-join " "
          (-map #'buffer-file-name buffers)))

;;;; ___________________________________________________________________________
;;;; ---- Reverting buffers ----

(defun nomis/revert-buffer/simple ()
  (revert-buffer t t t))

(defun nomis/revert-buffer-reporting-failures (&optional b)
  (let* ((b (or b (current-buffer))))
    (with-current-buffer b
      (condition-case e
          (nomis/revert-buffer/simple)
        (error
         (nomis/message-no-disp "==== Failed to revert %s" b))))))

(defun nomis/revert-all-buffers (buffer-predicate inhibit-message?)
  "Revert all buffers that satisfy `buffer-predicate`.
For each buffer, the predicate is run with the buffer as the
current buffer. Unless `inhibit-message?` is non-nil, emit
a message when finished. The message includes the names of any
buffers that could not be reverted."
  ;; Copy-and-hack of http://www.emacswiki.org/emacs/RevertBuffer
  (let ((failures '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (funcall buffer-predicate buf))
          (condition-case e
              (nomis/revert-buffer/simple)
            (error
             (push buf failures)
             (nomis/message-no-disp "%s" e)
             (beep))))))
    (let* ((inhibit-message inhibit-message?))
      (message "Refreshed open files. %s"
               (if failures
                   (s-join " " (cons "Failures: "
                                     (-map #'buffer-file-name failures)))
                 "")))))

(defun nomis/revert-all-unmodified-buffers (inhibit-message?)
  "Refreshes all open unmodified buffers from their files."
  (interactive "P")
  (nomis/revert-all-buffers (lambda (b) (not (buffer-modified-p b)))
                            inhibit-message?))

(defun nomis/revert-all-modified-buffers (inhibit-message?)
  "Refreshes all open modified buffers from their files."
  ;; Copied from http://www.emacswiki.org/emacs/RevertBuffer, and renamed.
  (interactive "P")
  (when (nomis/y-or-n-p-reporting-non-local-exit
         "Really revert modified buffers? You will lose stuff.")
    (nomis/revert-all-buffers 'buffer-modified-p
                              inhibit-message?)))

;;;; ___________________________________________________________________________

(defun nomis/revert-out-of-sync-buffers-in-repo
    (&optional force?)
  (interactive "P")
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (revert-buffer?-fun (lambda (b)
                               (and (buffer-file-name b)
                                    (not (buffer-modified-p b))
                                    (funcall in-current-repo?-fun b)
                                    (not (verify-visited-file-modtime b)))))
         (buffers-to-maybe-revert (nomis/find-buffers revert-buffer?-fun)))
    (if (null buffers-to-maybe-revert)
        (message "No buffers are out-of-sync")
      (let* ((revert-buffers?
              (or force?
                  (nomis/y-or-n-p-reporting-non-local-exit
                   (format
                    "Do you want to revert the following %s buffer(s) that are out-of-sync with their files? %s"
                    (length buffers-to-maybe-revert)
                    buffers-to-maybe-revert)))))
        (when revert-buffers?
          (dolist (b buffers-to-maybe-revert)
            (nomis/message-no-disp "==== ==== Reverting %s" b)
            (nomis/revert-buffer-reporting-failures b)))))))

;;;; ___________________________________________________________________________

(provide 'nomis-revert)
