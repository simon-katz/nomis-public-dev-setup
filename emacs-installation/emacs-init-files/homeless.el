;;;; Init stuff -- homeless -- to maybe move to a better place

;;;; TODO: Move homeless stuff to somewhere appropriate.

;;;; ___________________________________________________________________________
;;;; Reverting buffers.
;;;; Copy-and-hack of http://www.emacswiki.org/emacs/RevertBuffer.

(defun _revert-all-buffers (buffer-predicate)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (funcall buffer-predicate buf))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun revert-all-unmodified-buffers ()
  "Refreshes all open unmodified buffers from their files."
  (interactive)
  (_revert-all-buffers (lambda (b) (not (buffer-modified-p b)))) )

(defun revert-all-modified-buffers ()
  "Refreshes all open modified buffers from their files."
  ;; Copied from http://www.emacswiki.org/emacs/RevertBuffer, and renamed.
  (interactive)
  (when (y-or-n-p "Really revert modified buffers? You will lose stuff.")
    (_revert-all-buffers 'buffer-modified-p)) )

;;;; ___________________________________________________________________________

(defun nomis-untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(global-set-key [f11] 'nomis-untabify-buffer)

(defun nomis-indent-buffer ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(global-set-key [f12] 'nomis-indent-buffer)

;;;; ___________________________________________________________________________

(defun timestamp (p)
  (interactive "P")
  (insert (if p
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%Y-%m-%d"))))

;;;; ___________________________________________________________________________

(provide 'homeless)
