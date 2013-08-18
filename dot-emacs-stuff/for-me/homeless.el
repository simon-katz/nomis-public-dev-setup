;; Init stuff -- homeless -- to maybe move to a better place

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

(provide 'homeless)
