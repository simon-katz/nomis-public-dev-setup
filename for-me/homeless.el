;; Init stuff -- homeless -- to maybe move to a better place

;;;; ___________________________________________________________________________

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  ;; Copied from http://www.emacswiki.org/emacs/RevertBuffer.
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;;;; ___________________________________________________________________________

(provide 'homeless)
