;;; nomis-save-and-read-data.el --- Save and read data -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(cl-defun nomis/save-to-file (filename data &key pretty?)
  (make-directory (file-name-directory filename)
                  t)
  (with-temp-file filename
    (if pretty?
        (insert (with-output-to-string (pp data)))
      (prin1 data (current-buffer)))))

(defun nomis/read-from-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;;;; ___________________________________________________________________________

(provide 'nomis-save-and-read-data)
