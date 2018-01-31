;;;; Init stuff -- Files.

(require 'cl)

;;;; ___________________________________________________________________________

(defun nomis/find-window-in-frame (buffer-name)
  (let* ((frame (selected-frame))
         (windows (window-list frame)))
    (cl-find-if (lambda (w)
                  (equal (-> w window-buffer buffer-name)
                         buffer-name))
                windows)))

;;;; ___________________________________________________________________________

(provide 'nomis-buffers-windows-frames)
