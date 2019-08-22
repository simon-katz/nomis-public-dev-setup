;;;; Init stuff -- Buffers.

;;;; ___________________________________________________________________________

(defun nomis/point-etc-string ()
  (format "(point) = %s of %s"
          (point)
          (point-max)))

(defun nomis/report-point-etc ()
  (interactive)
  (message (nomis/point-etc-string)))

(define-key global-map (kbd "C-c =") 'nomis/report-point-etc)

;;;; ___________________________________________________________________________

(provide 'nomis-buffers)
