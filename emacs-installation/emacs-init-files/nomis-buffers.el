;;;; Init stuff -- Buffers.

;;;; ___________________________________________________________________________

(defun nomis-report-point-etc ()
  (interactive)
  (message "(point) = %s of %s"
           (point)
           (point-max)))

(define-key global-map (kbd "C-c =") 'nomis-report-point-etc)

;;;; ___________________________________________________________________________

(provide 'nomis-buffers)
