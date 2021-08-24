;;;; Init stuff -- diff-mode.

;;;; ___________________________________________________________________________

(defvar nomis/doing-yv-dev-env-var-name "DOING_YV_DEV")

(defun nomis/toggle-doing-yv-dev ()
  (interactive)
  (let ((v (setenv nomis/doing-yv-dev-env-var-name
                   (if (getenv nomis/doing-yv-dev-env-var-name)
                       nil
                     "YES"))))
    (message "YV stuff turned %s"  (if v "on" "off"))))

;;;; ___________________________________________________________________________

(provide 'nomis-yv-stuff)
