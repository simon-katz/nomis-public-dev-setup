;;;; Init stuff -- magit

(add-hook 'magit-mode-hook (lambda () (company-mode 0)))

(setq magit-status-buffer-switch-function 'switch-to-buffer)

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
