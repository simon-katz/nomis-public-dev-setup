;;;; Init stuff -- auto-complete

;;;; ___________________________________________________________________________

;;;; See https://cider.readthedocs.io/en/latest/code_completion/

(global-company-mode)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(case 1
  ;; Only one of these makes sense. Which do you prefer?
  (1 (setq company-idle-delay nil))
  (2 (setq company-minimum-prefix-length 2)))

;;;; ___________________________________________________________________________

(provide 'nomis-auto-complete)
