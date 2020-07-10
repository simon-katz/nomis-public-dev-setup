;;;; Init stuff -- Python

;;;; ___________________________________________________________________________

(elpy-enable)

;;;; ___________________________________________________________________________
;;;; Flycheck

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-python)
