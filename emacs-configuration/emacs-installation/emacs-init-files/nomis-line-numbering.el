;;;; Init stuff -- Line numbering.

(global-display-line-numbers-mode 1)

(setq-default ;; display-line-numbers 'visual
              ;; display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

(setq column-number-indicator-zero-based nil)

;;;; ___________________________________________________________________________

(provide 'nomis-line-numbering)
