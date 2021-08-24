;;;; Init stuff -- Screens.

;;;; ___________________________________________________________________________

(when (eq window-system 'ns)
  ;; Fix broken thing
  (setq window-mgr-title-bar-pixel-height 22))

;;;; ___________________________________________________________________________

(provide 'nomis-screens)
