;;;; Init stuff -- Mouse scrolling.

(setq mouse-wheel-scroll-amount '(2
                                  ((shift) .      4)
                                  ;; The following don't seem to work
                                  ;; ((control-shift) . 4)
                                  ;; ((meta) .       4)
                                  ;; ((meta-shift) . 8)
                                  ))

(setq mouse-wheel-progressive-speed nil) ; trackpad scrolls too fast when non-nil

;;;; ___________________________________________________________________________

(provide 'nomis-mouse-scrolling)
