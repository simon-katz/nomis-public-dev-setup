;;;; Init stuff -- Mouse scrolling.

;;;; ___________________________________________________________________________

(setq mouse-wheel-scroll-amount '(2
                                  ((shift) .      4)
                                  ;; The following don't seem to work
                                  ;; ((control-shift) . 4)
                                  ;; ((meta) .       4)
                                  ;; ((meta-shift) . 8)
                                  ))

(setq mouse-wheel-progressive-speed nil) ; trackpad scrolls too fast when non-nil

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;;;; ___________________________________________________________________________

;; This is actually mouse zooming, not scrolling, but WTF...
;; Disable `mouse-wheel-text-scale` commands:

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-M-<wheel-up>"))
(global-unset-key (kbd "C-M-<wheel-down>"))

;;;; ___________________________________________________________________________

(provide 'nomis-mouse-scrolling)
