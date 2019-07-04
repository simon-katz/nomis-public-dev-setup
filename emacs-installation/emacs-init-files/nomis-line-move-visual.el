;;;; Init stuff -- visual-line-mode

;;;; ___________________________________________________________________________

(setq line-move-visual nil) ; the default of T is annoying, and it
                            ; screws up keyboard macros

(unless (equal emacs-version "25.2") ; not running old Emacs on Android
  (define-fringe-bitmap 'nomis/right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b01110000
     #b00010000
     #b00010000
     #b00000000])
  (define-fringe-bitmap 'nomis/left-curly-arrow
    [#b00000000
     #b00001000
     #b00001000
     #b00001110
     #b00000000
     #b00000000
     #b00000000
     #b00000000])
  (setq visual-line-fringe-indicators '(nomis/left-curly-arrow
                                        nomis/right-curly-arrow)))

;;;; ___________________________________________________________________________

(provide 'nomis-line-move-visual)
