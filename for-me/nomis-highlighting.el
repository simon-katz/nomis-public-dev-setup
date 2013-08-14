;;;; Init stuff -- Line highlighting.

;;;; ___________________________________________________________________________

;; Defaults:
;; (set-face-background 'mode-line          "grey75")
;; (set-face-background 'mode-line-inactive "grey90")
;; (set-face-foreground 'mode-line          "black")
;; (set-face-foreground 'mode-line-inactive "grey20")

;; Tailoring
(progn
  (set-face-background 'mode-line "#ccccff")
  (set-face-background 'mode-line-inactive "grey75"))

;;;; ___________________________________________________________________________

;; Use M-x list-colors-display to see colors.

(global-hl-line-mode 1)
;; (set-face-background 'hl-line "lightcyan")
;; (set-face-background 'hl-line "lightcyan1")
;; (set-face-background 'hl-line "azure2")
;; (set-face-background 'hl-line "slategray1")
;; (set-face-background 'hl-line "palegreen")
;; (set-face-background 'hl-line "palegreen1")
(set-face-background 'hl-line "darkseagreen1")
;; (set-face-background 'hl-line "grey97")
;; (set-face-background 'hl-line "white")
;; (set-face-background 'hl-line "RGB:9999/9999/9999")
;; (set-face-background 'hl-line "lightyellow")
;; (set-face-background 'hl-line "LightGoldenrodYellow")
;; (remove-hook 'coding-hook 'turn-on-hl-line-mode)

;;;; ___________________________________________________________________________

(provide 'nomis-highlighting)
